#' @export
compare  <- function(x,y,...,
                     xname=  feR:::.var.name(deparse(substitute(x))),
                     yname =  feR:::.var.name(deparse(substitute(by))),
                     show.desc = T,
                     p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001,
                     DEBUG=TRUE) {

  function.args <- formals(compare)
  function.args$... <- NULL

  passed.args <- as.list(match.call()[-1])
  final.args <- as.list(modifyList(function.args, passed.args))


  do.call(feR:::.compare,final.args)
}


.compare <- function(x,y,...){
  UseMethod(".compare")
}

.compare.numeric <- function(x,y,...,p.sig = 0.05, DEBUG = F){

  if(class(y) != "factor") y <- factor(y)

  args <- list(...)
  args$x <- x
  args$by <- y
  args$byname <- args[["yname"]]
  args$p.sig <- p.sig
  args$DEBUG <- DEBUG

  total.cat <- length(levels(y))
  desc <- do.call(feR::describe, args)

  is.normal = TRUE
  for(v in desc) {
    if(!attr(v,"is.normal")) {
      is.normal = FALSE
      break
    }
  }

  #--- test homocedasticity
  bart <- bartlett.test(x ~ y)
  is.var.equal <- bart$p.value < p.sig

  if(DEBUG) cat("[.compare.numeric] Normality ->",is.normal,"\n")
  if(total.cat == 2) {
    if(is.normal) {
      if(DEBUG) cat("[.compare.numeric] Homocedasticity ->",is.var.equal,"\n")
      if(is.var.equal) result <- do.call(feR:::t_student, args)
      else result <- feR:::welch_test(x, by = y)
    } else {
      result <- feR::wilcoxon_test(x, by = y)
    }
  } else {
    if(is.normal & is.var.equal){
      result <- feR:::anova_test(x ~ y)
    }else{
      result <- feR:::kruskal_test(x ~ y)
    }
  }

  if(exists("result")) return(result)
  else return(NA)
}
