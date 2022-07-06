#' @export
compare  <- function(x,y,...,
                     x.name=  feR:::.var.name(deparse(substitute(x))),
                     y.name =  feR:::.var.name(deparse(substitute(y))),
                     show.desc = T, show.var = T,
                     p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001,
                     DEBUG=FALSE) {

  function.args <- formals(compare)
  function.args$... <- NULL

  passed.args <- as.list(match.call()[-1])
  final.args <- as.list(modifyList(function.args, passed.args))


  do.call(feR:::.compare,final.args)
}


.compare <- function(x,y,...){
  UseMethod(".compare")
}

.compare.numeric <- function(x,y,...,p.sig = 0.05, DEBUG = F, show.desc = T, show.var = T){

  if(class(y) != "factor") y <- factor(y)

  args <- list(...)
  args$x <- x
  args$y <- y
  args$x.name <- args[["x.name"]]
  args$y.name <- args[["y.name"]]
  args$p.sig <- p.sig
  args$DEBUG <- DEBUG

  total.cat <- length(levels(y))
  desc <- do.call(feR::describe, args)
  is.normal = (sum(desc$p.norm.exact < p.sig)==0) #.. if any p.norm.exact is below p.sig we need non.parametric tests


  # print(desc)
  # print(is.normal)

  #--- test homocedasticity
  bart <- bartlett.test(x ~ y)
  is.var.equal <- bart$p.value < p.sig

  if(DEBUG) cat("[.compare.numeric] Normality ->",is.normal,"\n")
  if(total.cat == 2) {
    if(is.normal) {
      if(DEBUG) cat("[.compare.numeric] Homocedasticity ->",is.var.equal,"\n")
      if(is.var.equal) result <- do.call(feR:::t_student, args)
      else result <- feR:::welch_test(x, y = y)
    } else {
      result <- feR::wilcoxon_test(x, y = y)
    }
  } else {
    if(is.normal & is.var.equal){
      result <- feR:::anova_test(x ~ y)
    }else{
      result <- feR:::kruskal_test(x ~ y)
    }
  }

  if(show.desc) {
    attr(result,"SHOW.DESCRIPTIVES") <- TRUE
    attr(result, "DESCRIPTIVES") <- desc
  } else {
    attr(result,"SHOW.DESCRIPTIVES") <- FALSE
  }

  if(show.var) {
    attr(result,"SHOW.VARIANCE") <- TRUE
    attr(result, "VARIANCE") <- bart
  } else {
    attr(result,"SHOW.VARIANCE") <- FALSE
  }


  if(exists("result")) return(result)
  else return(NA)
}
