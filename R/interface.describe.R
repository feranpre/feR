



#' @export
describe <- function(x, ...,
                     xname=  feR:::.var.name(deparse(substitute(x))),
                     by = NULL,
                     byname =  feR:::.var.name(deparse(substitute(by))),
                     decimals = 4,
                     guess.factor = TRUE,
                     max.factor.cat = 10,
                     na.rm = TRUE,
                     ci = 0.95,

                     #----------------------- factors
                     total.by.row = TRUE,
                     total.by.column = FALSE,

                     #----------------------- comparisons


                     #----------------------- coding
                     DEBUG = FALSE) {

  args <- formals(describe)
  args$... <- NULL


  passed.args <- as.list(match.call()[-1])
  final.args <- as.list(modifyList(args, passed.args))

  if(!is.null(by)) final.args$by <- by
  if(guess.factor) final.args$x <- do.call(feR:::.guess.factor,final.args)
  do.call(feR:::.describe,final.args)
}


.describe <- function(x,..., DEBUG=FALSE){
  UseMethod(".describe",x)
}


.describe.data.frame <- function(x,...) {

  args=list(...)
  results <- list()

  for(var.name in names(x)){
    args$x <- x[,var.name]
    args$xname <- var.name
    var <- do.call(feR:::.describe,args)
    results[[var.name]] <- var

  }
  class(results) <- c("feR_describe_data_frame",class(results))
  return(results)
}




.describe.numeric <- function(x,..., by = NULL, DEBUG=FALSE) {


  if(DEBUG) cat("\n[.describe.numeric] Called\n")
  args <- list(...)
  args$x <- x
  args$DEBUG <- DEBUG


  if(is.null(by)) {
    if(DEBUG) cat("\n[.describe.numeric] No by\n")
    result <- do.call(feR:::.describe.feR_math.numeric,args)
  }
  else {
    result <- tapply(x,by,function(x.value){
      args$x = x.value
      args$by = by
      do.call(feR:::.describe.feR_math.numeric,args)
      })
    class(result) <- c("feR_describe_numeric_list",class(result))
    attr(result,"var.name") <- args[["xname"]]
    attr(result,"by.name") <- args[["byname"]]
  }
  return(result)
}



#' @export
.describe.factor <- function(x,..., by = NULL,
                             # decimals = 4,
                             # total.by.row = TRUE,
                             # total.by.column = FALSE,
                             DEBUG = FALSE) {
  if(DEBUG) cat("\n[.describe.factor] Called\n")

  args <- list(...)
  args$x <- x
  args$DEBUG <- DEBUG
  if(!is.null(by)) args$by <- by
  result <- do.call(feR:::.describe.feR_math.factor,args)


  return(result)

}



#' @export
print.feR_describe_numeric <- function(obj) {
  print(knitr::kable(obj, caption = attr(obj,"var.name")))
  cat("\nNormality test:",attr(obj,"nor.test"),"; p.value:",attr(obj,"p.norm"),"\n")
}


#' @export
print.feR_describe_numeric_list <- function(obj) {

  nor.text = ""
  for(i in 1:length(obj)){

    db <- obj[[i]]
    # print(db)
    cat.name <- names(obj)[i]

    nor.text.temp <- paste0(attr(obj,'by.name'),"[",cat.name,"] -> Normality test:",attr(db,"nor.test"),"; p.value:",attr(db,"p.norm"),"\n")
    nor.text <- paste0(nor.text,nor.text.temp)

    if(i==1) result <- as.data.frame(db)
    else {
      result <- cbind(result,as.data.frame(db)$stat.value)
    }

    names(result)[ncol(result)] <- cat.name
  }

  print(knitr::kable(result, caption = paste(attr(obj,"var.name"),"vs",attr(obj,"by.name"))))
  cat(nor.text)

}


#' @export
print.feR_describe_factor <- function(obj) {
  if(!is.null(attr(obj,"by.name"))) print(knitr::kable(obj, caption = paste(attr(obj,"var.name"),"vs",attr(obj,"by.name"))))
  else print(knitr::kable(obj, caption = attr(obj,"var.name")))
}


#' @export
print.feR_describe_data_frame <- function(obj) {
  for(x in obj){
    print(x)
  }

}
