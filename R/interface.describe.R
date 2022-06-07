



#' @export
describe <- function(x, ...,
                     xname=  feR:::.var.name(deparse(substitute(x))),
                     y = NULL,
                     yname =  feR:::.var.name(deparse(substitute(y))),
                     decimals = 4,
                     guess.factor = TRUE,
                     max.factor.cat = 10,
                     na.rm = TRUE,
                     ci = 0.95,

                     #----------------------- factors
                     total.by.row = TRUE,
                     total.by.column = FALSE,


                     show.general = TRUE,

                     #----------------------- comparisons


                     #----------------------- coding
                     DEBUG = FALSE) {

  args <- formals(describe)
  args$... <- NULL


  passed.args <- as.list(match.call()[-1])
  final.args <- as.list(modifyList(args, passed.args))

  if (!is.null(y)) final.args$y <- y
  if (guess.factor) final.args$x <- do.call(feR:::.guess.factor, final.args)
  do.call(feR:::.describe, final.args)
}


.describe <- function(x, ..., DEBUG = FALSE) {
  UseMethod(".describe", x)
}


.describe.data.frame <- function(x, ...) {

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




.describe.numeric <- function(x,..., y = NULL, decimals = 4,
                              show.general = TRUE,
                              DEBUG=FALSE) {


  if(DEBUG) cat("\n[.describe.numeric] Called\n")
  args <- list(...)
  args$x <- x
  args$DEBUG <- DEBUG


  if(is.null(y)) {
    if(DEBUG) cat("\n[.describe.numeric] No y\n")
    result <- do.call(feR:::.describe.feR_math.numeric,args)
  }
  else {
    result.temp <- tapply(x, y, function(xValue){
      args$x = xValue
      args$y = y
      do.call(feR:::.describe.feR_math.numeric,args)
      })
    for(r in names(result.temp)){
      r.temp <- result.temp[[r]]
      r.g <- data.frame(group=r)
      r.g <- cbind(r.g,r.temp)

      if(!exists("result")) {
        result = r.g
        nor.test <- list(attr(r.temp, "nor.test"))
        p.norm <- list(attr(r.temp, "p.norm"))
        names(nor.test)[length(nor.test)] <- r
        names(p.norm)[length(p.norm)] <- r
        }
      else {
        result <- rbind(result, r.g)
        nor.test <- append(nor.test, attr(r.temp, "nor.test"))
        p.norm <- append(p.norm, attr(r.temp, "p.norm"))
        names(nor.test)[length(nor.test)] <- r
        names(p.norm)[length(p.norm)] <- r
        }
      
    }
    class(result) <- c("feR_describe_numeric_list", class(result))
    attr(result, "var.name") <- args[["xname"]]
    attr(result, "y.name") <- args[["yname"]]
    attr(result, "nor.test") <- nor.test
    attr(result, "p.norm") <- p.norm
  }
  attr(result,"decimals") <- decimals
  return(result)
}



#' @export
.describe.factor <- function(x,..., y = NULL,
                             # decimals = 4,
                             # total.by.row = TRUE,
                             # total.by.column = FALSE,
                             DEBUG = FALSE) {
  if(DEBUG) cat("\n[.describe.factor] Called\n")

  args <- list(...)
  args$x <- x
  args$DEBUG <- DEBUG
  if(!is.null(y)) args$y <- y
  result <- do.call(feR:::.describe.feR_math.factor, args)


  return(result)

}



#' @export
print.feR_describe_numeric <- function(obj, raw=FALSE) {
  if(raw) {
    print("RAW")
    print(knitr::kable(obj))
    return()
  }
  decimals <- attr(obj,"decimals")

  for(v in names(obj)){
    value <- obj[1,v]
    if(is.numeric(value)) value <- round(value, digits = decimals)
    value.char <- as.character(value)

    if(exists("stats")) stats <- c(stats,v)
    else stats <- v

    if(exists("values")) values <- c(values,value)
    else values <- value
  }

  x.final <- data.frame(stats=stats, value=values)

  print(knitr::kable(x.final, caption = attr(obj,"var.name")))
  cat("\nNormality test:",attr(obj,"nor.test"),"; p.value:",attr(obj,"p.norm"),"\n")
}


#' @export
print.feR_describe_numeric_list <- function(obj) {

  nor.text = ""
  decimals = attr(obj,"decimals")
  rownames(obj) <- obj$group
  obj$group <- NULL
  result <- t(obj)

  # for(i in 1:length(obj)){
  #
  #   db <- obj[[i]]
  #   new.decimals = attr(db,"decimals")
  #   if(decimals < new.decimals) decimals <- new.decimals
  #   # print(db)
  #   cat.name <- names(obj)[i]
  #
  #   nor.text.temp <- paste0(attr(obj,'y.name'),"[",cat.name,"] -> Normality test:",attr(db,"nor.test"),"; p.value:",attr(db,"p.norm"),"\n")
  #   nor.text <- paste0(nor.text,nor.text.temp)
  #
  #
  #   # if(i==1) result <- as.data.frame(db)
  #   if(i==1) {
  #     result <- db
  #     result$group <- cat.name
  #     result <- result[,c(ncol(result),1:(ncol(result)-1))]
  #   }
  #   else {
  #     db$group <- cat.name
  #     result <- rbind(result,db)
  #   }
  #
  #   # names(result)[ncol(result)] <- cat.name
  # }

  # for(v in names(result)){
  #   value <- result[1,v]
  #   if(is.numeric(value)) value <- round(value, digits = decimals)
  #   value.char <- as.character(value)
  #
  #   if(exists("stats")) stats <- c(stats,v)
  #   else stats <- v
  #
  #   if(exists("values")) values <- c(values,value)
  #   else values <- value
  # }

  # x.final <- data.frame(stats=stats, value=values)



  print(knitr::kable(result, caption = paste(attr(obj,"var.name"),"vs",attr(obj,"y.name"))))
  for (g in names(attr(obj,"nor.test"))){
    cat("\nNormality test ",g,":",attr(obj,"nor.test")[[g]],"; p.value:",attr(obj,"p.norm")[[g]],"\n")
  }
  


}


#' @export
print.feR_describe_factor <- function(obj) {


  if(!is.null(attr(obj,"y.name"))) print(knitr::kable(obj, caption = paste(attr(obj,"var.name"),"vs",attr(obj,"y.name"))))
  else print(knitr::kable(obj, caption = attr(obj,"var.name")))
}


#' @export
print.feR_describe_data_frame <- function(obj) {
  for(x in obj){
    print(x)
  }

}
