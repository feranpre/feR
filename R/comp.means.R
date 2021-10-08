#' comp.means
#'
#' @export
#'
comp.means <- function(x,y=NULL, ..., xname=  feR:::.var.name(deparse(substitute(x))),
                       by=NULL, byname = feR:::.var.name(deparse(substitute(by))),
                       paired = FALSE,
                       decimals=2){

  if(missing(x)) feR:::.error.msg(er="MEAN_COMP_X_MISSING")
  if(paired & missing(y)) fer:::.error.msg(er="MEAN_COMP_Y_MISSING")
  if(!paired & missing(by)) fer:::.error.msg(er="MEAN_COMP_BY_MISSING")
  UseMethod("comp.means")

}


#' comp.means.numeric
#'
#' @export
comp.means.numeric <- function(x,y=NULL, ..., xname=feR:::.var.name(deparse(substitute(x))),
                       by=NULL, byname = feR:::.var.name(deparse(substitute(by))),
                       paired = FALSE, p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001, ci = 0.95,
                       decimals=2){
  # print(xname)
  # print(byname)
  x.mean <- feR::means(x=x, xname=xname, by=by, byname = byname, decimals = decimals)



  normal <- all(x.mean$is.normal)
  if(normal) {
    test <- t.test(x ~ by, conf.level = ci)
    x.test <- x.mean[,c("var.name","group.var","group","n.valid","n.missing")]
    x.test$comp.test <- "Welch t-test"
    x.test$stat.name <- "t"
    x.test$stat.value <- test$statistic
    x.test$p.value <- test$p.value
    x.test$estimate <- test$estimate
    x.test$p.symbols[x.test$p.value >= p.sig] <- "-"
    x.test$p.symbols[x.test$p.value < p.sig] <- "*"
    x.test$p.symbols[x.test$p.value < p.sig.small] <- "**"
    x.test$p.symbols[x.test$p.value < p.sig.very.small] <- "***"
    x.test$ci.low <- test$conf.int[[1]]
    x.test$ci.high <- test$conf.int[[2]]

    class(x.test) <- append("feR.comp.mean", class(x.test))

  }


  result <- cbind(x.mean, x.test[,-c(1:5)])
  class(result) <- append("feR.comp.mean.total", class(result))
  attr(result,"DESC") <- x.mean
  attr(result,"COMP") <- x.test

  return(result)
}


#' print.feR.comp.mean.total
#'
#'  Esta función deberá controlar si se imprimen los descriptivos y formato y esas cosas
#'
#' @export
print.feR.comp.mean.total <- function(x) {

    x.mean <- attr(x,"DESC")
    x.comp <- attr(x,"COMP")

    print(x.mean)
    print(x.comp)

}


#' print.feR.comp.mean
#'
#'  Esta función deberá controlar si se imprimen los descriptivos y formato y esas cosas
#'
#' @export
print.feR.comp.mean <- function(x) {


  feR::print.feR.means(x)

}
