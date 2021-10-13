#' comp.means
#'
#' @export
#'
comp.means <- function(x,y=NULL, ..., xname=  feR:::.var.name(deparse(substitute(x))),
                       by=NULL, byname = feR:::.var.name(deparse(substitute(by))),
                       DEBUG=FALSE,
                       paired = FALSE,
                       show.descriptives=TRUE,
                       decimals=2){

  if(missing(x)) {
    feR:::.error.msg(er="MEAN_COMP_X_MISSING")
    stop()
  }

  if(paired){
    if (!is.data.frame(x) & missing(y)) {
      feR:::.error.msg(er="MEAN_COMP_Y_MISSING")
      stop()
    }
    if (is.data.fram(x) & missing(y)) {
      if(ncol(x) <2 | ncol(x)>2) {
        feR:::.error.msg(er="MEAN_COMP_PAIRED_MUST_BE_2")
        stop()
      }
    }
  }
  else if (missing(by)) {
      feR:::.error.msg(er="MEAN_COMP_BY_MISSING")
      stop()
  }



  UseMethod("comp.means")

}


#' comp.means.numeric
#'
#' @export
comp.means.numeric <- function(x,y=NULL, ..., xname=feR:::.var.name(deparse(substitute(x))),
                       by=NULL, byname = feR:::.var.name(deparse(substitute(by))),
                       DEBUG=FALSE,
                       paired = FALSE,
                       show.descriptives = TRUE,
                       p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001, ci = 0.95,
                       decimals=2){
  # print(xname)
  # print(byname)
  if(DEBUG) cat("\n[comp.means.numeric] START...\n")

    if(!missing(by)) {
      if(is.data.frame(by)) {

      }
      else {
        if(!is.factor(by)) {
          by.value = as.factor(by)
        }
        else by.value = by
        result <- feR:::.comp.means(x=x, xname=xname, by = by.value, byname = byname, DEBUG = DEBUG,
                                    p.sig = p.sig, p.sig.small = p.sig.small,
                                    p.sig.very.small = p.sig.very.small, ci = ci,
                                    decimals=decimals)
      }

    } else {
      feR:::.error.msg(er="MEAN_COMP_BY_MISSING")
      stop()
    }
  if(exists("result")) {
    attr(result,"SHOW.DESCRIPTIVES") <- show.descriptives
    return(result)
  }
  else return(NA)
}



#' comp.means.data.frame
#'
#' @export
comp.means.data.frame <- function(x,y=NULL, ..., xname=feR:::.var.name(deparse(substitute(x))),
                               by=NULL, byname = feR:::.var.name(deparse(substitute(by))),
                               DEBUG=FALSE,
                               paired = FALSE, p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001, ci = 0.95,
                               decimals=2){

  if(DEBUG) cat("\n[comp.means.data.frame] START...\n")
  if(!paired) {
    if(!is.data.frame(by)) {


    }
    for(var in names(x)) {
      if(DEBUG) cat("\n       var: ",var, sep = "")
      var.value <- dplyr::pull(x,var)
      res <- feR::comp.means(var.value, xname=xname, by = by, byname = byname, DEBUG = DEBUG)
    }
  }
}

#' comp.means.numeric
#'
.comp.means <- function(x,y=NULL, ..., xname=feR:::.var.name(deparse(substitute(x))),
                               by=NULL, byname = feR:::.var.name(deparse(substitute(by))),
                               DEBUG=FALSE,
                               paired = FALSE, p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001, ci = 0.95,
                               decimals=2){
  x.mean <- feR::means(x=x, xname=xname, by=by, byname = byname, decimals = decimals)



  normal <- all(x.mean$is.normal)
  if(normal) {

    var.test.p <- car::leveneTest(x, group = by)$`Pr(>F)`[1]
    homocedasticity.p <- ifelse(var.test.p < p.sig.very.small, paste0("<",p.sig.very.small), round(var.test.p, digits = decimals + 1))
    homocedasticity <- ifelse(var.test.p < p.sig, FALSE, TRUE)

    test <- t.test(x ~ by, conf.level = ci, var.equal = homocedasticity)
    x.test <- x.mean[,c("var.name","group.var","group","n.valid","n.missing")]
    x.test$comp.test <- "Welch t-test"
    x.test$df <- test$parameter
    x.test$stat.name <- "t"
    x.test$stat.value <- round(test$statistic, digits = decimals)
    x.test$stat.ci.low <- round(test$conf.int[[1]], digits = decimals)
    x.test$stat.ci.high <- round(test$conf.int[[2]], digits = decimals)
    x.test$p.value.exact <- round(test$p.value, digits = decimals+1)
    x.test$p.value <- ifelse(test$p.value < p.sig.very.small, paste0("<",p.sig.very.small), round(test$p.value, digits = decimals + 1))

    x.test$p.symbols[test$p.value >= p.sig] <- "-"
    x.test$p.symbols[test$p.value < p.sig] <- "*"
    x.test$p.symbols[test$p.value < p.sig.small] <- "**"
    x.test$p.symbols[test$p.value < p.sig.very.small] <- "***"

    x.test$estimate <- round(test$estimate, digits = decimals)
    x.test$estimate



    class(x.test) <- append("feR.comp.mean", "data.frame")
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

    if(attr(x,"SHOW.DESCRIPTIVES")) {
      x.mean <- attr(x,"DESC")
      print(x.mean)
    }

  x.comp <- attr(x,"COMP")
  print(x.comp)

}


#' print.feR.comp.mean
#'
#'  Esta función deberá controlar si se imprimen los descriptivos y formato y esas cosas
#'
#' @export
print.feR.comp.mean <- function(x) {

  x <- feR:::.clean.table.var.names(x)
  print(knitr::kable(x))

}





