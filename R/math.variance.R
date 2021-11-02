#' TODO:
#'
#' Implementar el stop.on.error
#' Implementar lang
#'
#' Redondeo y eliminación de información inutil en print, pero no en data.frame
#'




#' variance.test
#'
#' @export
#'
variance.test <- function(x, ..., xname=  feR:::.var.name(deparse(substitute(x))),
                       by=NULL, byname = feR:::.var.name(deparse(substitute(by))),
                       p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001, ci = 0.95,
                       decimals=2,
                       DEBUG=FALSE,
                       show.descriptives=TRUE,
                       show.variance.test = TRUE,
                       show.comp.test = TRUE,
                       show.df = TRUE,
                       show.stat.name = TRUE,
                       show.stat.value = TRUE,
                       show.stat.ci = TRUE,
                       show.p.value.exact = TRUE,
                       show.p.value = TRUE,
                       show.p.symbols = TRUE,
                       show.estimate = TRUE, lang = "es", stop.on.error=TRUE, show.error = TRUE
                       ){

  if(missing(x)) {
    if(stop.on.error) stop(feR:::.error.msg(er="VARIANCE_TEST_X_MISSING", lang=lang))
    else if (show.error) message(feR:::.error.msg(er="VARIANCE_TEST_X_MISSING", lang=lang))
    return(NA)
  }

 if (missing(by)) {
   if(stop.on.error) stop(feR:::.error.msg(er="VARIANCE_TEST_BY_MISSING", lang=lang))
   else if (show.error) message(feR:::.error.msg(er="VARIANCE_TEST_BY_MISSING", lang=lang))
   return(NA)
  }

  UseMethod("variance.test")
}



#' variance.test.numeric
#'
#' @export
#'
variance.test.numeric <- function(x, xname=  feR:::.var.name(deparse(substitute(x))),
                                  by=NULL, byname = feR:::.var.name(deparse(substitute(by))),
                                  p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001, ci = 0.95,
                                  decimals = 2, DEBUG = FALSE, lang = "es", stop.on.error = TRUE) {

  by.name = byname
  if(!is.factor(by)) by <- as.factor(by)
  if(length(levels(by))<2) {
    feR:::.error.msg(er = "VARIANCE_TEST_1_GROUP", lang=lang, stop.on.error = stop.on.error)
    return(NA)
  }
  var.test <- car::leveneTest(x, group = by)

  var.test.p <- var.test$`Pr(>F)`[1]
  homocedasticity <- ifelse(var.test.p < p.sig, FALSE, TRUE)
  x.variance <- data.frame("var.name" = xname,"group.var" = by.name,
                           "n.valid" = sum(!is.na(x) & !is.na(by)),
                           "n.missing" = sum(is.na(x) | is.na(by)))
  x.variance$test.name <- "Levene"
  x.variance$df.group <- var.test$Df[[1]]
  x.variance$df.n <- var.test$Df[[2]]
  x.variance$stat.name <- "F"
  x.variance$stat.value <- var.test$`F value`[1]
  x.variance$p.value <- var.test.p
  x.variance$homocedasticity <- var.test.p < p.sig

  class(x.variance) <- append("feR.variance.test","data.frame")
  attr(x.variance, "TEST") <- "Levene"
  attr(x.variance, "DECIMALS") <- decimals
  attr(x.variance, "LANG") <- lang

  return(x.variance)
}




#'
#' print.feR.variance.test
#'
#' @export
print.feR.variance.test <- function(x){
  print(knitr::kable(x, caption = paste("Homocedasticity test of",x$var.name[1],"by",x$group.var[1])))
}
