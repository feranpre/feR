
#' welch_test
#'
#' @export
welch_test <- function(x,xname=feR:::.var.name(deparse(substitute(x))),
                         by=NULL, byname = feR:::.var.name(deparse(substitute(by))),
                         ci=0.95,alternative="two.sided",
                         stop.on.error = FALSE, lang = "es", decimals = 2){
  tryCatch(feR:::.check.comp_means.parameters(x=x,by=by,ci=ci,alternative=alternative,lang=lang, method="auto"), error = function(e) {
    if(stop.on.error) stop(e)
    else return(NA)
  })

  test <- tryCatch(t.test(x ~ by, conf.level = ci, var.equal = FALSE, alternative = alternative),
                    error = function(e) {
                      if(stop.on.error) stop(e)
                      else return(NA)
                    })

  if(length(test) == 1) if(is.na(test)) return(NA)
  x.test <- feR:::.t.test.results(test, xname=xname, byname=byname, by.levels=levels(as.factor(by)),
                                  decimals = decimals, testname="Welch t-test", alternative = alternative)
  return(x.test)
}



#' t_student
#'
#' @export
t_student <- function(x,xname=feR:::.var.name(deparse(substitute(x))),
                           by=NULL, byname = feR:::.var.name(deparse(substitute(by)))
                           ,ci=0.95, alternative="two.sided",
                           stop.on.error = FALSE, lang = "es", decimals = 2){
  tryCatch(feR:::.check.comp_means.parameters(x=x,by=by,ci=ci,alternative=alternative,lang=lang, method="auto"), error = function(e) {
    if(stop.on.error) stop(e)
    else return(NA)
  })

  test <- tryCatch(t.test(x ~ by, conf.level = ci, var.equal = TRUE, alternative = alternative),
                    error = function(e) {
                      if(stop.on.error) stop(e)
                      else return(NA)
                    })

  if(length(test) == 1) if(is.na(test)) return(NA)
  x.test <- feR:::.t.test.results(test, xname=xname, byname=byname,
                                  by.levels = levels(as.factor(by)),
                                  decimals = decimals, alternative = alternative,
                                  testname="Student t-test")

  return(x.test)
}

#' .t.test.results
#'
#'
.t.test.results <- function(test, xname=NULL, byname=NULL, by.levels=NULL, testname=NULL, alternative=NULL, decimals=2){
  x.test <- data.frame(var.name = xname)
  x.test$group.var <- byname
  x.test$g1 <- by.levels[1]
  x.test$g2 <- by.levels[2]
  x.test$test.name <- testname

    x.test$df <- test$parameter
    x.test$stat.name <- "t"
    x.test$stat.value <- test$statistic
    x.test$stat.ci.low <- test$conf.int[[1]]
    x.test$stat.ci.high <- test$conf.int[[2]]
    x.test$alternative <- alternative
    x.test$p.value <- test$p.value

    # x.test$estimate <- test$estimate
    x.test$mean.diff <- test$estimate[[1]] - test$estimate[[2]]




  x.test$mean.diff.ci.low <- test$conf.int[[1]]
  x.test$mean.diff.ci.high <- test$conf.int[[2]]

  class(x.test) <- c("feR.comp_means", "data.frame")
  attr(x.test, "DECIMALS") <- decimals
  return(x.test)
}
