

#'
#' @export
wilcoxon_test <- function(x,x.name=feR:::.var.name(deparse(substitute(x))),
                          y=NULL, y.name = feR:::.var.name(deparse(substitute(by))),
                          ci=0.95,alternative="two.sided",
                          stop.on.error = TRUE, show.error = TRUE, lang = "es", decimals = 2){


  tryCatch(feR:::.check.comp_means.parameters(x=x,y=y,ci=ci,alternative=alternative,lang=lang, method="auto"), error = function(e) {
    if(stop.on.error) stop(e)
    else if (show.error) message(e)
    return(NA)
  })

  test <- tryCatch(wilcox.test(x ~ y, conf.level = ci, conf.int = TRUE, alternative = alternative, exact = T),
                   error = function(e) {
                     if(stop.on.error) stop(e)
                     else if (show.error) message(e)
                     return(NA)
                   },
                   warning = function(w){
                     TIES = FALSE
                     if(length(w) > 1 ) {if(w[1]$message == "cannot compute exact p-value with ties") TIES = TRUE}
                     else if(w[1]$message == "cannot compute exact p-value with ties") TIES = TRUE

                     if(TIES) wilcox.test(x ~ y, conf.level = ci, conf.int = TRUE, alternative = alternative, exact = F)
                   })

  if(length(test) == 1) if(is.na(test)) return(NA)
  x.test <- feR:::.wilcoxon.results(test, x.name=x.name, y.name=y.name, y.levels=levels(as.factor(y)),
                                  decimals = decimals, testname="Wilcoxon-Mann-Whitney", alternative = alternative)
  return(x.test)
}


#' .t.test.results
#'
#'
.wilcoxon.results <- function(test, x.name=NULL, y.name=NULL, y.levels=NULL, testname=NULL, alternative=NULL, decimals=2){
  x.test <- data.frame(var.name = x.name)
  x.test$group.var <- y.name
  x.test$g1 <- y.levels[1]
  x.test$g2 <- y.levels[2]
  x.test$test.name <- testname
  x.test$df <- NA

  x.test$stat.name <- "W"
  x.test$stat.value <- test$statistic
  x.test$stat.ci.low <- NA
  x.test$stat.ci.high <- NA
  x.test$alternative <- alternative
  x.test$p.value <- test$p.value

  x.test$mean.diff <- test$estimate
  x.test$mean.diff.ci.low <- test$conf.int[[1]]
  x.test$mean.diff.ci.high <- test$conf.int[[2]]

  class(x.test) <- c("feR.comp_means", "data.frame")
  attr(x.test, "DECIMALS") <- decimals
  return(x.test)
}
