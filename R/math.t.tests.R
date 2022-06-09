
#' welch_test
#'
#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param x DESCRIPTION.
#' @param x.name DESCRIPTION.
#' @param y DESCRIPTION.
#' @param y.name DESCRIPTION.
#' @param ci DESCRIPTION.
#' @param alternative DESCRIPTION.
#' @param stop.on.error DESCRIPTION.
#' @param lang DESCRIPTION.
#' @param decimals DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
#' @export
welch_test <- function(x, x.name=feR:::.var.name(deparse(substitute(x))),
                         y=NULL, y.name = feR:::.var.name(deparse(substitute(y))),
                         p.sig=0.05, alternative="two.sided",
                         stop.on.error = TRUE, lang = "es", decimals = 4) {
  ci <- 1 - p.sig
  tryCatch(feR:::.check.comp_means.parameters(x = x, y = y, ci = ci,
              alternative = alternative, lang = lang, method = "auto"),
           error = function(e) {
              if (stop.on.error) stop(e)
              else return(NA)
           }
           )

  test <- tryCatch(t.test(x ~ y, conf.level = ci, var.equal = FALSE, alternative = alternative),
                    error = function(e) {
                      if(stop.on.error) stop(e)
                      else return(NA)
                    })

  if (length(test) == 1) if (is.na(test)) return(NA)
  x.test <- feR:::.t.test.results(test, x.name = x.name, y.name = y.name,
                                  y.levels = levels(as.factor(y)),
                                  decimals = decimals, test.name = "Welch t-test",
                                  alternative = alternative)
  return(x.test)
}



#' t_test
#'
#' @export
t_test <- function(x, x.name=feR:::.var.name(deparse(substitute(x))),
                           y=NULL, y.name = feR:::.var.name(deparse(substitute(y)))
                           ,p.sig=0.05, alternative="two.sided",
                           stop.on.error = FALSE, lang = "es", decimals = 4) {
  ci <- 1 - p.sig
  tryCatch(feR:::.check.comp_means.parameters(x = x, y = y, ci = ci, alternative = alternative,
                                              lang = lang, method = "auto"),
          error = function(e) {
              if (stop.on.error) stop(e)
              else return(NA)
          })

  test <- tryCatch(t.test(x ~ y, conf.level = ci, var.equal = TRUE, alternative = alternative),
                    error = function(e) {
                      if(stop.on.error) stop(e)
                      else return(NA)
                    })

  if (length(test) == 1) if (is.na(test)) return(NA)
  x.test <- feR:::.t.test.results(test, x.name = x.name, y.name=y.name,
                                  y.levels = levels(as.factor(y)),
                                  decimals = decimals, alternative = alternative,
                                  test.name = "Student t-test")

  return(x.test)
}

#' .t.test.results
#'
#'
.t.test.results <- function(test, x.name=NULL, y.name=NULL, y.levels=NULL,
                            test.name=NULL, alternative=NULL, decimals=4) {

  x.test <- data.frame(var.name = x.name)
  x.test$g1 <- y.levels[1]
  x.test$g2 <- y.levels[2]
  x.test$test.name <- test.name

  x.test$df <- test$parameter
  x.test$stat.name <- "t"
  x.test$stat.value <- test$statistic
  x.test$stat.ci.low <- test$conf.int[[1]]
  x.test$stat.ci.high <- test$conf.int[[2]]
  x.test$alternative <- alternative
  x.test$p.value <- test$p.value

  x.test$mean.diff <- test$estimate[[1]] - test$estimate[[2]]

  x.test$mean.diff.ci.low <- test$conf.int[[1]]
  x.test$mean.diff.ci.high <- test$conf.int[[2]]

  class(x.test) <- c("feR.comp_means", "data.frame")
  attr(x.test, "DECIMALS") <- decimals
  return(x.test)
}
