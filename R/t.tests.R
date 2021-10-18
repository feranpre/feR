#' =============================================
#' ORDENANDO COSAS
#' =============================================
#' - Estas funciones deberían controlar sus errores básicos
#' - Devolver un data.frame con la información
#' - Tener un print que saque la información adecuadamente
#' - Tener su set de pruebas
#' - USAR FORMULAS
#'
#' Una vez que esté esto resuelto usaré este sistema para ampliarlo a todo
#' - Primero archivo con las pruebas matemáticas y sus soluciones
#' - Despues muestra de resultados y ampliación de información (otro archivo)
#' - Por último un wrapper que permita discriminar qué función usar y agregue todo
#'     - Este wrapper será el que determine:
#'           - qué prueba usar
#'           - si se muestran todas las columnas o no
#'           - si se muestra interpretación o no
#'           - gestión más sofisticada de errores
#'
#'
#' PERO LO PRIMERO es hacer que todo esto funcione suave y pueda tener toda la estadística en orden
#'
#'
#' Wilcoxon-Mann-Whitney está hecho.
#'
#' Parece que todo funciona pero hay que probarlo mejor, terminar los tests y meter las formulas
#'
#'

#' t_test
#'
#' @export
t_test <- function(x,xname=feR:::.var.name(deparse(substitute(x))),
                   by=NULL, byname = feR:::.var.name(deparse(substitute(by))),
                   ci=0.95,alternative="two.sided",
                   stop.on.error = FALSE, lang = "es", decimals = 2,
                   method = "auto",
                   show.descriptives = TRUE,
                   show.variance = TRUE) {
  tryCatch(feR:::.check.t_test.parameters(x=x,by=by,ci=ci,alternative=alternative,lang=lang, method=method), error = function(e) {
    if(stop.on.error) stop(e)
    else return(NA)
  })

  x.means <- feR::means(x, xname = xname, by = by, byname = byname, decimals = decimals,
                        ci = ci, stop.on.error = stop.on.error, lang = lang)
  x.variance <- feR::variance.test(x, xname = xname, by = by, byname = byname, decimals = decimals,
                                   ci = ci, stop.on.error = stop.on.error, lang = lang)
  homocedasticity = x.variance$homocedasticity[1]

  if(!all(x.means$is.normal)) {
    print("NOT NORMAL")
    x.test <- feR::t_test.wilcoxon(x, xname = xname, by = by, byname = byname, decimals = decimals,
                                  ci = ci, alternative = alternative,
                                  stop.on.error = stop.on.error, lang = lang)
  }
  else {
    if(
      (tolower(method) == "auto" & !homocedasticity) |
      (tolower(method) == "welch")
      ) x.test <- feR::t_test.welch(x, xname = xname, by = by, byname = byname, decimals = decimals,
                                                       ci = ci, alternative = alternative,
                                                       stop.on.error = stop.on.error, lang = lang)

    else if (
      (tolower(method) == "auto" & homocedasticity) |
      (tolower(method) == "student")
      ) x.test <- feR::t_test.student(x, xname = xname, by = by, byname = byname, decimals = decimals,
                                       ci = ci, alternative = alternative,
                                       stop.on.error = stop.on.error, lang = lang)
    }



  if(exists("x.test")) {
    if(exists("x.variance")) attr(x.test,"VARIANCE") <- x.variance
    if(exists("x.means")) attr(x.test,"DESCRIPTIVES") <- x.means
    class(x.test) <- c("feR.t_test", "data.frame")
    attr(x.test, "DECIMALS") <- decimals
    attr(x.test, "SHOW.DESCRIPTIVES") <- show.descriptives
    attr(x.test, "SHOW.VARIANCE") <- show.variance
    return(x.test)

  }
}

#' t_test.wilcoxon
#'
#'
#' rstatix::wilcox_test(ToothGrowth ,len ~ supp)
#' coin::wilcox_test(ToothGrowth$len ~ ToothGrowth$supp, conf.int = T)
#' wilcox.test(ToothGrowth$len ~ ToothGrowth$supp, conf.int = T, exact = T)
#'
#'
# tryCatch(wilcox.test(ToothGrowth$len ~ ToothGrowth$supp, conf.level = 0.95, conf.int = TRUE, alternative = "two.sided", exact = T),
# error = function(e) {
#   message(e,"\n")
#   if(stop.on.error) stop()
#   else return(NA)
# },
# warning = function(w){
#   print(w)
# })
#'
#' @export
t_test.wilcoxon <- function(x,xname=feR:::.var.name(deparse(substitute(x))),
                         by=NULL, byname = feR:::.var.name(deparse(substitute(by))),
                         ci=0.95,alternative="two.sided",
                         stop.on.error = FALSE, lang = "es", decimals = 2){
  tryCatch(feR:::.check.t_test.parameters(x=x,by=by,ci=ci,alternative=alternative,lang=lang, method="auto"), error = function(e) {
    if(stop.on.error) stop(e)
    else return(NA)
  })

  test <- tryCatch(wilcox.test(x ~ by, conf.level = ci, conf.int = TRUE, alternative = alternative, exact = T),
                   error = function(e) {
                     message(e,"\n")
                     if(stop.on.error) stop()
                     else return(NA)
                   },
                   warning = function(w){
                     TIES = FALSE
                     if(length(w) > 1 ) {if(w[1]$message == "cannot compute exact p-value with ties") TIES = TRUE}
                     else if(w[1]$message == "cannot compute exact p-value with ties") TIES = TRUE

                     if(TIES) wilcox.test(x ~ by, conf.level = ci, conf.int = TRUE, alternative = alternative, exact = F)
                   })

  if(length(test) == 1) if(is.na(test)) return(NA)
  x.test <- feR:::.t.test.results(test, xname=xname, byname=byname, by.levels=levels(as.factor(by)),
                                  decimals = decimals, testname="Wilcoxon-Mann-Whitney")
  return(x.test)
}


#' t_test.welch
#'
#' @export
t_test.welch <- function(x,xname=feR:::.var.name(deparse(substitute(x))),
                         by=NULL, byname = feR:::.var.name(deparse(substitute(by))),
                         ci=0.95,alternative="two.sided",
                         stop.on.error = FALSE, lang = "es", decimals = 2){
  tryCatch(feR:::.check.t_test.parameters(x=x,by=by,ci=ci,alternative=alternative,lang=lang, method="auto"), error = function(e) {
    if(stop.on.error) stop(e)
    else return(NA)
  })

  test <- tryCatch(t.test(x ~ by, conf.level = ci, var.equal = FALSE, alternative = alternative),
                    error = function(e) {
                      message(e,"\n")
                      if(stop.on.error) stop()
                      else return(NA)
                    })

  if(length(test) == 1) if(is.na(test)) return(NA)
  x.test <- feR:::.t.test.results(test, xname=xname, byname=byname, by.levels=levels(as.factor(by)),
                                  decimals = decimals, testname="Welch t-test")
  return(x.test)
}



#' t_test.student
#'
#' @export
t_test.student <- function(x,xname=feR:::.var.name(deparse(substitute(x))),
                           by=NULL, byname = feR:::.var.name(deparse(substitute(by)))
                           ,ci=0.95, alternative="two.sided",
                           stop.on.error = FALSE, lang = "es", decimals = 2){
  tryCatch(feR:::.check.t_test.parameters(x=x,by=by,ci=ci,alternative=alternative,lang=lang, method="auto"), error = function(e) {
    if(stop.on.error) stop(e)
    else return(NA)
  })

  test <- tryCatch(t.test(x ~ by, conf.level = ci, var.equal = TRUE, alternative = alternative),
                    error = function(e) {
                      message(e,"\n")
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
  x.test <- data.frame(var.name = c(xname,xname))
  x.test$by.name <- byname
  x.test$by.levels <- by.levels
  x.test$test.name <- testname

  if(testname == "Student t-test" |
     testname == "Welch t-test" ) {
    x.test$df <- test$parameter
    x.test$stat.name <- "t"
    x.test$stat.value <- test$statistic
    x.test$stat.ci.low <- test$conf.int[[1]]
    x.test$stat.ci.high <- test$conf.int[[2]]
    x.test$p.value <- test$p.value

    x.test$estimate <- test$estimate
    x.test$mean.diff <- test$estimate[[1]] - test$estimate[[2]]

  }
  else if (testname == "Wilcoxon-Mann-Whitney") {
      x.test$df <- NA
      x.test$stat.name <- "W"
      x.test$stat.value <- test$statistic
      x.test$stat.ci.low <- NA
      x.test$stat.ci.high <- NA
      x.test$p.value <- test$p.value

      x.test$estimate <- NA
      x.test$mean.diff <- test$estimate
  }

  x.test$mean.diff.ci.low <- test$conf.int[[1]]
  x.test$mean.diff.ci.high <- test$conf.int[[2]]

  class(x.test) <- c("feR.t_test", "data.frame")
  attr(x.test, "DECIMALS") <- decimals
  return(x.test)
}

#' print.feR.t_test
#'
#' @export
print.feR.t_test <- function(x){
  decimals = attr(x,"DECIMALS")
  x$stat.value <- round(x$stat.value,decimals)
  x$stat.ci.low <- round(x$stat.ci.low,decimals)
  x$stat.ci.high <- round(x$stat.ci.high,decimals)
  x$p.exact <- x$p.value
  x$p.value <- round(x$p.value, decimals)
  x$estimate <- round(x$estimate, decimals)
  x$mean.diff <- round(x$mean.diff, decimals)
  x$mean.diff.ci.low <- round(x$mean.diff.ci.low, decimals)
  x$mean.diff.ci.high <- round(x$mean.diff.ci.high, decimals)

  if(!is.null(attr(x,"SHOW.DESCRIPTIVES"))) if(attr(x,"SHOW.DESCRIPTIVES")) print(attr(x,"DESCRIPTIVES"))
  if(!is.null(attr(x,"SHOW.VARIANCE"))) if(attr(x,"SHOW.VARIANCE")) print(attr(x,"VARIANCE"))
  print(knitr::kable(x))
}

.check.t_test.parameters <- function(x,by,ci=0.95,alternative="two.sided", lang = "en", method = "auto") {

  if(missing(x)) stop(feR:::.error.msg("MISSING_X", lang=lang))
  if(missing(by)) stop(feR:::.error.msg("MISSING_BY", lang=lang))
  if(!is.numeric(x)) stop(feR:::.error.msg("NON_NUM_VECTOR", lang=lang))
  if(length(x) != length(by)) stop(feR:::.error.msg("DIFF_LEN_VECTOR", lang=lang))
  if(!is.factor(by)) by <- as.factor(by)
  if(length(levels(by))!= 2) stop(feR:::.error.msg("2_GROUPS", lang=lang))
  if(sum(!is.na(x))<4) stop(feR:::.error.msg("NOT_ENOUGH_X_OBS", lang=lang))
  if(sum(!is.na(by))<4) stop(feR:::.error.msg("NOT_ENOUGH_BY_OBS", lang=lang))
  if(missing(ci)) stop(feR:::.error.msg("MISSING_CI", lang=lang))
  if(!any(alternative %in% c("two.sided", "less", "greater"))) stop(feR:::.error.msg("ALTERNATIVE_T_TEST_NOT_VALID", lang=lang))
  if(!any(method %in% c("auto", "student","welch"))) stop(feR:::.error.msg("T_TEST_NOT_VALID", lang=lang))
  feR:::.check.stat.parameters(ci=ci, lang = lang)
}


.t.test.PRUEBAS <- function() {
  data("ToothGrowth")

  #................................................................. ERRORES.. STUDENT
  testthat::test_that(
    "Checking errors in t_test.student",
    {
      testthat::expect_error(feR::t_test.student(stop.on.error = T, lang = "en"),feR:::.error.msg("MISSING_X", lang="en"))
      testthat::expect_error(feR::t_test.student(as.character(ToothGrowth$len), stop.on.error = T, lang = "en"),feR:::.error.msg("NON_NUM_VECTOR", lang="en"))
      testthat::expect_error(feR::t_test.student(ToothGrowth$len, stop.on.error = T, lang = "en"),feR:::.error.msg("DIFF_LEN_VECTOR", lang="en"))
      testthat::expect_error(feR::t_test.student(ToothGrowth$len, by = ToothGrowth$supp[1:3], stop.on.error = T, lang = "en"),feR:::.error.msg("DIFF_LEN_VECTOR", lang="en"))
      testthat::expect_error(feR::t_test.student(ToothGrowth$len, by = ToothGrowth$dose, stop.on.error = T, lang = "en"),feR:::.error.msg("2_GROUPS", lang="en"))
      testthat::expect_error(feR::t_test.student(as.numeric(c(1,2,rep(NA,2))), by= as.factor(c("a","b","a","b")), stop.on.error = T, lang = "en"), feR:::.error.msg("NOT_ENOUGH_X_OBS", lang="en"))
      testthat::expect_error(feR::t_test.student(ToothGrowth$len[1:4], by= as.factor(c("a","b",rep(NA,2))), stop.on.error = T, lang = "en"), feR:::.error.msg("NOT_ENOUGH_BY_OBS", lang="en"))
      testthat::expect_error(feR::t_test.student(ToothGrowth$len, by=ToothGrowth$supp, stop.on.error = T, ci = 2, lang="en"), feR:::.error.msg("CI_LIMITS", lang="en"))
    }
  )


  #................................................................. ERRORES.. WELCH
  testthat::test_that(
    "Checking errors in t_test.welch",
    {
      testthat::expect_error(feR::t_test.welch(stop.on.error = T, lang = "en"),feR:::.error.msg("MISSING_X", lang="en"))
      testthat::expect_error(feR::t_test.welch(as.character(ToothGrowth$len), stop.on.error = T, lang = "en"),feR:::.error.msg("NON_NUM_VECTOR", lang="en"))
      testthat::expect_error(feR::t_test.welch(ToothGrowth$len, stop.on.error = T, lang = "en"),feR:::.error.msg("DIFF_LEN_VECTOR", lang="en"))
      testthat::expect_error(feR::t_test.welch(ToothGrowth$len, by = ToothGrowth$supp[1:3], stop.on.error = T, lang = "en"),feR:::.error.msg("DIFF_LEN_VECTOR", lang="en"))
      testthat::expect_error(feR::t_test.welch(ToothGrowth$len, by = ToothGrowth$dose, stop.on.error = T, lang = "en"),feR:::.error.msg("2_GROUPS", lang="en"))
      testthat::expect_error(feR::t_test.welch(as.numeric(c(1,2,rep(NA,2))), by= as.factor(c("a","b","a","b")), stop.on.error = T, lang = "en"), feR:::.error.msg("NOT_ENOUGH_X_OBS", lang="en"))
      testthat::expect_error(feR::t_test.welch(ToothGrowth$len[1:4], by= as.factor(c("a","b",rep(NA,2))), stop.on.error = T, lang = "en"), feR:::.error.msg("NOT_ENOUGH_BY_OBS", lang="en"))
      testthat::expect_error(feR::t_test.welch(ToothGrowth$len, by=ToothGrowth$supp, stop.on.error = T, ci = 2, lang="en"), feR:::.error.msg("CI_LIMITS", lang="en"))
      testthat::expect_error(feR::t_test.welch(ToothGrowth$len, by=ToothGrowth$supp, stop.on.error = T, ci = 2, lang="en", alternative = "wrong"), feR:::.error.msg("ALTERNATIVE_T_TEST_NOT_VALID", lang="en"))
    }
  )
  #................................................................. OK
  testthat::test_that("Checking p.value in t_test.student",testthat::expect_equal(feR::t_test.student(ToothGrowth$len, by=ToothGrowth$supp, stop.on.error = T, lang="en")$p.value[1],t.test(ToothGrowth$len ~ ToothGrowth$supp, var.equal=T)$p.value)  )
  testthat::test_that("Checking p.value in t_test.student for 'less' alternative",testthat::expect_equal(feR::t_test.student(ToothGrowth$len, by=ToothGrowth$supp, stop.on.error = T, lang="en", alternative = "less")$p.value[1],t.test(ToothGrowth$len ~ ToothGrowth$supp, var.equal=T, alternative = "less")$p.value))
  testthat::test_that("Checking p.value in t_test.welch",testthat::expect_equal(feR::t_test.welch(ToothGrowth$len, by=ToothGrowth$supp, stop.on.error = T, lang="en")$p.value[1],t.test(ToothGrowth$len ~ ToothGrowth$supp, var.equal=F)$p.value))
  testthat::test_that("Checking p.value in t_test.welch for 'less' alternative",testthat::expect_equal(feR::t_test.welch(ToothGrowth$len, by=ToothGrowth$supp, stop.on.error = T, lang="en", alternative = "less")$p.value[1],t.test(ToothGrowth$len ~ ToothGrowth$supp, var.equal=F, alternative = "less")$p.value))
}
