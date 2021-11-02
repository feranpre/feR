
#' comp_means
#'
#' @export
comp_means <- function(x,xname=NULL,
                       by=NULL, byname = NULL,
                       ci=0.95,alternative="two.sided",
                       decimals = 2,
                       method = "auto", paired = FALSE,
                       p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001,
                       show.descriptives = TRUE,
                       show.variance = TRUE,
                       stop.on.error = TRUE, show.error = TRUE,
                       lang = "es",
                       DEBUG=FALSE,...) {

  if(missing(x)) {
    if(stop.on.error) stop(feR:::.error.msg(er="MISSING_X", lang=lang))
    else if (show.error) message(feR:::.error.msg(er="MISSING_X", lang=lang))
    return(NA)
  }

  if (missing(by)) {
    if(stop.on.error) stop(feR:::.error.msg(er="MISSING_BY", lang=lang))
    else if (show.error) message(feR:::.error.msg(er="MISSING_BY", lang=lang))
    return(NA)
  }
  UseMethod("comp_means")

}




#' comp_means.numeric
#'
#' @export
comp_means.numeric <- function(x,xname=feR:::.var.name(deparse(substitute(x))),
                               by=NULL, byname = feR:::.var.name(deparse(substitute(by))),
                               ci=0.95,alternative="two.sided",
                               stop.on.error = FALSE, lang = "es", decimals = 2,
                               method = "auto", paired = FALSE,
                               p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001,
                               show.descriptives = TRUE,
                               show.variance = TRUE,
                               stop.on.errors = TRUE, show.error = TRUE,
                               DEBUG=FALSE,...) {

  tryCatch(feR:::.check.comp_means.parameters(x=x,by=by,ci=ci,alternative=alternative,lang=lang, method=method),
           error = function(e) {
              if(stop.on.error) stop(e)
              else {
                if (show.errors) message(e)
                return(NA)
                }
            })

  x.means <- feR::means(x, xname = xname, by = by, byname = byname, decimals = decimals,
                        ci = ci, stop.on.error = stop.on.error, lang = lang)
  x.variance <- feR::variance.test(x, xname = xname, by = by, byname = byname, decimals = decimals,
                                   ci = ci, stop.on.error = stop.on.error, lang = lang)
  homocedasticity = x.variance$homocedasticity[1]

  if(
    (tolower(method) == "auto" & !all(x.means$is.normal)) |
    (tolower(method) == "wilcoxon")
  ) {
    x.test <- feR::wilcoxon_test(x, xname = xname, by = by, byname = byname, decimals = decimals,
                                 ci = ci, alternative = alternative,
                                 stop.on.error = stop.on.error, show.error = show.error, lang = lang)
  }
  else {
    if(
      (tolower(method) == "auto" & !homocedasticity) |
      (tolower(method) == "welch")
    ) x.test <- feR::welch_test(x, xname = xname, by = by, byname = byname, decimals = decimals,
                                ci = ci, alternative = alternative,
                                stop.on.error = stop.on.error, show.error = show.error, lang = lang)

    else if (
      (tolower(method) == "auto" & homocedasticity) |
      (tolower(method) == "student")
    ) x.test <- feR::t_student(x, xname = xname, by = by, byname = byname, decimals = decimals,
                               ci = ci, alternative = alternative,
                               stop.on.error = stop.on.error, show.error = show.error, lang = lang)
  }



  if(exists("x.test")) {
    # attr(x.test, "VARIANCE") <- list(attr(res, "VARIANCE"))
    # attr(x.test, "DESCRIPTIVES") <- list(attr(res, "DESCRIPTIVES"))

    if(exists("x.variance")) attr(x.test,"VARIANCE") <- list(x.variance)
    if(exists("x.means")) attr(x.test,"DESCRIPTIVES") <- list(x.means)
    class(x.test) <- c("feR.comp_means", "data.frame")
    attr(x.test, "DECIMALS") <- decimals
    attr(x.test, "SHOW.DESCRIPTIVES") <- show.descriptives
    attr(x.test, "SHOW.VARIANCE") <- show.variance
    return(x.test)
  }
}

#' comp_means.data.frame
#'
#' @export
comp_means.data.frame <- function(x,xname=feR:::.var.name(deparse(substitute(x))),
                               by=NULL, byname = feR:::.var.name(deparse(substitute(by))),
                               ci=0.95,alternative="two.sided",
                                lang = "es", decimals = 2,
                               method = "auto", paired = FALSE,
                               p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001,
                               show.descriptives = TRUE,
                               show.variance = TRUE,
                               DEBUG=FALSE, stop.on.error = TRUE,
                               show.error = TRUE,...) {

  if(!paired) {
    for(var in names(x)) {
      # if(DEBUG) cat("\n       var: ",var, sep = "")
      var.value <- dplyr::pull(x,var)
      if(is.numeric(var.value)) {

        #.... check by options
        #........ can be:
        #................. vector
        #................. variable names
        #................. data.frame

        by.name = byname

        if(!is.data.frame(by)) {
          if(length(by) == length(var.value)) by.value = by     #... vector
          else if (any(by %in% names(x))) by.value <- x[,by]    #... variable names
        } else by.value <- by                                   #... data.frame
        if(!identical(by.value, var.value)) {
          res <- feR::comp_means(var.value, xname=var, by = by.value, byname = by.name, DEBUG = DEBUG,
                                 p.sig = p.sig, p.sig.small = p.sig.small,
                                 p.sig.very.small = p.sig.very.small, ci = ci,
                                 stop.on.error = stop.on.error, show.error = show.error)

          if (!exists("x.test")) {
            x.test <- res
            attr(x.test, "VARIANCE") <- list(attr(res, "VARIANCE"))[[1]]
            attr(x.test, "DESCRIPTIVES") <- list(attr(res, "DESCRIPTIVES"))[[1]]

          }
          else {
            x.test <- rbind(x.test, res)
            attr(x.test,"VARIANCE")[[length(attr(x.test,"VARIANCE")) + 1]] <- attr(res,"VARIANCE")[[1]]
            attr(x.test,"DESCRIPTIVES")[[length(attr(x.test,"DESCRIPTIVES")) + 1]] <- attr(res,"DESCRIPTIVES")[[1]]
          }
        }

      }
    }
  }
  else {
    #................... PAIRED
  }


  if (exists("x.test")) {
    attr(x.test, "DECIMALS") <- decimals
    attr(x.test, "SHOW.DESCRIPTIVES") <- show.descriptives
    attr(x.test, "SHOW.VARIANCE") <- show.variance
    class(x.test) <- c("feR.comp_means", "data.frame")
    return(x.test)
  }
}

.check.comp_means.parameters <- function(x,by,ci=0.95,alternative="two.sided", lang = "en", method = "auto") {

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


.comp_means.PRUEBAS <- function() {
  data("ToothGrowth")

  #................................................................. ERRORES.. STUDENT
  testthat::test_that(
    "Checking errors in t_student",
    {
      testthat::expect_error(feR::t_student(stop.on.error = T, lang = "en"),feR:::.error.msg("MISSING_X", lang="en"))
      testthat::expect_error(feR::t_student(as.character(ToothGrowth$len), stop.on.error = T, lang = "en"),feR:::.error.msg("NON_NUM_VECTOR", lang="en"))
      testthat::expect_error(feR::t_student(ToothGrowth$len, stop.on.error = T, lang = "en"),feR:::.error.msg("DIFF_LEN_VECTOR", lang="en"))
      testthat::expect_error(feR::t_student(ToothGrowth$len, by = ToothGrowth$supp[1:3], stop.on.error = T, lang = "en"),feR:::.error.msg("DIFF_LEN_VECTOR", lang="en"))
      testthat::expect_error(feR::t_student(ToothGrowth$len, by = ToothGrowth$dose, stop.on.error = T, lang = "en"),feR:::.error.msg("2_GROUPS", lang="en"))
      testthat::expect_error(feR::t_student(as.numeric(c(1,2,rep(NA,2))), by= as.factor(c("a","b","a","b")), stop.on.error = T, lang = "en"), feR:::.error.msg("NOT_ENOUGH_X_OBS", lang="en"))
      testthat::expect_error(feR::t_student(ToothGrowth$len[1:4], by= as.factor(c("a","b",rep(NA,2))), stop.on.error = T, lang = "en"), feR:::.error.msg("NOT_ENOUGH_BY_OBS", lang="en"))
      testthat::expect_error(feR::t_student(ToothGrowth$len, by=ToothGrowth$supp, stop.on.error = T, ci = 2, lang="en"), feR:::.error.msg("CI_LIMITS", lang="en"))
      testthat::expect_error(feR::t_student(ToothGrowth$len, by=ToothGrowth$supp, stop.on.error = T, lang="en", alternative = "not-valid"), feR:::.error.msg("ALTERNATIVE_T_TEST_NOT_VALID", lang="en"))
    }
  )


  #................................................................. ERRORES.. WELCH
  testthat::test_that(
    "Checking errors in welch_test",
    {
      testthat::expect_error(feR::welch_test(stop.on.error = T, lang = "en"),feR:::.error.msg("MISSING_X", lang="en"))
      testthat::expect_error(feR::welch_test(as.character(ToothGrowth$len), stop.on.error = T, lang = "en"),feR:::.error.msg("NON_NUM_VECTOR", lang="en"))
      testthat::expect_error(feR::welch_test(ToothGrowth$len, stop.on.error = T, lang = "en"),feR:::.error.msg("DIFF_LEN_VECTOR", lang="en"))
      testthat::expect_error(feR::welch_test(ToothGrowth$len, by = ToothGrowth$supp[1:3], stop.on.error = T, lang = "en"),feR:::.error.msg("DIFF_LEN_VECTOR", lang="en"))
      testthat::expect_error(feR::welch_test(ToothGrowth$len, by = ToothGrowth$dose, stop.on.error = T, lang = "en"),feR:::.error.msg("2_GROUPS", lang="en"))
      testthat::expect_error(feR::welch_test(as.numeric(c(1,2,rep(NA,2))), by= as.factor(c("a","b","a","b")), stop.on.error = T, lang = "en"), feR:::.error.msg("NOT_ENOUGH_X_OBS", lang="en"))
      testthat::expect_error(feR::welch_test(ToothGrowth$len[1:4], by= as.factor(c("a","b",rep(NA,2))), stop.on.error = T, lang = "en"), feR:::.error.msg("NOT_ENOUGH_BY_OBS", lang="en"))
      testthat::expect_error(feR::welch_test(ToothGrowth$len, by=ToothGrowth$supp, stop.on.error = T, ci = 2, lang="en"), feR:::.error.msg("CI_LIMITS", lang="en"))
      testthat::expect_error(feR::welch_test(ToothGrowth$len, by=ToothGrowth$supp, stop.on.error = T, lang="en", alternative = "not-valid"), feR:::.error.msg("ALTERNATIVE_T_TEST_NOT_VALID", lang="en"))
    }
  )

  #................................................................. ERRORES.. WILCOX
  testthat::test_that(
    "Checking errors in wilcoxon_test",
    {
      testthat::expect_error(feR::wilcoxon_test(stop.on.error = T, lang = "en"),feR:::.error.msg("MISSING_X", lang="en"))
      testthat::expect_error(feR::wilcoxon_test(as.character(ToothGrowth$len), stop.on.error = T, lang = "en"),feR:::.error.msg("NON_NUM_VECTOR", lang="en"))
      testthat::expect_error(feR::wilcoxon_test(ToothGrowth$len, stop.on.error = T, lang = "en"),feR:::.error.msg("DIFF_LEN_VECTOR", lang="en"))
      testthat::expect_error(feR::wilcoxon_test(ToothGrowth$len, by = ToothGrowth$supp[1:3], stop.on.error = T, lang = "en"),feR:::.error.msg("DIFF_LEN_VECTOR", lang="en"))
      testthat::expect_error(feR::wilcoxon_test(ToothGrowth$len, by = ToothGrowth$dose, stop.on.error = T, lang = "en"),feR:::.error.msg("2_GROUPS", lang="en"))
      testthat::expect_error(feR::wilcoxon_test(as.numeric(c(1,2,rep(NA,2))), by= as.factor(c("a","b","a","b")), stop.on.error = T, lang = "en"), feR:::.error.msg("NOT_ENOUGH_X_OBS", lang="en"))
      testthat::expect_error(feR::wilcoxon_test(ToothGrowth$len[1:4], by= as.factor(c("a","b",rep(NA,2))), stop.on.error = T, lang = "en"), feR:::.error.msg("NOT_ENOUGH_BY_OBS", lang="en"))
      testthat::expect_error(feR::wilcoxon_test(ToothGrowth$len, by=ToothGrowth$supp, stop.on.error = T, ci = 2, lang="en"), feR:::.error.msg("CI_LIMITS", lang="en"))
      testthat::expect_error(feR::wilcoxon_test(ToothGrowth$len, by=ToothGrowth$supp, stop.on.error = T, lang="en", alternative = "not-valid"), feR:::.error.msg("ALTERNATIVE_T_TEST_NOT_VALID", lang="en"))
    }
  )


  #................................................................... ERRORES COMP_MEANS
  testthat::test_that(
    "Checking errors in comp_means",
    {
      testthat::expect_error(feR::comp_means(ToothGrowth$len, by=ToothGrowth$supp, stop.on.error = T, lang="en", method = "not-valid"), feR:::.error.msg("T_TEST_NOT_VALID", lang="en"))
    }
  )
  #................................................................. OK
  testthat::test_that("Checking p.value in t_student",testthat::expect_equal(feR::t_student(ToothGrowth$len, by=ToothGrowth$supp, stop.on.error = T, lang="en")$p.value,t.test(ToothGrowth$len ~ ToothGrowth$supp, var.equal=T)$p.value)  )
  testthat::test_that("Checking p.value in t_student for 'less' alternative",testthat::expect_equal(feR::t_student(ToothGrowth$len, by=ToothGrowth$supp, stop.on.error = T, lang="en", alternative = "less")$p.value,t.test(ToothGrowth$len ~ ToothGrowth$supp, var.equal=T, alternative = "less")$p.value))
  testthat::test_that("Checking p.value in welch_test",testthat::expect_equal(feR::welch_test(ToothGrowth$len, by=ToothGrowth$supp, stop.on.error = T, lang="en")$p.value,t.test(ToothGrowth$len ~ ToothGrowth$supp, var.equal=F)$p.value))
  testthat::test_that("Checking p.value in welch_test for 'less' alternative",testthat::expect_equal(feR::welch_test(ToothGrowth$len, by=ToothGrowth$supp, stop.on.error = T, lang="en", alternative = "less")$p.value,t.test(ToothGrowth$len ~ ToothGrowth$supp, var.equal=F, alternative = "less")$p.value))
  testthat::test_that("Checking p.value in wilcoxon_test",testthat::expect_equal(feR::wilcoxon_test(ToothGrowth$len, by=ToothGrowth$supp, stop.on.error = T, lang="en")$p.value,wilcox.test(ToothGrowth$len ~ ToothGrowth$supp, exact =FALSE)$p.value))
  testthat::test_that("Checking p.value in wilcoxon_test for 'less' alternative",testthat::expect_equal(feR::wilcoxon_test(ToothGrowth$len, by=ToothGrowth$supp, stop.on.error = T, lang="en", alternative = "less")$p.value,wilcox.test(ToothGrowth$len ~ ToothGrowth$supp, exact =FALSE, alternative = "less")$p.value))
}
