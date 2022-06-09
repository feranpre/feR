
.check.comp_means.parameters <- function(x, y, ci=0.95,
                                        alternative="two.sided", 
                                        lang = "en", method = "auto") {

  if (missing(x)) stop(feR:::.error.msg("MISSING_X", lang = lang))
  if (missing(y)) stop(feR:::.error.msg("MISSING_BY", lang = lang))
  if (!is.numeric(x)) stop(feR:::.error.msg("NON_NUM_VECTOR", lang = lang))
  if (length(x) != length(y)) stop(feR:::.error.msg("DIFF_LEN_VECTOR", lang = lang))
  if (!is.factor(y)) y <- as.factor(y)
  if (length(levels(y)) != 2) stop(feR:::.error.msg("2_GROUPS", lang = lang))
  if (sum(!is.na(x)) < 4) stop(feR:::.error.msg("NOT_ENOUGH_X_OBS", lang = lang))
  if (sum(!is.na(y)) < 4) stop(feR:::.error.msg("NOT_ENOUGH_BY_OBS", lang = lang))
  if (missing(ci)) stop(feR:::.error.msg("MISSING_CI", lang = lang))
  if (!any(alternative %in% c("two.sided", "less", "greater"))) stop(
                                feR:::.error.msg("ALTERNATIVE_T_TEST_NOT_VALID", lang = lang)
                                                                    )
  if (!any(method %in% c("auto", "student", "welch"))) stop(
                                            feR:::.error.msg("T_TEST_NOT_VALID", lang = lang)
                                            )
  feR:::.check.stat.parameters(ci = ci, lang = lang)
}
