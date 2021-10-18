.error.msg <- function(er="DEFAULT", lang = "es") {
  error_msg.es <- c(
    "DEFAULT"="Error",
    "MISSING_X"="falta 'x'",
    "MISSING_BY"="falta 'by'",
    "NON_NUM_VECTOR"="vector no numérico",
    "DIFF_LEN_VECTOR" = "los vectores tienen tamaños diferentes",
    "2_GROUPS" = "there must be exactly 2 groups",
    "NOT_ENOUGH_X_OBS" = "not enough 'x' observations",
    "NOT_ENOUGH_BY_OBS" = "not enough 'by' observations",
    "MISSING_CI" = "missing 'confidence interval'",
    "CI_LIMITS" = "'confidence interval' must be a number > 0 and < 1",


    "DEFAULT"="Ha habido un problema inespecífico",
    "T_STUDENT"="Ha habido un problema al realizar un t.test (Student)" ,
    "T_WELCH"="Ha habido un problema al realizar un t.test (Welch)",



    "MEAN_NOT_NUMERIC" = "Se requiere un vector numérico o un data.frame con al menos una variable numérica",
    "MEAN_BY" = "Se ha especificado una variable de agrupación que no puede ser usada",
    "MEAN_COMP_X_MISSING" = "Falta vector de datos numéricos 'X' para comparación de medias",
    "MEAN_COMP_Y_MISSING" = "Falta vector de datos numéricos 'Y' para comparación de medias",
    "MEAN_COMP_PAIRED_MUST_BE_2" = "Para comparar medias apareadas hace falta que el data.frame tenga solo dos variables",
    "MEAN_COMP_BY_MISSING" = "Falta factor de agrupacion para comparación de medias",
    "MEAN_COMP_BY_ERROR" = "Se ha detectado un factor de agrupación pero no puede usarse por algún motivo",
    "VARIANCE_TEST_X_MISSING" = "Falta variable numérica para test de varianzas",
    "VARIANCE_TEST_BY_MISSING" = "Falta factor de agrupacion para test de varianzas",
    "VARIANCE_TEST_1_GROUP" = "La variable grupo necesita ser categorica y tener al menos 2 niveles"
  )

  error_msg.en <- c(
    "DEFAULT"="Error",
    "MISSING_X"="missing x",
    "MISSING_BY"="missing by",
    "NON_NUM_VECTOR"="non-numeric vector",
    "DIFF_LEN_VECTOR" = "vectors have differente lenghts",
    "2_GROUPS" = "there must be exactly 2 groups",
    "NOT_ENOUGH_X_OBS" = "not enough 'x' observations",
    "NOT_ENOUGH_BY_OBS" = "not enough 'by' observations",
    "MISSING_CI" = "missing 'confidence interval'",
    "CI_LIMITS" = "'confidence interval' must be a number > 0 and < 1",
    "ALTERNATIVE_T_TEST_NOT_VALID" = "an alternative hypothesis was specified that is not correct. Correct options are: 'two.sided', 'less' and 'greater'"
  )

  if(lang == "es") {
    if (!is.na(error_msg.es[er])) msg <- error_msg.es[er]
    else if(!is.na(error_msg.en[er])) msg <- error_msg.en[er]
    else msg <- "Error"
  } else {
    if(!is.na(error_msg.en[er])) msg <- error_msg.en[er]
    else msg <- "Error"
  }
}

.check.t_test.parameters <- function(x,by,ci=0.95,alternative="two.sided", lang = "en") {

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
  feR:::.check.stat.parameters(ci=ci, lang = lang)
}

.check.stat.parameters <- function(ci, lang = "en") {
  if(ci<0 | ci >1) stop(feR:::.error.msg("CI_LIMITS", lang=lang))
}
