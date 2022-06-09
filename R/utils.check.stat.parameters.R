.check.stat.parameters <- function(ci, lang = "en") {
  if(ci < 0 | ci > 1) stop(feR:::.error.msg("CI_LIMITS", lang = lang))
}
