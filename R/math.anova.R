anova_test <- function(x,y,
                       xname=feR:::.var.name(deparse(substitute(x))),
                       yname=feR:::.var.name(deparse(substitute(y))),
                       ci=0.95,alternative="two.sided",
                       stop.on.error = TRUE, show.error = TRUE, lang = "es", decimals = 2
                       ) {
  a <- anova(x ~ y)

}
