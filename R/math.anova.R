anova_test <- function(x,y,
                       xname=feR:::.var.name(deparse(substitute(x))),
                       yname=feR:::.var.name(deparse(substitute(y))),
                       ci=0.95,
                       stop.on.error = TRUE, show.error = TRUE, lang = "es", decimals = 4
                       ) {

  result <- summary(aov(x ~ y))
  xresult <- result[[1]]

  names(xresult) <- c("df","sum_sq","mean_sum_sq","F.value","p.value")
  rownames(xresult)[1] <- yname

  return(xresult)
}



kruskall_test <- function(x,y,
                       xname=feR:::.var.name(deparse(substitute(x))),
                       yname=feR:::.var.name(deparse(substitute(y))),
                       ci=0.95,
                       stop.on.error = TRUE, show.error = TRUE, lang = "es", decimals = 4
                       ) {

  k <- kruskal.test(x ~ y)

  xresult <- data.frame(chi_sq = k$statistic, df = k$parameter, p.value = k$p.value)
  rownames(xresult) <- yname


  return(xresult)
}
