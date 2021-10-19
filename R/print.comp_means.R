
#' print.feR.comp_means
#'
#' @export
print.feR.comp_means <- function(x){
  # if(class(x) == "data.frame")
  decimals = attr(x,"DECIMALS")
  x$stat.value <- round(x$stat.value,decimals)
  x$stat.ci.low <- round(x$stat.ci.low,decimals)
  x$stat.ci.high <- round(x$stat.ci.high,decimals)
  x$p.exact <- x$p.value
  x$p.value <- round(x$p.value, decimals)
  x$mean.diff <- round(x$mean.diff, decimals)
  x$mean.diff.ci.low <- round(x$mean.diff.ci.low, decimals)
  x$mean.diff.ci.high <- round(x$mean.diff.ci.high, decimals)

  if(!is.null(attr(x,"SHOW.DESCRIPTIVES")) && (attr(x,"SHOW.DESCRIPTIVES"))) lapply(attr(x,"DESCRIPTIVES"), print)
  if(!is.null(attr(x,"SHOW.VARIANCE")) && (attr(x,"SHOW.VARIANCE"))) lapply(attr(x,"VARIANCE"), print)

  caption = ""
  cont = 1
  for (vn in unique(x$var.name)) {
    if (caption == "") caption = vn
    else caption <- paste(caption, ifelse(cont == length(unique(x$var.name)),"&",","), vn)
    cont = cont + 1
  }
  caption <- paste(caption, "by")
  cont = 1
  for (vn in unique(x$group.var)) {
    if(cont == 1) caption <- paste(caption, vn)
    else caption <- paste(caption, ifelse(cont == length(unique(x$group.var)),"&",","), vn)
    cont = cont + 1
  }

  print(knitr::kable(x, caption = paste("Mean comparison of",caption)))
}
