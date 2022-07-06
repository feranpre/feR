
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

  if(!is.null(attr(x,"SHOW.DESCRIPTIVES")) && (attr(x,"SHOW.DESCRIPTIVES"))) print(attr(x,"DESCRIPTIVES"))
  if(!is.null(attr(x,"SHOW.VARIANCE")) && (attr(x,"SHOW.VARIANCE"))) print(attr(x,"VARIANCE"))

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

  # library(tidyr)
  # x.final <- x %>% pivot_longer()
  # print(x.final)

  for(v in names(x)){
    value <- as.character(x[1,v])
    if(exists("stats")) stats <- c(stats,v)
    else stats <- v

    if(exists("values")) values <- c(values,value)
    else values <- value
  }

  x.final <- data.frame(stats=stats, value=values)
  # print(x.final)

  print(knitr::kable(x.final, caption = paste("Mean comparison of",caption)))
}
