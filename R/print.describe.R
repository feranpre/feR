
#' @export
print.feR_describe_numeric <- function(obj, raw=FALSE) {
  if (raw) {
    message("RAW")
    print(knitr::kable(obj))
    return()
  }
  decimals <- attr(obj, "decimals")
  show.markdown.division <- attr(obj, "show.markdown.division")
  markdown.division.prefix <- attr(obj, "markdown.division.prefix")

  for (v in names(obj)) {
    value <- obj[1, v]
    if (is.numeric(value)) value <- round(value, digits = decimals)

    if (exists("stats")) stats <- c(stats, v)
    else stats <- v

    if (exists("values")) values <- c(values,round(value, digits = decimals))
    else values <- round(value, digits = decimals)
  }

  x.final <- data.frame(stats = stats, value = values)

  if (show.markdown.division) cat("\n", markdown.division.prefix,
                                  " Descripción de **", attr(obj, "x.name"),
                                  "**\n", sep = "")
  print(knitr::kable(x.final, caption = attr(obj, "x.name")))

  # cat("\n decimals ",decimals,"\n")
  # print(toString(obj))
  # print(attr(obj, "p.norm"))
  p.val <- round(obj$p.norm.exact,digits = (decimals+1))
  zeroes <- paste0(rep(0,decimals),collapse="")
  p.val <- round(obj$p.norm.exact,digits = decimals)
  if(p.val == 0) p.val <- paste0("<0.",zeroes,"1")

  cat("\nNormality test:", attr(obj, "nor.test"),
      "; p.value:", p.val, "\n", sep = "")
}


#' @export
print.feR_describe_numeric_list <- function(obj) {

  decimals <- attr(obj, "decimals")
  show.markdown.division <- attr(obj, "show.markdown.division")
  markdown.division.prefix <- attr(obj, "markdown.division.prefix")

  rownames(obj) <- obj$group
  obj$group <- NULL


  if (!is.null(attr(obj, "result.general"))) {
    if (show.markdown.division) cat("\n", markdown.division.prefix,
                                    " Descripción general de ", attr(obj, "x.name"), "\n", sep = "")
    print(attr(obj, "result.general"))
  }

  for (v in names(obj)) {
    value <- obj[, v]
    if (is.numeric(value)) obj[, v] <- round(value, digits = decimals)
  }

  result <- t(obj)
  if (show.markdown.division) cat("\n",markdown.division.prefix,
                                    " Descripción de **", attr(obj, "x.name"),
                                    "** por grupos de **",
                                    attr(obj, "y.name"), "**\n", sep = "")
  print(knitr::kable(result, caption = paste(attr(obj, "x.name"), "vs", attr(obj, "y.name"))))
  for (g in names(attr(obj, "nor.test"))) {
    p.val <- round(attr(obj, "p.norm")[[g]],digits = (decimals+1))
    if(p.val == 0) p.val <- paste0("<0.",rep(0,decimals),1)
    else p.val <- round(attr(obj, "p.norm")[[g]],digits = decimals)
    cat("\nNormality test ", g, ":", attr(obj, "nor.test")[[g]],
        "; p.value:", p.val, "\n")
  }
}


#' @export
print.feR_describe_factor <- function(obj) {
  decimals <- attr(obj, "decimals")
  show.markdown.division <- attr(obj, "show.markdown.division")
  markdown.division.prefix <- attr(obj, "markdown.division.prefix")

  n.rows <- rownames(obj)[grepl("_n",rownames(obj))]
  perc.rows <- rownames(obj)[grepl("_percent",rownames(obj))]

  if(!is.null(attr(obj, "y.name"))) {
    result <- obj[n.rows,]
    per.df <- obj[perc.rows,]
    for(c in names(result)) {
      result[,c] <- paste0(result[,c],"(",round(per.df[,c],digits=decimals),"%)")
    }
    obj <- result


    titulo <- paste(attr(obj, "x.name"), "vs", attr(obj, "y.name"))
    if (show.markdown.division) cat("\n",markdown.division.prefix," ",titulo,"\n",sep="")
    print(knitr::kable(obj, caption = titulo))
  } else {
    if (show.markdown.division) cat("\n",markdown.division.prefix," ",attr(obj, "x.name"),"\n",sep="")
    print(knitr::kable(obj, caption = attr(obj, "x.name")))
  }
}


#' @export
print.feR_describe_data_frame <- function(obj) {
  for (x in obj) {
    print(x)
  }

}
