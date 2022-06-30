
#' @export
print.feR_describe_numeric <- function(obj, raw=FALSE) {
  if (raw) {
    message("RAW")
    print(knitr::kable(obj))
    return()
  }
  decimals <- attr(obj, "decimals")

  for (v in names(obj)) {
    value <- obj[1, v]
    if (is.numeric(value)) value <- round(value, digits = decimals)

    if (exists("stats")) stats <- c(stats, v)
    else stats <- v

    if (exists("values")) values <- c(values,round(value, digits = decimals))
    else values <- round(value, digits = decimals)
  }

  x.final <- data.frame(stats = stats, value = values)

  print(knitr::kable(x.final, caption = attr(obj, "x.name")))
  message("\nNormality test:", attr(obj, "nor.test"),
      "; p.value:", attr(obj, "p.norm"), "\n")
}


#' @export
print.feR_describe_numeric_list <- function(obj) {

  decimals <- attr(obj, "decimals")
  show.markdown.division <- attr(obj, "show.markdown.division")
  markdown.division.prefix <- attr(obj, "markdown.division.prefix")

  rownames(obj) <- obj$group
  obj$group <- NULL


  if (!is.null(attr(obj, "result.general"))) {
    if (show.markdown.division) message("\n", markdown.division.prefix,
                                    "Descripción general de ", attr(obj, "x.name"), "\n")
    print(attr(obj, "result.general"))
  }

  for (v in names(obj)) {
    value <- obj[, v]
    if (is.numeric(value)) obj[, v] <- round(value, digits = decimals)
  }

  result <- t(obj)
  if (show.markdown.division) message("\n", markdown.division.prefix,
                                    "Descripción de **", attr(obj, "x.name"),
                                    "** por grupos de **",
                                    attr(obj, "y.name"), "**\n", sep = "")
  print(knitr::kable(result, caption = paste(attr(obj, "x.name"), "vs", attr(obj, "y.name"))))
  for (g in names(attr(obj, "nor.test"))) {
    message("\nNormality test ", g, ":", attr(obj, "nor.test")[[g]],
        "; p.value:", attr(obj, "p.norm")[[g]], "\n")
  }
}


#' @export
print.feR_describe_factor <- function(obj) {

  if(!is.null(attr(obj, "y.name"))) {
    print(knitr::kable(obj, caption = paste(attr(obj, "x.name"), "vs", attr(obj, "y.name"))))
  } else {
    print(knitr::kable(obj, caption = attr(obj, "x.name")))
  }
}


#' @export
print.feR_describe_data_frame <- function(obj) {
  for (x in obj) {
    print(x)
  }

}
