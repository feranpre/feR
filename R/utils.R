#' means.default
#'

.error.msg <- function(er="DEFAULT", lang = "es", as.text = TRUE) {
  error_msg.es <- c(
    "DEFAULT"="Ha habido un problema inespecífico",
    "MEAN_NOT_NUMERIC" = "Se requiere un vector numérico o un data.frame con al menos una variable numérica",
    "MEAN_COMP_X_MISSING" = "Falta vector de datos numéricos 'X' para comparación de medias",
    "MEAN_COMP_Y_MISSING" = "Falta vector de datos numéricos 'Y' para comparación de medias",
    "MEAN_COMP_PAIRED_MUST_BE_2" = "Para comparar medias apareadas hace falta que el data.frame tenga solo dos variables",
    "MEAN_COMP_BY_MISSING" = "Falta factor de agrupacion para comparación de medias"
  )

  if(lang == "es") {
    if (!is.na(error_msg.es[er])) msg <- error_msg.es[er]
    else if(!is.na(error_msg.en[er])) msg <- error_msg.en[er]
    else msg <- "Error"
  } else {
    if(!is.na(error_msg.en[er])) msg <- error_msg.en[er]
    else msg <- "Error"
  }

  if(as.text) cat("\n[ERROR]: ", msg,"\n")
  else stop(paste0("\n[ERROR]: ", msg,"\n"))

}


.var.name <- function(xname) {
    if (grepl("$", xname, fixed=TRUE)) {
    xname <- stringr::str_extract(xname, '(?<=\\$).*')
  }
  else if (grepl("c(", xname, fixed=TRUE)) {
    xname <- xname[1:ifelse(length(xname)>=10,10,length(xname))]
  }
  return(xname)
}


#' .clean.table.var.names
#'
#' the comp.mean and means functions return a data.frame with this aspect
#'
#'   |var.name |group.var |group  | n.valid| n.missing|       min|       max| mean| sd|    median|   IQR|   se| ci.upper| ci.lower| p.norm| p.norm.exact|nor.test |is.normal |
#'   |:--------|:---------|:------|-------:|---------:|---------:|---------:|----:|--:|---------:|-----:|----:|--------:|--------:|------:|------------:|:--------|:---------|
#'   |AGE      |SEX       |Female |      18|         3|  20.66345|  37.46681|   31|  4|  32.11929|  4.65| 0.97|    32.90|    29.10|   0.24|    0.2364976|SW       |TRUE      |
#'   |AGE      |SEX       |Male   |      13|         3|  23.45472|  38.11588|   31|  5|  31.72284|  8.57| 1.47|    33.87|    28.13|   0.29|    0.2878895|SW       |TRUE      |
#'   |HEIGHT   |SEX       |Female |      18|         3| 120.00000| 205.00000|  174| 23| 182.00000| 32.25| 5.52|   184.83|   163.17|   0.09|    0.0861440|SW       |TRUE      |
#'   |HEIGHT   |SEX       |Male   |      12|         4| 120.00000| 193.00000|  161| 25| 163.00000| 33.50| 7.26|   175.22|   146.78|   0.30|    0.2990383|SW       |TRUE      |
#'
#'
#' But there are as show multiple instances of the variable name and the group name
#' The desired output is:
#'
#' |var.name |group.var |group  | n.valid| n.missing|       min|       max| mean| sd|    median|   IQR|   se| ci.upper| ci.lower| p.norm| p.norm.exact|nor.test |is.normal |
#' |:--------|:---------|:------|-------:|---------:|---------:|---------:|----:|--:|---------:|-----:|----:|--------:|--------:|------:|------------:|:--------|:---------|
#' |AGE      |data_$SEX |Female |      18|         3|  20.66345|  37.46681|   31|  4|  32.11929|  4.65| 0.97|    32.90|    29.10|   0.24|    0.2364976|SW       |TRUE      |
#' |         |          |Male   |      13|         3|  23.45472|  38.11588|   31|  5|  31.72284|  8.57| 1.47|    33.87|    28.13|   0.29|    0.2878895|SW       |TRUE      |
#' |HEIGHT   |data_$SEX |Female |      18|         3| 120.00000| 205.00000|  174| 23| 182.00000| 32.25| 5.52|   184.83|   163.17|   0.09|    0.0861440|SW       |TRUE      |
#' |         |          |Male   |      12|         4| 120.00000| 193.00000|  161| 25| 163.00000| 33.50| 7.26|   175.22|   146.78|   0.30|    0.2990383|SW       |TRUE      |
#'
#' This is what this function does
#'
#' It does something similar for stat columns but instead of leaving the first instance leaves the last one
#'
.clean.table.var.names <- function(x) {
  total.vars <- length(unique(x$var.name))
  vars <- unique(x$var.name)
  by.found <- any("group.var" %in% names(x))
  for(v in vars) {
    rows.var <- length(x$var.name[x$var.name == v])
    if(by.found) {
      for(by.var in unique(x$group.var)){
        rows.by <- length(x$var.name[x$var.name == v & x$group.var == by.var])

        if("comp.test" %in% names(x)) {
          if(table(x$comp.test) == rows.by) x$comp.test <- c(rep(" ", rows.by -1), x$comp.test[rows.by])
        }
        if("df" %in% names(x)) {
          if(table(x$df) == rows.by) x$df <- c(rep(" ", rows.by -1), x$df[rows.by])
        }

        if("stat.name" %in% names(x)) {
          if(table(x$stat.name) == rows.by) x$stat.name <- c(rep(" ", rows.by -1), x$stat.name[rows.by])
        }
        if("stat.value" %in% names(x)) {
          if(table(x$stat.value) == rows.by) x$stat.value <- c(rep(" ", rows.by -1), x$stat.value[rows.by])
        }
        if("p.value" %in% names(x)) {
          if(table(x$p.value) == rows.by) x$p.value <- c(rep(" ", rows.by -1), x$p.value[rows.by])
        }
        if("p.value.exact" %in% names(x)) {
          if(table(x$p.value.exact) == rows.by) x$p.value.exact <- c(rep(" ", rows.by -1), x$p.value.exact[rows.by])
        }
        if("p.symbols" %in% names(x)) {
          if(table(x$p.symbols) == rows.by) x$p.symbols <- c(rep(" ", rows.by -1), x$p.symbols[rows.by])
        }
        if("stat.ci.high" %in% names(x)) {
          if(table(x$stat.ci.high) == rows.by) x$stat.ci.high <- c(rep(" ", rows.by -1), x$stat.ci.high[rows.by])
        }
        if("stat.ci.low" %in% names(x)) {
          if(table(x$stat.ci.low) == rows.by) x$stat.ci.low <- c(rep(" ", rows.by -1), x$stat.ci.low[rows.by])
        }
        x$group.var[x$var.name == v & x$group.var == by.var] <- c(by.var,rep(" ",rows.by -1))
      }
    }
    x$var.name[x$var.name == v] <- c(v,rep(" ",rows.var -1))
  }
  return(x)
}
