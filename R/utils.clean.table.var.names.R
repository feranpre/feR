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
.clean.table.var.names <- function(x, DEBUG=FALSE) {
  if(DEBUG){
    cat("\n--------------------- PRINT-----------------\n")
    print(class(x))
    print(attr(x,"TEST"))
    cat("\n---------------------------------------------\n")
  }
  # total.vars <- length(unique(x$var.name))
  by.found <- any("group.var" %in% names(x))
  for(v in unique(x$var.name)) {
    rows.var <- length(x$var.name[x$var.name == v])
    if(by.found) {
      for(by.var in unique(x$group.var)){
        select.group <- x$var.name == v & x$group.var == by.var
        rows.by <- length(x$var.name[select.group])

        if(!is.null(attr(x,"TEST"))) {
          if(attr(x,"TEST") == "t.test") {
            if(DEBUG) cat("\n Cleaning t.test table\n")

            if("comp.test" %in% names(x)) {
              if(length(unique(x$comp.test[select.group])) == 1)
                x$comp.test[select.group] <- c(rep(" ", rows.by -1), x$comp.test[select.group][rows.by])
            }
            if("df" %in% names(x)) {
              if(length(unique(x$df[select.group])) == 1) x$df[select.group] <- c(rep(" ", rows.by -1), x$df[select.group][rows.by])
            }

            if("stat.name" %in% names(x)) {
              if(length(unique(x$stat.name[select.group])) == 1) x$stat.name[select.group] <- c(rep(" ", rows.by -1), x$stat.name[select.group][rows.by])
            }
            if("stat.value" %in% names(x)) {
              if(length(unique(x$stat.value[select.group])) == 1) x$stat.value[select.group] <- c(rep(" ", rows.by -1), x$stat.value[select.group][rows.by])
            }
            # if("p.value" %in% names(x)) {
            #   if(length(unique(x$p.value[select.group])) == 1) x$p.value[select.group] <- c(rep(" ", rows.by -1), x$p.value[select.group][rows.by])
            # }
            if("p.value.exact" %in% names(x)) {
              if(length(unique(x$p.value.exact[select.group])) == 1) x$p.value.exact[select.group] <- c(rep(" ", rows.by -1), x$p.value.exact[select.group][rows.by])
            }
            if("p.symbols" %in% names(x)) {
              if(length(unique(x$p.symbols[select.group])) == 1) x$p.symbols[select.group] <- c(rep(" ", rows.by -1), x$p.symbols[select.group][rows.by])
            }
            if("stat.ci.high" %in% names(x)) {
              if(length(unique(x$stat.ci.high[select.group])) == 1) x$stat.ci.high[select.group] <- c(rep(" ", rows.by -1), x$stat.ci.high[select.group][rows.by])
            }
            if("stat.ci.low" %in% names(x)) {
              if(length(unique(x$stat.ci.low[select.group])) == 1) x$stat.ci.low[select.group] <- c(rep(" ", rows.by -1), x$stat.ci.low[select.group][rows.by])
            }

          }
        }
        x$group.var[select.group] <- c(by.var,rep(" ",rows.by -1))
      }
    }
    x$var.name[x$var.name == v] <- c(v,rep(" ",rows.var -1))
  }
  return(x)
}
