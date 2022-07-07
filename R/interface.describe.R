#' @TODO: Añadir un comando para separar tablas unas de otras en el print, de forma que se puedan poner cabeceras de markdown



#' describe
#'
#' `describe()` give descriptive statistics about the vector/data.frame
#' passed as argument.
#'
#' If it is a vector it will discriminate between **numeric** and **factor**
#' (it will even try to guess if a numeric variable is a factor) and will
#' give the correct descriptive statistics.
#'
#' @param x DESCRIPTION.
#' @param ... DESCRIPTION.
#' @param x.name DESCRIPTION.
#' @param y DESCRIPTION.
#' @param y.name DESCRIPTION.
#' @param decimals DESCRIPTION.
#' @param guess.factor DESCRIPTION.
#' @param max.factor.cat DESCRIPTION.
#' @param na.rm DESCRIPTION.
#' @param ci DESCRIPTION.
#' @param total.by.row DESCRIPTION.
#' @param total.by.column DESCRIPTION.
#' @param show.general DESCRIPTION.
#' @param DEBUG DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
#'
#' @export
describe <- function(x, ...,
                     x.name=  feR:::.var.name(deparse(substitute(x))),
                     y = NULL,
                     y.name =  feR:::.var.name(deparse(substitute(y))),
                     decimals = 4,
                     guess.factor = TRUE,
                     max.factor.cat = 10,
                     na.rm = TRUE,
                     ci = 0.95,

                     #----------------------- factors
                     total.by.row = TRUE,
                     total.by.column = FALSE,

                     #----------------------- printing options
                     show.general = TRUE,
                     show.markdown.division = TRUE,
                     markdown.division.prefix = "##",


                     #----------------------- comparisons


                     #----------------------- coding
                     DEBUG = FALSE) {

  args <- formals(describe)
  args$... <- NULL


  passed.args <- as.list(match.call()[-1])
  final.args <- as.list(modifyList(args, passed.args))


  if (!is.null(y)) final.args$y <- y
  if (guess.factor) final.args$x <- do.call(feR:::.guess.factor, final.args)
  do.call(feR:::.describe, final.args)
}


.describe <- function(x, ..., DEBUG = FALSE, show.general = TRUE) {
  if(is.character(x)) x <- factor(x)
  if(is.logical(x)) x <- factor(x)
  UseMethod(".describe", x)
}


.describe.data.frame <- function(x, ...) {

  args <- list(...)
  results <- list()

  for (var.name in names(x)) {
    args$x <- x[, var.name]
    args$x.name <- var.name
    var <- do.call(feR:::.describe, args)
    results[[var.name]] <- var

  }
  class(results) <- c("feR_describe_data_frame", class(results))
  return(results)
}

.describe.default <- function(x, ..., y = NULL, decimals = 4,
                              show.general = TRUE,
                              show.markdown.division = TRUE,
                              markdown.division.prefix = "##",
                              DEBUG = FALSE) {
  return(data.frame(description = "Not possible"))
}



.describe.numeric <- function(x, ..., y = NULL, decimals = 4,
                              show.general = TRUE,
                              show.markdown.division = TRUE,
                              markdown.division.prefix = "##",
                              DEBUG = FALSE) {


  if (DEBUG) cat("\n[.describe.numeric] Called\n")
  args <- list(...)
  args$x <- x
  args$DEBUG <- DEBUG


  if (is.null(y) | (show.general & !is.null(y))) {
    if (is.null(y)) {
      if (DEBUG) cat("\n[.describe.numeric] basic desc")
      result.general <- do.call(feR:::.describe.feR_math.numeric, args)
    }
    else {
      if (DEBUG) cat("\n[.describe.numeric] show.general requested\n")
      args.general <- args
      args.general$show.general <- FALSE
      args.general$y <- NULL
      result.general <- do.call(feR:::.describe.numeric, args.general)
    }
  }
  if (!is.null(y)) {
    result.temp <- tapply(x, y, function(xValue) {
                                                    args$x <- xValue
                                                    args$y <- y
                                                    do.call(feR:::.describe.feR_math.numeric, args)
                                                  })
    for (r in names(result.temp)) {
      r.temp <- result.temp[[r]]
      r.g <- data.frame(group = r)
      r.g <- cbind(r.g, r.temp)

      if (!exists("result.groups")) {
        result.groups <- r.g
        nor.test <- list(attr(r.temp, "nor.test"))
        p.norm <- list(attr(r.temp, "p.norm"))
        names(nor.test)[length(nor.test)] <- r
        names(p.norm)[length(p.norm)] <- r
        }
      else {
        result.groups <- rbind(result.groups, r.g)
        nor.test <- append(nor.test, attr(r.temp, "nor.test"))
        p.norm <- append(p.norm, attr(r.temp, "p.norm"))
        names(nor.test)[length(nor.test)] <- r
        names(p.norm)[length(p.norm)] <- r
        }

    }

    class(result.groups) <- c("feR_describe_numeric_list", class(result.groups))

    attr(result.groups, "nor.test") <- nor.test
    attr(result.groups, "p.norm") <- p.norm
    attr(result.groups, "y.name") <- args[["y.name"]]

  }

  if (exists("result.groups")) {
    result <- result.groups
    if (show.general & exists("result.general")) {
      attr(result, "result.general") <- result.general
    }
  } else {
    result <- result.general
  }
  attr(result, "decimals") <- decimals
  attr(result, "show.general") <- show.general
  attr(result, "show.markdown.division") <- show.markdown.division
  attr(result, "markdown.division.prefix") <- markdown.division.prefix
  attr(result, "x.name") <- args[["x.name"]]

  return(result)
}



#' @export
.describe.factor <- function(x,..., y = NULL,
                             # decimals = 4,
                             # total.by.row = TRUE,
                             # total.by.column = FALSE,
                             DEBUG = FALSE) {
  if(DEBUG) cat("\n[.describe.factor] Called\n")

  args <- list(...)
  args$x <- x
  args$DEBUG <- DEBUG
  if(!is.null(y)) args$y <- y
  result <- do.call(feR:::.describe.feR_math.factor, args)


  return(result)

}


