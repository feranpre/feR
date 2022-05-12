#' @export
describe <- function(x, ...,
                     decimals = 4,
                     force.factor = FALSE,
                     max.factor.cat = 10,
                     DEBUG = TRUE) {
  if(force.factor) x <- factor(x)
  UseMethod("describe",x)
}

#' @export
describe.data.frame <- function(x,...,
                                decimals = 4,
                                force.factor = FALSE,
                                max.factor.cat = 10,
                                DEBUG = TRUE) {



print(lapply(x, function(var, ...,
                   decimals = decimals,
                   force.factor = force.factor,
                   max.factor.cat = max.factor.cat,
                   DEBUG = DEBUG) {
                      UseMethod("describe",var)
                      # print(class(var))
                      # feR::describe(var, ...,
                      #               force.factor = force.factor,
                      #               max.factor.cat = max.factor.cat,
                      #               DEBUG = DEBUG)
                      #
                      #                 })
}))

}

#' @export
describe.numeric <- function(x,...,
                             decimals = 4,
                             force.factor = FALSE,
                             max.factor.cat = 10,
                             DEBUG = TRUE) {
  if(DEBUG) print("[describe.numeric]Called")
  n.missing = sum(is.na(x))
  n.valid = length(x) - n.missing

  if(n.valid > 2) {
    #... check if it could be a factor
    l.total <- length(levels(factor(x)))
    if((l.total < n.valid*.1)&(l.total <= 10)) {
      #threshold for factor is 10% of the samples AND total of categories bellow 10
      return(describe(x,..., force.factor = TRUE))
    } else {
      #.......................
      #.... numeric variable
      #.......................
      return(feR::math_describe(x,decimals = decimals))

    }
  } else {
    return("asdfsdaf")
  }
}


#' @export
describe.factor <- function(x,...,
                            decimals = 4,
                            force.factor = FALSE,
                            max.factor.cat = 10,
                            DEBUG = TRUE) {
  print(match.call(expand.dots = FALSE))
  return("FACTOR")
}
