
#' normal.test
#'
#' FUNCTION_DESCRIPTION
#'
#' @param x DESCRIPTION.
#' @param n.valid DESCRIPTION.
#' @param decimals DESCRIPTION.
#' @param p.sig DESCRIPTION.
#' @param p.sig.small DESCRIPTION.
#' @param p.sig.very.small DESCRIPTION.
#' @param stop.on.error DESCRIPTION.
#' @param show.error DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
#' @export
normal.test <- function(x, n.valid = NULL, decimals = 2, p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001,
                        stop.on.error = TRUE, show.error = TRUE) {

  if (is.null(n.valid)) n.valid = length(x) - sum(is.na(x))

  nor.test.error = FALSE
  if (n.valid > 3 & n.valid < 5000) {
    norm = tryCatch(shapiro.test(x),
                    error = function(e) {
                      if(stop.on.error) stop(e)
                      else {
                        if (show.errors) message(e)
                        return(NA)
                      }
                    })
    nor.test = "SW"
  }
  else if (n.valid > 4) {
    norm = tryCatch(nortest::lillie.test(x),
                error = function(e) {
                              if(stop.on.error) stop(e)
                              else {
                                if (show.errors) message(e)
                                return(NA)
                              }
                            })
    nor.test = "Lillie (KS)"
  # } else if (n.valid > 0) {
  #   norm = tryCatch(ks.test(x, "pnorm"),
  #                     error = function(e) {
  #                       if(stop.on.error) stop(e)
  #                       else {
  #                         if (show.errors) message(e)
  #                         return(NA)
  #                       }
  #                     })
  #   nor.test = "KS"
  } else {
    p.norm.exact = NA
    norm.stat = NA
    nor.test = "n too low"
    nor.test.error =TRUE
  }

  if(!nor.test.error) {
    p.norm.exact = norm$p.value
    norm.stat = norm$statistic
  }

  if (!nor.test.error) is.normal = p.norm.exact > p.sig
  else is.normal = NA
  is.normal <- data.frame(is.normal = is.normal)
  is.normal$p.exact.value = p.norm.exact
  is.normal$test = nor.test
  is.normal$statistic = norm.stat

  if( (p.norm.exact <= p.sig.very.small) & !nor.test.error)  p.norm <- paste0(" <",p.sig.very.small)
  else p.norm <- round(p.norm.exact, digits = decimals+1)

  is.normal$p.value <- p.norm

  class(is.normal) = append("feR.normality", class(is.normal))
  return(is.normal)
}
