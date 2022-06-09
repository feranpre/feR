
#' normal.test
#'
#' Test if a sample is normal or not based on the best available test,
#' the default is Shapiro-Wilks but if conditions are not met then
#' Lilliefor's correction for Kolmogorov-Smirnof is used instead
#' 
#' It also gives a boolean `is.normal` value based on p.sig
#'
#' @param x vector with numeric data.
#' @param decimals sumber of decimal values (default = 2).
#' @param p.sig p value under witch we consider the test statistically significant (default = 0.05).
#' @param p.sig.small p value defined as "small" (default = 0.01).
#' @param p.sig.very.small p value defined as "very small", this value will be some
#'                         times represented as p<0.001 (default = 0.001).
#' @param stop.on.error if the function should stop on errors or just print them and go on (default=TRUE).
#' @param show.error wether to show an error (default=TRUE).
#'
#' @return a feR.normality that is a data.frame with:
#'  + is.normal: (bool) TRUE if p.value is below p.sig
#'  + p.exact.value: p value for the test
#'  + test: (character) test used -> SW for Shapiro-Wilks, Lillie(KS) for Lilliefor's correction for Kolmogorov-Smirnof test
#'  + statistic: (num) value of the statistic corresponding to the test performed
#'  + p.value: (num) p value rounded to "digits" decimal places
#'
#' @examples
#'
#' feR::normal.test(mtcars$mpg)
#'
#' @export
normal.test <- function(x, decimals = 2,
                        p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001,
                        stop.on.error = TRUE, show.error = TRUE) {

  n.valid = length(x) - sum(is.na(x))

  nor.test.error = FALSE
  if (n.valid > 3 & n.valid < 5000) {
    norm = tryCatch(shapiro.test(x),
                    error = function(e) {
                      if(stop.on.error) stop(e)
                      else {
                        if (show.error) message(e)
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
                                if (show.error) message(e)
                                return(NA)
                              }
                            })
    nor.test = "Lillie (KS)"
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




#' is.normal
#'
#' Evaluates if a vector 'x' is follows a normal distribution or not
#'
#' @param x numeric vector
#' @param p.sig (default=0.05) value under wich p has to be to reject normality
#'
#' @return (bool) TRUE if the vector is normal, FALSE in any other case
#' @examples
#'
#' is.norma(mtcars$mpg)
#'
#' @export
is.normal <- function(x, p.sig = 0.05) {
  r <- feR::normal.test(x, p.sig = p.sig)
  return(r$is.normal)
}