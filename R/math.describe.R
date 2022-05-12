#' @export
#'
math_describe <- function(x, na.rm = TRUE, ci = 0.95, decimals = 4, DEBUG = FALSE) {

  UseMethod("math_describe")
}

#' @export
math_describe.integer <- function(x, na.rm = TRUE, ci = 0.95, decimals = 4, DEBUG = FALSE) { feR::math_describe.numeric(x, na.rm = TRUE, ci = 0.95, decimals = 4, DEBUG = FALSE)}

#' @export
math_describe.numeric <- function(x,na.rm = TRUE, ci = 0.95, decimals = 4, DEBUG = FALSE) {
  n.missing = sum(is.na(x))
  n.valid = length(x) - n.missing
  x.normal = feR:::normal.test(x, n.valid=n.valid, decimals = decimals)
  is.x.normal = ifelse(x.normal$is.normal == TRUE & !is.na(x.normal$is.normal),TRUE,FALSE)
  alpha_2 <- ci+((1-ci)/2) #... alpha halves for confidence interval



  min = ifelse(n.valid > 1, min(x, na.rm = TRUE), NA)
  max = ifelse(n.valid > 1, max(x, na.rm = TRUE), NA)
  mean = ifelse(n.valid > 1, mean(x, na.rm = TRUE), NA)
  sd = ifelse(n.valid > 1, sd(x, na.rm = TRUE), NA)
  median = ifelse(n.valid > 1, median(x, na.rm = TRUE), NA)
  IQR = ifelse(n.valid > 1, IQR(x, na.rm = TRUE), NA)
  se <- ifelse(n.valid > 1, sd(x, na.rm = TRUE)/sqrt(n.valid), NA)
  if (n.valid > 1) {

    if(is.x.normal){
      error <- qnorm(alpha_2)*se
    } else {
      error <- qt(alpha_2, df=n.valid -1)*se
    }
    ci.upper <- mean + error
    ci.lower <- mean - error
  } else {
    ci.upper = NA
    ci.lower = NA
  }

  result <- data.frame(STAT = c("n.valid","n.missing",
                                "min","max","mean",
                                "sd","median","IQR","se",
                                "ci.upper","ci.lower",
                                "p.norm.exact","p.norm","nor.test","is.normal"),
                       VALUE = c(n.valid,n.missing,
                                   round(c(min,max,mean,
                                   sd,median,IQR,se,ci.upper,ci.lower,
                                   x.normal$p.exact.value), digits = decimals)
                                ,x.normal$p.value,x.normal$test,x.normal$is.normal)
    )

  class(result) <- append("feR_math_describe_numeric",class(result))
  return(result)
}

#' @export
print.feR_math_describe_numeric <- function(obj) {
  print(knitr::kable(obj))
}
