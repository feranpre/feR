means_desc <- function(x, p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001, ci = .95, ...) {

  n.missing = sum(is.na(x))
  n.valid = length(x) - n.missing
  x.normal = feR:::normal.test(x, n.valid=n.valid)


  min = ifelse(n.valid > 1, min(x, na.rm = TRUE), NA)
  max = ifelse(n.valid > 1, max(x, na.rm = TRUE), NA)
  mean = ifelse(n.valid > 1, mean(x, na.rm = TRUE), NA)
  sd = ifelse(n.valid > 1, sd(x, na.rm = TRUE), NA)
  median = ifelse(n.valid > 1, median(x, na.rm = TRUE), NA)
  IQR = ifelse(n.valid > 1, IQR(x, na.rm = TRUE), NA)
  se <- ifelse(n.valid > 1, sd(x, na.rm = TRUE)/sqrt(n.valid), NA)
  if (n.valid > 1) {
    alpha_2 <- ci+((1-ci)/2)
    if(!is.na(x.normal$is.normal) && (x.normal$is.normal)){
      error <- qnorm(alpha_2)*se
    } else {
      error <- qt(alpha_2, df=n.valid -1)*se
    }
    ci.upper <- mean + error
    ci.lower <- mean - error
  }
  else {
    ci.upper = NA
    ci.lower = NA
  }

  result <- data.frame("n.valid" = n.valid,
                       "n.missing" = n.missing,
                       "min" = min,
                       "max" = max,
                       "mean" = mean,
                       "sd" = sd,
                       "median" = median,
                       "IQR" = IQR,
                       "se" = se,
                       "ci.upper" = ci.upper,
                       "ci.lower" = ci.lower,
                       "p.norm" = x.normal$p.value,
                       "p.norm.exact" = x.normal$p.exact.value,
                       "nor.test" = x.normal$test,
                       "is.normal" =  x.normal$is.normal)


  # class(result) <- append("feR.means",class(result))

  return(result)
}
