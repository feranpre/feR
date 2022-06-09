
#' @export
.describe.feR_math.numeric <- function(x, ..., y = NULL,
                                       decimals = 4,
                                       p.sig = 0.05,
                                       na.rm = TRUE,
                                       DEBUG = TRUE) {
  
  ci <- 1 - p.sig
  if(DEBUG) cat("\n[.describe.feR_math.numeric] Called\n")
  args <- list(...)

  var.name = args[["xname"]]
  if(DEBUG) cat("\n[.describe.feR_math.numeric] var.name =",var.name,"\n x =",x,"\n")
  n.missing = sum(is.na(x))
  n.valid = length(x) - n.missing
  x.normal = feR:::normal.test(x, decimals = decimals)
  is.x.normal = ifelse(x.normal$is.normal == TRUE & !is.na(x.normal$is.normal),TRUE,FALSE)
  alpha_2 <- ci+((1-ci)/2) #... alpha halves for confidence interval



  min <- ifelse(n.valid > 1, min(x, na.rm = na.rm), NA)
  max <- ifelse(n.valid > 1, max(x, na.rm = na.rm), NA)
  mean <- ifelse(n.valid > 1, mean(x, na.rm = na.rm), NA)
  sd <- ifelse(n.valid > 1, sd(x, na.rm = na.rm), NA)
  median <- ifelse(n.valid > 1, median(x, na.rm = na.rm), NA)
  IQR <- ifelse(n.valid > 1, IQR(x, na.rm = na.rm), NA)
  se <- ifelse(n.valid > 1, sd(x, na.rm = na.rm)/sqrt(n.valid), NA)
  if (n.valid > 1) {

    if(is.x.normal){
      error <- qnorm(alpha_2) * se
    } else {
      error <- qt(alpha_2, df = n.valid - 1) * se
    }
    ci.upper <- mean + error
    ci.lower <- mean - error
  } else {
    ci.upper <- NA
    ci.lower <- NA
  }

  result <- data.frame(n.valid = n.valid,
                       n.missing = n.missing,
                       min = min,
                       max = max,
                       mean = mean,
                       ci.upper = ci.upper,
                       ci.lower = ci.lower,
                       sd = sd,
                       se = se,
                       median = median,
                       IQR = IQR,
                       p.norm.exact = x.normal$p.exact.value

  )

  class(result) <- c("feR_describe_numeric", class(result))
  attr(result, "var.name") <- var.name
  attr(result, "nor.test") <- x.normal$test
  attr(result, "is.normal") <- x.normal$is.normal
  attr(result, "p.norm") <- x.normal$p.value
  attr(result, "decimals") <- decimals
  return(result)
}
