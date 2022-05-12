#' @export
describe <- function(x, ...,
                     decimals = 4,
                     guess.factor = FALSE,
                     max.factor.cat = 10,
                     na.rm = TRUE,
                     ci = 0.95,
                     DEBUG = TRUE) {

  args <- formals(describe)
  args$... <- NULL

  passed.args <- as.list(match.call()[-1])
  final.args <- as.list(modifyList(args, passed.args))


  do.call(".describe",final.args)
}

#' @export
.describe <- function(x,..., DEBUG=FALSE){
  if(DEBUG) cat("\n[.describe] ->",class(x),"\n")

  args <- list(...)

  if(class(x) != "data.frame" & !is.factor(x)){



    if(args[["guess.factor"]]) {

      max.factor.cat <- args[["max.factor.cat"]]
      total.cat <- length(unique(x))
      length.limit <- (length(x)*.1)

      if(DEBUG) cat("\n[.describe] Guess factor ->",args[["guess.factor"]],
                    "\n max.factor =", max.factor.cat,
                    "\n total.cat =", total.cat,
                    "\n length.limit =", length.limit,
                    "\n")
      if((total.cat <= max.factor.cat) & (total.cat < length.limit)) {
        #.... if we have a numeric variable with less than max.factor.cat different values
        #     AND there are less than 10% of the length of the vector in different values
        #     then we can guess that it's a factor
        x <- factor(x)
        if(DEBUG) cat("\n[.describe] -> Factor guess: IS FACTOR =>",class(x),"\n")
        # UseMethod(".describe",a)

      }
    }

  }

  UseMethod(".describe",x)
}



#' @export
.describe.data.frame <- function(x,...) {

  args=list(...)
  result.num <- data.frame()
  result.cat <- data.frame()
  for(var.name in names(x)){
    args$x <- x[,var.name]
    args$xname <- var.name
    var <- do.call(".describe",args)
    if(("stat" %in% names(var))){
      #... numeric result
      if(!("stat" %in% names(result.num))) result.num <- data.frame("stat" = var$stat)
      result.num <- cbind(result.num, var$value)
      names(result.num)[length(names(result.num))] <- var$value[var$stat=="var.name"]
    } else {
      #... factor result
      if(!("group" %in% names(result.cat))) result.num <- data.frame("group" = var$group)
      result.cat <- rbind(result.cat, var)
      # names(result.num)[length(names(result.num))] <- var$value[var$stat=="var.name"]
    }

  }

  r.num.present <- nrow(result.num) > 0
  r.cat.present <- nrow(result.cat) > 0
  if(r.num.present & r.cat.present) return(list(result.num,result.cat))
  else if(r.num.present) return(result.num)
  else if(r.cat.present) return(result.cat)
}

#' @export
.describe.numeric <- function(x,...,
                              xname=  feR:::.var.name(deparse(substitute(x))),
                              decimals = 4,
                              ci = 0.95,
                              na.rm = TRUE,
                              DEBUG = FALSE) {
  if(DEBUG) cat("\n[.describe.numeric] Called\n")
  args <- list(...)

  var.name = xname
  if(DEBUG) cat("\n[.describe.numeric] var.name =",var.name,"\n x =",x,"\n")
  n.missing = sum(is.na(x))
  n.valid = length(x) - n.missing
  x.normal = feR:::normal.test(x, n.valid=n.valid, decimals = decimals)
  is.x.normal = ifelse(x.normal$is.normal == TRUE & !is.na(x.normal$is.normal),TRUE,FALSE)
  alpha_2 <- ci+((1-ci)/2) #... alpha halves for confidence interval



  min = ifelse(n.valid > 1, min(x, na.rm = na.rm), NA)
  max = ifelse(n.valid > 1, max(x, na.rm = na.rm), NA)
  mean = ifelse(n.valid > 1, mean(x, na.rm = na.rm), NA)
  sd = ifelse(n.valid > 1, sd(x, na.rm = na.rm), NA)
  median = ifelse(n.valid > 1, median(x, na.rm = na.rm), NA)
  IQR = ifelse(n.valid > 1, IQR(x, na.rm = na.rm), NA)
  se <- ifelse(n.valid > 1, sd(x, na.rm = na.rm)/sqrt(n.valid), NA)
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

  result <- data.frame(stat = c("var.name","n.valid","n.missing",
                                "min","max","mean",
                                "ci.upper","ci.lower",
                                "sd","se","median","IQR",
                                "p.norm.exact","p.norm","nor.test","is.normal"),
                       value = c(var.name,n.valid,n.missing,
                                 round(c(min,max,mean,ci.upper,ci.lower,
                                         sd,se,median,IQR,
                                         x.normal$p.exact.value), digits = decimals)
                                 ,x.normal$p.value,x.normal$test,x.normal$is.normal)
  )
  class(result) <- append("feR_describe_numeric",class(result))
  return(result)
}


#' @export
.describe.factor <- function(x,...,
                            decimals = 4,
                            DEBUG = FALSE) {
  if(DEBUG) cat("\n[.describe.factor] Called\n")
  if(class(x)!="factor") x <- factor(x)

  result <- data.frame("group" = c(levels(x),"NA"))
  t.n <- table(x, useNA = "always")
  result$n <- as.data.frame(t.n)$Freq
  result$rel.freq <- round(prop.table(t.n)*100,digits = decimals)
  result$rel.freq.valid <- c(round(prop.table(table(x, useNA = "no"))*100,digits = decimals),NA)
  return(result)

}
