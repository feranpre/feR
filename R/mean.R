#' means
#'
#' @export
#'
means <- function(x, ..., x.name= NULL,by=NULL,decimals=2,
                   DEBUG=FALSE, DEBUG.FORMA=FALSE, DEBUG.CALL=FALSE,
                   show.vars=TRUE,show.by=TRUE,show.groups=TRUE,show.n.valid=TRUE,
                   show.n.missing=TRUE,show.min=TRUE,show.max=TRUE,
                   show.mean=TRUE,show.sd=TRUE,show.median=TRUE,
                   show.IRQ=TRUE, show.se = FALSE, show.ci.upper = FALSE, show.ci.lower = FALSE,
                   show.p.norm=TRUE,show.p.norm.exact=FALSE,
                   show.nor.test=TRUE,show.is.normal=TRUE,
                   show.interpretation=FALSE,lang="es",show.global=TRUE,
                   p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001, ci = 0.95,
                   comp=FALSE,
                   show.post.hoc = TRUE,
                   show.desc=TRUE, paired = T){



  UseMethod("means")


}


#' means.default
#'
#' @export
#'
means.default <- function(x, ..., xname= deparse(substitute(x)),by=NULL,decimals=2,
                          DEBUG=FALSE, DEBUG.FORMA=FALSE, DEBUG.CALL=FALSE,
                          show.vars=TRUE,show.by=TRUE,show.groups=TRUE,show.n.valid=TRUE,
                          show.n.missing=TRUE,show.min=TRUE,show.max=TRUE,
                          show.mean=TRUE,show.sd=TRUE,show.median=TRUE,
                          show.IRQ=TRUE, show.se = FALSE, show.ci.upper = FALSE, show.ci.lower = FALSE,
                          show.p.norm=TRUE,show.p.norm.exact=FALSE,
                          show.nor.test=TRUE,show.is.normal=TRUE,
                          show.interpretation=FALSE,lang="es",show.global=TRUE,
                          p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001, ci = 0.95,
                          comp=FALSE,
                          show.post.hoc = TRUE,
                          show.desc=TRUE, paired = T){

  print("If you end up here there must be trouble")
  # print(xname)
}


#' means.numeric
#'
#' @export
#'
means.numeric <- function(x, ..., xname= deparse(substitute(x)), by=NULL,decimals=2,
                 DEBUG=FALSE, DEBUG.FORMA=FALSE, DEBUG.CALL=FALSE,
                 show.vars=TRUE,show.by=TRUE,show.groups=TRUE,show.n.valid=TRUE,
                 show.n.missing=TRUE,show.min=TRUE,show.max=TRUE,
                 show.mean=TRUE,show.sd=TRUE,show.median=TRUE,
                 show.IRQ=TRUE, show.se = FALSE, show.ci.upper = FALSE, show.ci.lower = FALSE,
                 show.p.norm=TRUE,show.p.norm.exact=FALSE,
                 show.nor.test=TRUE,show.is.normal=TRUE,
                 show.interpretation=FALSE,lang="es",show.global=TRUE,
                 p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001, ci = 0.95,
                 comp=FALSE,
                 show.post.hoc = TRUE,
                 show.desc=TRUE, paired = T){

  # print("NUMERO")
  # x.name <- deparse(substitute(x))
  # print(xname)
  return(as.data.frame(.means(x, xname= deparse(substitute(x)), by=by,decimals=decimals,
         DEBUG=DEBUG, DEBUG.FORMA=DEBUG.FORMA, DEBUG.CALL=DEBUG.CALL,
         show.vars=show.vars,show.by=show.by,show.groups=show.groups,show.n.valid=show.n.valid,
         show.n.missing=show.n.missing,show.min=show.min,show.max=show.max,
         show.mean=show.mean,show.sd=show.sd,show.median=show.median,
         show.IRQ=show.IRQ,show.se=show.se, show.ci.upper=show.ci.upper,
         show.ci.lower=show.ci.lower, show.p.norm=show.p.norm,
         show.p.norm.exact=show.p.norm.exact, show.nor.test=show.nor.test,
         show.is.normal=show.is.normal, show.interpretation=show.interpretation,
         lang=lang, show.global=show.global, p.sig=p.sig,
         p.sig.small=p.sig.small, p.sig.very.small=p.sig.very.small,
         ci=ci, comp=comp, show.post.hoc=show.post.hoc, show.desc=show.desc,
         paired=paired)))

}




#' means.data.frame
#'
#' @export
#'
means.data.frame <- function(x, ..., xname= deparse(substitute(x)),by=NULL,decimals=2,
                         DEBUG=FALSE, DEBUG.FORMA=FALSE, DEBUG.CALL=FALSE,
                         show.vars=TRUE,show.by=TRUE,show.groups=TRUE,show.n.valid=TRUE,
                         show.n.missing=TRUE,show.min=TRUE,show.max=TRUE,
                         show.mean=TRUE,show.sd=TRUE,show.median=TRUE,
                         show.IRQ=TRUE, show.se = FALSE, show.ci.upper = FALSE, show.ci.lower = FALSE,
                         show.p.norm=TRUE,show.p.norm.exact=FALSE,
                         show.nor.test=TRUE,show.is.normal=TRUE,
                         show.interpretation=FALSE,lang="es",show.global=TRUE,
                         p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001, ci = 0.95,
                         comp=FALSE,
                         show.post.hoc = TRUE,
                         show.desc=TRUE, paired = T){

  # print("df")
  # print(xname)
  x.data.frame <- x
  x <- NULL

  for(var in names(x.data.frame)) {
    x <- x.data.frame %>% pull(var)
    res <- means(x, xname = var)
    if(!exists("final.res")) final.res <- as.data.frame(res)
    else final.res <- rbind(final.res, res)
    res <- NULL
  }

  if(exists("final.res")) return(final.res)
  else return(NA)
}





.means <- function(x, xname= deparse(substitute(x)), decimals = 2, p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001, ci = .95, ...) {
  # require("nortest")

  if (!is.numeric(x)) {
    warning(paste0("[.means] ERROR - Variable ",xname," is not numeric"), call. = FALSE)
    return(NA)
  }

  n.missing = sum(is.na(x))
  n.valid = length(x) - n.missing
  x.normal = feR:::.normal(x, n.valid=n.valid)

  if (n.valid > 0) {
    min = min(x, na.rm = TRUE)
    max = max(x, na.rm = TRUE)
    mean = round(mean(x, na.rm = TRUE))
    sd = round(sd(x, na.rm = TRUE))
    median = median(x, na.rm = TRUE)
    IQR = IQR(x, na.rm = TRUE)
    se <- sd(x, na.rm = TRUE)/sqrt(n.valid)
    alpha_2 <- ci+((1-ci)/2)
    if(x.normal$is.normal){
      error <- qnorm(alpha_2)*se
    } else {
      error <- qt(alpha_2, df=n.valid -1)*se
    }
    ci.upper <- mean + error
    ci.lower <- mean - error


  } else {
    min = NA
    max = NA
    mean = NA
    sd = NA
    median = NA
    IQR = NA
    se = NA
    ci.upper = NA
    ci.lower = NA
  }





  result <- data.frame("n.valid" = n.valid,
                       "n.missing" = n.missing,
                       "min" = min,
                       "max" = max,
                       "mean" = round(mean, digits=decimals),
                       "sd" = round(sd, digits=decimals),
                       "median" = median,
                       "IQR" = round(IQR, digits=decimals),
                       "se" = round(se, digits=decimals),
                       "ci.upper" = round(ci.upper, digits=decimals),
                       "ci.lower" = round(ci.lower, digits=decimals),
                       "p.norm" = round(x.normal$p.value, digits=decimals),
                       "p.norm.exact" = x.normal$p.exact.value,
                       "nor.test" = x.normal$test,
                       "is.normal" =  x.normal$is.normal)
  class(result) <- append("udaicR",class(result))

  return(result)
}

