#' means
#'
#' @export
#'
means <- function(x, ..., xname=  feR:::.var.name(deparse(substitute(x))),
                  by=NULL, byname = feR:::.var.name(deparse(substitute(by))),
                  decimals=2,
                   DEBUG=FALSE,
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
                   show.desc=TRUE, paired = T, stop.on.error=FALSE){

  UseMethod("means", x)

}


#' means.default
#'
#' @export
#'
means.default <- function(x, ..., xname=  feR:::.var.name(deparse(substitute(x))),
                          by=NULL, byname = feR:::.var.name(deparse(substitute(by))),
                          decimals=2,
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
                          show.desc=TRUE, paired = T, stop.on.error=TRUE){

  feR:::.error.msg("MEAN_NOT_NUMERIC", lang = lang, stop.on.error = stop.on.error)
  # print(xname)
}


#' means.numeric
#'
#' @export
#'
means.numeric <- function(x, ..., xname= feR:::.var.name(deparse(substitute(x))),
                          by=NULL, byname = feR:::.var.name(deparse(substitute(by))),decimals=2,
                 DEBUG=FALSE,
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
  if(DEBUG) cat("\n [means.numeric] START...\n")
  #........................................................... BY .............
  if(!missing(by)) {
    if(!is.factor(by)) {
      by.value = as.factor(by)
    }
    else by.value = by

    for (level in levels(by.value)){
      res <- .means(x[by.value == level], xname= deparse(substitute(x)), by=by, decimals=decimals,
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
                    paired=paired, errors.as.text = errors.as.text)
      res$group = level
      if(exists("group.res")) group.res <- rbind(group.res, res)
      else group.res <- res
      res <- NULL
    }
    res = group.res
  }

  else {
    res <- .means(x, xname= deparse(substitute(x)), by=by,decimals=decimals,
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
                  paired=paired, errors.as.text = errors.as.text)

  }

  if(exists("res")){
    # print(ifelse(nrow(res)==1,xname,c(xname,rep(" ",nrow(res)-1))))
    # res$var.name <- " "
    res$var.name[1] <- xname
    if(any(names(res)=="group")) {
      # res$group.var = " "
      res$group.var[1] = byname
      res <- res[,c("var.name","group.var","group",names(res)[1:(length(names(res))-3)])]
    }
    else res <- res[,c("var.name",names(res)[-length(names(res))])]

    # if(!from.data.frame) class(res) <- c("feR.means", class(res))
    return(res)

  } else return(NA)

}




#' means.data.frame
#'
#' @export
#'
means.data.frame <- function(x, ..., xname= deparse(substitute(x)),by=NULL, byname = deparse(substitute(by)), decimals=2,
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
                         show.desc=TRUE, paired = T, errors.as.text=FALSE){

  if(DEBUG) cat("\n [means.data.frame] START...\n")
  x.data.frame <- x
  x <- NULL
  num.var.exists <- FALSE

  #.... checking if there is a by and if its a vector or a data.frame
  found.by <- !missing(by)
  if(found.by) {
    #... by is a vector
    if(length(by) == nrow(x.data.frame)) {
      by.data = as.data.frame(by)
      names(by.data) <- byname
    }
    #... is not a vector, lets check for var names
    else {
      # print("BY with names")
      if(any(by %in% names(x.data.frame))) {
        by.names <- by[by %in% names(x.data.frame)]
        by.data <- as.data.frame(x.data.frame[,by.names])
        names(by.data) <- by.names
      }
      else feR:::.error.msg("MEAN_BY", lang = lang, stop.on.error = stop.on.error)
      # print(by.data)
    }
  }

  #.... checking if there are var names in ...
  parametros <- unlist(list(...))
  # print(parametros)
  param.all <- all(parametros %in% names(x.data.frame))
  if(param.all) {
    x.data.frame <- x.data.frame[,parametros]
  }


  for(var in names(x.data.frame)) {
    x <-  dplyr::pull(x.data.frame, var)
    if(is.numeric(x)) {
      if(found.by) {
        #.... by is a vector with the same length as the data.frame
        # if(length(by) == length(x)) res <- means(x, xname = var, by = by, byname = byname, from.data.frame = TRUE)
        # else {
        #
          # print("BY")
        #   print(by)
        #   print(names(x.data.frame))
        #   #... check if its a varname
        #   if(all(by %in% names(x.data.frame))) {
            for(by.name in names(by.data)) {
              # print(by.name)
              by.value <- dplyr::pull(by.data, by.name)
              by.res <- feR::means(x, xname = var, by = by.value, byname = by.name, from.data.frame = TRUE)
              if(!exists("res")) res <- as.data.frame(by.res)
              else res <- rbind(res, by.res)
            }

          # } else {
          #   print("ERROR IN BY")
          # }
        # }
      }
      #... no by
      else res <- feR::means(x, xname = var, from.data.frame = TRUE)
      if(!exists("final.res")) final.res <- as.data.frame(res)
      else final.res <- rbind(final.res, res)
      res <- NULL
      num.var.exists <- TRUE
    }
  }

  if(num.var.exists) {
    if(exists("final.res")) {
      class(final.res) <- c("feR.means", class(final.res))
      return(final.res)
    }
    else return(NA)
  } else feR:::.error.msg("MEAN_NOT_NUMERIC", lang = lang, stop.on.error = stop.on.error)

}





.means <- function(x, decimals = 2, p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001, ci = .95, ...) {
  # require("nortest")

  # if (!is.numeric(x)) {
  #   warning(paste0("[.means] ERROR - Variable ",xname," is not numeric"), call. = FALSE)
  #   return(NA)
  # }

  n.missing = sum(is.na(x))
  n.valid = length(x) - n.missing
  x.normal = feR:::.normal(x, n.valid=n.valid)

  if (n.valid > 1) {
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
  class(result) <- append("feR.means",class(result))

  return(result)
}

#' print.feR.means
#'
#' @export
print.feR.means <- function(x) {
  x <- feR:::.clean.table.var.names(x)
  print(knitr::kable(x))
}
