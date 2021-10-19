#' means
#'
#' @export
#'
means <- function(x, ..., xname=  feR:::.var.name(deparse(substitute(x))),
                  by=NULL, byname = feR:::.var.name(deparse(substitute(by))),
                  decimals=2,
                  show.var.name=TRUE,show.group.var=TRUE,show.group=TRUE,show.n.valid=TRUE,
                  show.n.missing=TRUE,show.min=TRUE,show.max=TRUE,
                  show.mean=TRUE,show.sd=TRUE,show.median=TRUE,
                  show.IQR=TRUE, show.se = FALSE, show.ci.upper = FALSE, show.ci.lower = FALSE,
                  show.p.norm=TRUE,show.p.norm.exact=FALSE,
                  show.nor.test=TRUE,show.is.normal=TRUE,
                  show.interpretation=FALSE,lang="es",show.global=TRUE,
                   p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001, ci = 0.95,
                   comp=FALSE,
                   show.post.hoc = TRUE,
                   show.desc=TRUE, paired = T, stop.on.error=FALSE, show.error = TRUE, DEBUG=FALSE){

  if(missing(x)) {
    if(stop.on.error) stop(feR:::.error.msg(er="MISSING_X", lang=lang))
    else if (show.error) message(feR:::.error.msg(er="MISSING_X", lang=lang))
    return(NA)
  }
  UseMethod("means")

}


#' means.default
#'
#' @export
#'
means.default <- function(x, ..., xname=  feR:::.var.name(deparse(substitute(x))),
                          by=NULL, byname = feR:::.var.name(deparse(substitute(by))),
                          decimals=2,
                          DEBUG=FALSE,
                          show.var.name=TRUE,show.group.var=TRUE,show.group=TRUE,show.n.valid=TRUE,
                          show.n.missing=TRUE,show.min=TRUE,show.max=TRUE,
                          show.mean=TRUE,show.sd=TRUE,show.median=TRUE,
                          show.IQR=TRUE, show.se = FALSE, show.ci.upper = FALSE, show.ci.lower = FALSE,
                          show.p.norm=TRUE,show.p.norm.exact=FALSE,
                          show.nor.test=TRUE,show.is.normal=TRUE,
                          show.interpretation=FALSE,lang="es",show.global=TRUE,
                          p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001, ci = 0.95,
                          comp=FALSE,
                          show.post.hoc = TRUE,
                          show.desc=TRUE, paired = T, stop.on.error=TRUE, show.errors = TRUE){
  if (stop.on.error) stop(feR:::.error.msg("MISSING_X", lang=lang))
  else if (show.errors) message(feR:::.error.msg("MISSING_X", lang=lang))
  return(NA)
}


#' means.numeric
#'
#' @export
#'
means.numeric <- function(x, ..., xname= feR:::.var.name(deparse(substitute(x))),
                          by=NULL, byname = feR:::.var.name(deparse(substitute(by))),decimals=2,
                 DEBUG=FALSE,
                 show.var.name=TRUE,show.group.var=TRUE,show.group=TRUE,show.n.valid=TRUE,
                 show.n.missing=TRUE,show.min=TRUE,show.max=TRUE,
                 show.mean=TRUE,show.sd=TRUE,show.median=TRUE,
                 show.IQR=TRUE, show.se = FALSE, show.ci.upper = FALSE, show.ci.lower = FALSE,
                 show.p.norm=TRUE,show.p.norm.exact=FALSE,
                 show.nor.test=TRUE,show.is.normal=TRUE,
                 show.interpretation=FALSE,lang="es",show.global=TRUE,
                 p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001, ci = 0.95,
                 comp=FALSE,
                 show.post.hoc = TRUE,
                 show.desc=TRUE, paired = F, stop.on.error=TRUE, show.errors = TRUE){
  if(DEBUG) cat("\n [means.numeric] START...\n")
  #........................................................... BY .............
  has.groups = FALSE
  if(!missing(by)) {
    has.groups = TRUE
    #................... format and get by.value
    if(!is.factor(by)) {
      by.value = as.factor(by)
    }
    else by.value = by

    #.................. split by.value by factors
    for (level in levels(by.value)){
      res <- .means(x[by.value == level & !is.na(by.value)], decimals=decimals, p.sig=p.sig,
                    p.sig.small=p.sig.small, p.sig.very.small=p.sig.very.small,
                    ci=ci)
      res$group = level
      if(exists("group.res")) group.res <- rbind(group.res, res)
      else group.res <- res
      res <- NULL
    }
    res = group.res
    if(sum(is.na(by.value))>0) {
      group.res <- .means(x[is.na(by.value)], decimals=decimals, p.sig=p.sig,
                    p.sig.small=p.sig.small, p.sig.very.small=p.sig.very.small,
                    ci=ci)
      res$group = "NA"
      if(exists("res")) res <- rbind(res, group.res)
      else res <- group.res
      group.res <- NULL
    }
  }

  else {
    res <- feR:::.means(x, decimals=decimals, p.sig=p.sig,
                        p.sig.small=p.sig.small, p.sig.very.small=p.sig.very.small,
                        ci=ci)
  }

  if(exists("res")){

    res$var.name[1] <- xname
    if(has.groups) {
      res$group.var[1] = byname
      res <- res[,c("var.name","group.var","group",names(res)[1:(length(names(res))-3)])]
    }
    else res <- res[,c("var.name",names(res)[-length(names(res))])]

    class(res) <- c("feR.means", class(res))
    attr(res,"DECIMALS") <- decimals
    attr(res,"LANG") <- lang
    if(has.groups && show.global) attr(res,"GLOBAL") <- feR:::means(x, xname = xname, decimals=decimals, p.sig=p.sig,
                                                         p.sig.small=p.sig.small, p.sig.very.small=p.sig.very.small,
                                                         ci=ci)
    res <- feR:::.means.show.attr(res,show.var.name = show.var.name,
                            show.group.var = show.group.var,
                            show.group = show.group,
                            show.n.valid = show.n.valid,
                            show.n.missing = show.n.missing,
                            show.min = show.min,
                            show.max = show.max,
                            show.mean = show.mean,
                            show.sd = show.sd,
                            show.median = show.median,
                            show.IQR = show.IQR,
                            show.se = show.se,
                            show.ci.upper = show.ci.upper,
                            show.ci.lower = show.ci.lower,
                            show.p.norm = show.p.norm,
                            show.p.norm.exact = show.p.norm.exact,
                            show.nor.test = show.nor.test,
                            show.is.normal = show.is.normal,
                            show.global = show.global)

    return(res)

  } else return(NA)

}




#' means.data.frame
#'
#' @export
#'
means.data.frame <- function(x, ..., xname= deparse(substitute(x)),by=NULL, byname = deparse(substitute(by)),
                             decimals=2,
                         DEBUG=FALSE, DEBUG.FORMA=FALSE, DEBUG.CALL=FALSE,
                         show.var.name=TRUE,show.group.var=TRUE,show.group=TRUE,show.n.valid=TRUE,
                         show.n.missing=TRUE,show.min=TRUE,show.max=TRUE,
                         show.mean=TRUE,show.sd=TRUE,show.median=TRUE,
                         show.IQR=TRUE, show.se = FALSE, show.ci.upper = FALSE, show.ci.lower = FALSE,
                         show.p.norm=TRUE,show.p.norm.exact=FALSE,
                         show.nor.test=TRUE,show.is.normal=TRUE,
                         show.interpretation=FALSE,lang="es",show.global=TRUE,
                         p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001, ci = 0.95,
                         comp=FALSE,
                         show.post.hoc = TRUE,
                         show.desc=TRUE, paired = T, stop.on.error=TRUE, show.errors = TRUE){

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
      if(any(by %in% names(x.data.frame))) {
        by.names <- by[by %in% names(x.data.frame)]
        by.data <- as.data.frame(x.data.frame[,by.names])
        names(by.data) <- by.names
      }
      else {
        e <- feR:::.error.msg("MEAN_BY", lang = lang)
        if(stop.on.error) stop(e)
        else if (show.errors) message(e)
        return(NA)
      }
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
        if(found.by){
              for(by.name in names(by.data)) {
                # print(by.name)
                by.value <- dplyr::pull(by.data, by.name)
                by.res <- feR::means(x, xname = var, by = by.value, byname = by.name, show.global = show.global)
                if(!exists("res")) res <- by.res
                else res <- rbind(res, by.res)
              }
              # if(show.global) attr(res,"GLOBAL") <- eR::means(x, xname = var, show.global = show.global)
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
      if(!exists("final.res"))  return(NA)
      else {
        attr(final.res,"DECIMALS") <- decimals
        attr(final.res,"LANG") <- lang

        class(final.res) <- c("feR.means", class(final.res))
        final.res <- feR:::.means.show.attr(final.res,show.var.name = show.var.name,
                                show.group.var = show.group.var,
                                show.group = show.group,
                                show.n.valid = show.n.valid,
                                show.n.missing = show.n.missing,
                                show.min = show.min,
                                show.max = show.max,
                                show.mean = show.mean,
                                show.sd = show.sd,
                                show.median = show.median,
                                show.IQR = show.IQR,
                                show.se = show.se,
                                show.ci.upper = show.ci.upper,
                                show.ci.lower = show.ci.lower,
                                show.p.norm = show.p.norm,
                                show.p.norm.exact = show.p.norm.exact,
                                show.nor.test = show.nor.test,
                                show.is.normal = show.is.normal,
                                show.global = show.global)
        return(final.res)
      }
  } else {
    e <- feR:::.error.msg("MEAN_NOT_NUMERIC", lang = lang, stop.on.error = stop.on.error)
    if(stop.on.error) stop(e)
    else if (show.errors) message(e)
    return(NA)
  }
}




.means <- function(x, decimals = 2, p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001, ci = .95, ...) {

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


  class(result) <- append("feR.means",class(result))

  return(result)
}



.means.show.attr <- function(x,show.var.name =TRUE,
                 show.group.var =TRUE,
                 show.group =TRUE,
                 show.n.valid =TRUE,
                 show.n.missing =TRUE,
                 show.min =TRUE,
                 show.max =TRUE,
                 show.mean =TRUE,
                 show.sd =TRUE,
                 show.median =TRUE,
                 show.IQR =TRUE,
                 show.se =TRUE,
                 show.ci.upper =TRUE,
                 show.ci.lower =TRUE,
                 show.p.norm =TRUE,
                 show.p.norm.exact =TRUE,
                 show.nor.test =TRUE,
                 show.is.normal =TRUE,
                 show.global = TRUE){
  attr(x,"SHOW.VAR.NAME") = show.var.name
  attr(x,"SHOW.GROUP.VAR") = show.group.var
  attr(x,"SHOW.GROUP") = show.group
  attr(x,"SHOW.N.VALID") = show.n.valid
  attr(x,"SHOW.N.MISSING") = show.n.missing
  attr(x,"SHOW.MIN") = show.min
  attr(x,"SHOW.MAX") = show.max
  attr(x,"SHOW.MEAN") = show.mean
  attr(x,"SHOW.SD") = show.sd
  attr(x,"SHOW.MEDIAN") = show.median
  attr(x,"SHOW.IQR") = show.IQR
  attr(x,"SHOW.SE") = show.se
  attr(x,"SHOW.CI.UPPER") = show.ci.upper
  attr(x,"SHOW.CI.LOWER") = show.ci.lower
  attr(x,"SHOW.P.NORM") = show.p.norm
  attr(x,"SHOW.P.NORM.EXACT") = show.p.norm.exact
  attr(x,"SHOW.NOR.TEST") = show.nor.test
  attr(x,"SHOW.IS.NORMAL") = show.is.normal
  attr(x,"SHOW.GLOBAL") = show.global
  return(x)

}


#' print.feR.means
#'
#' @export
print.feR.means <- function(x) {
  decimals <- attr(x,"DECIMALS")

  show.var.name = attr(x,"SHOW.VAR.NAME")
  show.group.var = attr(x,"SHOW.GROUP.VAR")
  show.group = attr(x,"SHOW.GROUP")
  show.n.valid = attr(x,"SHOW.N.VALID")
  show.n.missing = attr(x,"SHOW.N.MISSING")
  show.min = attr(x,"SHOW.MIN")
  show.max = attr(x,"SHOW.MAX")
  show.mean = attr(x,"SHOW.MEAN")
  show.sd = attr(x,"SHOW.SD")
  show.median = attr(x,"SHOW.MEDIAN")
  show.IQR = attr(x,"SHOW.IQR")
  show.se = attr(x,"SHOW.SE")
  show.ci.upper = attr(x,"SHOW.CI.UPPER")
  show.ci.lower = attr(x,"SHOW.CI.LOWER")
  show.p.norm = attr(x,"SHOW.P.NORM")
  show.p.norm.exact = attr(x,"SHOW.P.NORM.EXACT")
  show.nor.test = attr(x,"SHOW.NOR.TEST")
  show.is.normal = attr(x,"SHOW.IS.NORMAL")
  show.global = attr(x,"SHOW.GLOBAL")

  #............................ first caption and global......
  caption = ""
  cont = 1
  for (vn in unique(x$var.name)) {
    if(cont == 1) caption = vn
    else caption <- paste(caption, ifelse(cont == unique(x$var.name),"&",","), vn)
    cont = cont + 1
  }
  if(!is.null(x$group.var)) {
    if(show.global) {
      print(attr(x,"GLOBAL"))
    }
    caption <- paste(caption, "by")
    cont = 1
    for (vn in unique(x$group.var)) {
      if(cont == 1) caption <- paste(caption, vn)
      else caption <- paste(caption, ifelse(cont == length(x$var.name),"&",","), vn)
      cont = cont + 1
    }

  }

  #......................... Limiting columns and rounding .........

  x$mean = round(x$mean, digits=decimals)
  x$sd = round(x$sd, digits=decimals)
  x$IQR = round(x$IQR, digits=decimals)
  x$se = round(x$se, digits=decimals)
  x$ci.upper = round(x$ci.upper, digits=decimals)
  x$ci.lower = round(x$ci.lower, digits=decimals)
  x$p.norm = round(x$p.norm.exact, digits=decimals)

  if(!show.var.name) x$var.name <- NULL
  if(!show.group.var) x$group.var <- NULL
  if(!show.group) x$group <- NULL
  if(!show.n.valid) x$n.valid <- NULL
  if(!show.n.missing) x$n.missing <- NULL
  if(!show.min) x$min <- NULL
  if(!show.max) x$max <- NULL
  if(!show.mean) x$mean <- NULL
  if(!show.sd) x$sd <- NULL
  if(!show.median) x$median <- NULL
  if(!show.IQR) x$IQR <- NULL
  if(!show.se) x$se <- NULL
  if(!show.ci.upper) x$ci.upper <- NULL
  if(!show.ci.lower) x$ci.lower <- NULL
  if(!show.p.norm) x$p.norm <- NULL
  if(!show.p.norm.exact) x$p.norm.exact <- NULL
  if(!show.nor.test) x$nor.test <- NULL
  if(!show.is.normal) x$is.normal <- NULL





  x <- feR:::.clean.table.var.names(x)
  print(knitr::kable(x, caption = paste("Mean of",caption)))
}


.means.PRUEBAS <- function() {
  data("ToothGrowth")

  #................................................................. ERRORES.. STUDENT
  testthat::test_that(
    "Checking errors in means",
    {
      testthat::expect_error(feR::means(stop.on.error = T, lang = "en"),feR:::.error.msg("MISSING_X", lang="en"))
      testthat::expect_error(feR::means(as.character(ToothGrowth$len), stop.on.error = T, lang = "en"),feR:::.error.msg("MISSING_X", lang="en"))
    }
  )
}
