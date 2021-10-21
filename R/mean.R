# TODO:
#
# Controlar warning de normalidad (salta por pocas observaciones en KS)
#
# df <- ToothGrowth
# df$supp[1:2]<- NA
# df$len[1:5]<- NA
#
# feR::means(df, by = "supp", stop.on.error = T, lang = "es",  show.global = T)
#
#
#
# TODO:
#
# Terminar los test
#



#' media
#'
#' This function will give a data.frame with the necessary statistics to describe
#' a numeric variable, including selecting the appropriate test to check if the
#' variable follows a normal distribution and offering the possibilitiy of a brief
#' interpretation paragraph to show how to describe the variable and a detailed
#' description of each column in the resulting data.base
#'
#'
#' @param x                   data.frame or vector with data
#' @param ...                 if data.frame was specified variables can be used here to select columns
#' @param xname               optional. Name for 'x' variable
#' @param by                  optional. Variable to group 'x' by and can be:
#'
#'                               - a vector with the same length as 'x'
#'                               - a data.frame
#'                               - if 'x' is a data.frame it can be a string (or vector of strings) with column, names present in 'x'
#'
#'
#' @param byname              optional. Name for 'by' variable
#' @param decimals            optional. Default = 2. Number of decimals to show.
#' @param lang
#'
#' @param show.global optional. Default = TRUE. If there is a 'by' variable defined this will
#'                     prompt the printing of the variable means WITHOUT agrupation too
#' @param p.sig       optional. Default = 0.05. Value below wich a p.value is considered 'statistically significant'
#' @param p.sig.small optional. Default = 0.01. Value below wich a p.value is considered 'statistically significant' and 'small'
#' @param p.sig.very.small optional. Value below wich a p.value is considered 'statistically significant' and 'very small'
#' @param ci optional. Default = 0.95. Value for the Confidence Interval
#'
#' @param show.var.name optional. Default = TRUE. Show var.name column on print
#' @param show.group.var optional. Default = TRUE. Show group.var column on print
#' @param show.group optional. Default = TRUE. Show group column on print
#' @param show.n.valid optional. Default = TRUE. Show n.valid column on print
#' @param show.n.missing optional. Default = TRUE. Show n.missing column on print
#' @param show.min optional. Default = TRUE. Show min column on print
#' @param show.max optional. Default = TRUE. Show max column on print
#' @param show.mean optional. Default = TRUE. Show mean column on print
#' @param show.sd optional. Default = TRUE. Show sd column on print
#' @param show.median optional. Default = TRUE. Show median column on print
#' @param show.IQR optional. Default = TRUE. Show IQR column on print
#' @param show.se optional. Default = TRUE. Show se column on print
#' @param show.ci.upper optional. Default = TRUE. Show ci.upper column on print
#' @param show.ci.lower optional. Default = TRUE. Show ci.lower column on print
#' @param show.p.norm optional. Default = TRUE. Show p.norm column on print
#' @param show.p.norm.exact optional. Default = TRUE. Show p.norm.exact column on print
#' @param show.nor.test optional. Default = TRUE. Show nor.test column on print
#' @param show.is.normal optional. Default = TRUE. Show is.normal column on print
#' @param show.interpretation optional. Default = FALSE Show a text with an EXAMPLE
#'                            to show how the data can be interpreted. This is by no
#'                            means a thorough or complete interpretation. Just a guide.

#' @param comp
#' @param show.post.hoc
#' @param show.desc
#' @param paired
#' @param stop.on.error
#' @param show.error
#' @param DEBUG
#'
#' @export
#'
media <- function(x, ..., xname=  feR:::.var.name(deparse(substitute(x))),
                  by=NULL, byname = feR:::.var.name(deparse(substitute(by))),
                  decimals=2,
                  show.var.name=TRUE,show.group.var=TRUE,show.group=TRUE,show.n.valid=TRUE,
                  show.n.missing=TRUE,show.min=TRUE,show.max=TRUE,
                  show.mean=TRUE,show.sd=TRUE,show.median=TRUE,
                  show.IQR=TRUE, show.se = FALSE, show.ci.upper = FALSE, show.ci.lower = FALSE,
                  show.p.norm=TRUE,show.p.norm.exact=FALSE,
                  show.nor.test=TRUE,show.is.normal=TRUE,
                  show.interpretation=FALSE,lang="es",show.global=TRUE,
                  show.help = FALSE,
                   p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001, ci = 0.95,
                   comp=FALSE,
                   show.post.hoc = TRUE,
                   show.desc=TRUE, paired = T, stop.on.error=FALSE, show.error = TRUE, DEBUG=FALSE){

  if(missing(x)) {
    if(stop.on.error) stop(feR:::.error.msg(er="MISSING_X", lang=lang))
    else if (show.error) message(feR:::.error.msg(er="MISSING_X", lang=lang))
    return(NA)
  }
  if (missing(by) | show.global) x.mean <- feR::means(x=x, ..., xname = xname, decimals = decimals, ci = ci,
                                              DEBUG=DEBUG,stop.on.error=stop.on.error, show.errors = show.errors, lang = lang)

  if(!missing(by)) {
    x.groups <- feR::means(x=x, ..., xname = xname, by = by, byname = byname, decimals = decimals, ci = ci,
                                DEBUG=DEBUG,stop.on.error=stop.on.error, show.errors = show.errors, lang = lang)
    if(show.global) attr(x.groups, "GLOBAL") <- x.mean
    x.mean <- x.groups
  }
  attr(x.mean,"DECIMALS") = decimals
  attr(x.mean,"LANG") = lang
  attr(x.mean,"P.SIG") = p.sig
  attr(x.mean,"P.SIG.SMALL") = p.sig.small
  attr(x.mean,"P.SIG.VERY.SMALL") = p.sig.very.small
  attr(x.mean,"CI") = ci


  attr(x.mean,"SHOW.VAR.NAME") = show.var.name
  attr(x.mean,"SHOW.GROUP.VAR") = show.group.var
  attr(x.mean,"SHOW.GROUP") = show.group
  attr(x.mean,"SHOW.N.VALID") = show.n.valid
  attr(x.mean,"SHOW.N.MISSING") = show.n.missing
  attr(x.mean,"SHOW.MIN") = show.min
  attr(x.mean,"SHOW.MAX") = show.max
  attr(x.mean,"SHOW.MEAN") = show.mean
  attr(x.mean,"SHOW.SD") = show.sd
  attr(x.mean,"SHOW.MEDIAN") = show.median
  attr(x.mean,"SHOW.IQR") = show.IQR
  attr(x.mean,"SHOW.SE") = show.se
  attr(x.mean,"SHOW.CI.UPPER") = show.ci.upper
  attr(x.mean,"SHOW.CI.LOWER") = show.ci.lower
  attr(x.mean,"SHOW.P.NORM") = show.p.norm
  attr(x.mean,"SHOW.P.NORM.EXACT") = show.p.norm.exact
  attr(x.mean,"SHOW.NOR.TEST") = show.nor.test
  attr(x.mean,"SHOW.IS.NORMAL") = show.is.normal
  attr(x.mean,"SHOW.GLOBAL") = show.global
  attr(x.mean,"SHOW.HELP") = show.help

  class(x.mean) <- c("feR.means","data.frame")
  return(x.mean)
}



#' @rdname media
#' @export
m <- media




#' means
#'
#' @export
#'
means <- function(x, ..., xname=  feR:::.var.name(deparse(substitute(x))),
                          by=NULL, byname = feR:::.var.name(deparse(substitute(by))),
                          decimals=2, ci = 0.95,
                          DEBUG=FALSE,stop.on.error=TRUE, show.errors = TRUE, lang = "es"){
  UseMethod("means")
}

#' means.default
#'
#' @exportS3Method
#'
means.default <- function(x, ..., xname=  feR:::.var.name(deparse(substitute(x))),
                          by=NULL, byname = feR:::.var.name(deparse(substitute(by))),
                          decimals=2, ci = 0.95,
                          DEBUG=FALSE,stop.on.error=TRUE, show.errors = TRUE, lang = "es"){

  if (stop.on.error) stop(feR:::.error.msg("MISSING_X", lang=lang))
  else if (show.errors) message(feR:::.error.msg("MISSING_X", lang=lang))
  return(NA)
}


#' means.numeric
#'
#' @exportS3Method
#'
means.numeric <- function(x, ..., xname=  feR:::.var.name(deparse(substitute(x))),
                          by=NULL, byname = feR:::.var.name(deparse(substitute(by))),
                          decimals=2, ci = 0.95,
                          DEBUG=FALSE,stop.on.error=TRUE, show.errors = TRUE, lang = "es"){

  if(DEBUG) cat("\n [means.numeric] START...\n")

  has.groups <- !missing(by)


  if(!has.groups){
    if(DEBUG) cat("\n [means.numeric] No groups detected\n")
    #....................................................... GLOBAL MEAN (or mean if not by present)
    final.res <- feR:::.means(x, decimals=decimals, ci=ci)

    if(!exists("final.res") | (exists("final.res") && !is.data.frame(final.res))) return(NA) #... if globan means could not be calculated there's no reason to follow
    else {
      final.res$var.name = xname
      final.res <- final.res[,c("var.name",names(final.res)[-length(names(final.res))])]
    }
  } else {
    if(DEBUG) cat("\n [means.numeric] Groups detected\n")
    #................... BY can be:
    #                      - factor
    #                      - vector that must be converted to factor
    #                      - data.frame
    #... by is a vector
    if(length(by) == length(x)) {
      if(!is.factor(by)) by.data = as.data.frame(as.factor(by))
      else by.data = as.data.frame(by)
      names(by.data) <- byname
    }
    else if (is.data.frame(by)) by.data <- by



    #.................. split by.value by factors
    for(by.var in names(by.data)){
      by.value <- as.factor(dplyr::pull(by.data, by.var))
      for (level in levels(by.value)){
        res <- feR:::.means(x[by.value == level & !is.na(by.value)], decimals=decimals, ci=ci)
        res$group = level
        if(exists("group.res")) group.res <- rbind(group.res, res)
        else group.res <- res
        # res <- NULL
      }

    }

    if(sum(is.na(by.value))>0) {
      res <- feR:::.means(x[is.na(by.value)], decimals=decimals, ci=ci)
      res$group = "."
      if(exists("group.res")) group.res <- rbind(group.res, res)
      else group.res <- res
    }
    if(exists("group.res") && is.data.frame(group.res)) {
      group.res$var.name = xname
      group.res$group.var = byname
      group.res <- group.res[,c("var.name","group.var","group",names(group.res)[1:(length(names(group.res))-3)])]
    }
    final.res <- group.res
  }


  if(exists("final.res")) return(final.res)
  else return(NA)
}




#' means.data.frame
#'
#' @exportS3Method
#'
means.data.frame <- function(x, ..., xname=  feR:::.var.name(deparse(substitute(x))),
                             by=NULL, byname = feR:::.var.name(deparse(substitute(by))),
                             decimals=2, ci = 0.95,
                             DEBUG=FALSE,stop.on.error=TRUE, show.errors = TRUE, lang = "es"){


  if(DEBUG) cat("\n [means.data.frame] START...\n")
  x.data.frame <- x
  num.var.exists <- FALSE

  has.groups <- !missing(by)
  if(has.groups) {
    if(DEBUG) cat("\n [means.data.frame] Groups found\n")

    #........................... BY can be:
    #                               - factor                - nothing needs to be done
    #                               - vector                - nothing needs to be done
    #                               - data.frame            - nothing needs to be done
    #                               - string with var names - data.frame must be created
    #... is not a vector, lets check for var names
    if(is.data.frame(by)) by.data <- by
    else if(is.factor(by)) by.data <- by
    else if(length(by) == nrow(x.data.frame)) by.data <- by
    else if(any(by %in% names(x.data.frame))) {
        by.names <- names(x.data.frame)[names(x.data.frame) %in% by]
        by.data <- as.data.frame(x.data.frame[,by.names])
        names(by.data) <- by.names
        x.data.frame <- x.data.frame[,names(x.data.frame)[!(names(x.data.frame) %in% by.names)]]
      }
    else {
        e <- feR:::.error.msg("MEAN_BY", lang = lang)
        if(stop.on.error) stop(e)
        else if (show.errors) message(e)
        return(NA)
    }

  }

  #.... checking if there are var names in ...
  if(!missing(...)){
    parametros <- unlist(list(...))
    param.all <- all(parametros %in% names(x.data.frame))
    if(param.all) {
      x.data.frame <- as.data.frame(x.data.frame[,parametros])
      names(x.data.frame) <- parametros
    }
    else {
      if (stop.on.error) stop(feR:::.error.msg("X_VAR_NOT_IN_DF", lang=lang))
      else if (show.errors) message(feR:::.error.msg("X_VAR_NOT_IN_DF", lang=lang))
      return(NA)
    }
  }

  for(var in names(x.data.frame)){
    x <-  dplyr::pull(x.data.frame, var)
    if(is.numeric(x)) {
      if(has.groups)res <- feR::means(x, xname = var,
                              by = by.data, byname = byname,
                              decimals=decimals,ci=ci, DEBUG = DEBUG,
                              stop.on.error = stop.on.error,
                              show.error = show.errors, lang = lang)
      #... no by
      else res <- feR::means(x, xname = var, decimals=decimals,
                             ci=ci,DEBUG=DEBUG, stop.on.error = stop.on.error,
                             show.error = show.errors, lang = lang)

      if(!exists("final.res")) final.res <- res
      else final.res <- rbind(final.res, res)
      num.var.exists <- TRUE
    }
  }

  #.............................. final result
  if(!num.var.exists) {
    e <- feR:::.error.msg("MEAN_NOT_NUMERIC", lang = lang)
    if(stop.on.error) stop(e)
    else if (show.errors) message(e)
    return(NA)
  }
  else if(!exists("final.res")) return(NA)
  else return(final.res)

}



.means <- function(x, p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001, ci = .95, ...) {

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
