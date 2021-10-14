#' comp.means
#'
#' @export
#'
comp.means <- function(x,y=NULL, ..., xname=  feR:::.var.name(deparse(substitute(x))),
                       by=NULL, byname = feR:::.var.name(deparse(substitute(by))),
                       DEBUG=FALSE,
                       paired = FALSE,
                       p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001, ci = 0.95,
                       show.descriptives=TRUE,
                       show.variance.test = TRUE,
                       show.comp.test = TRUE,
                       show.df = TRUE,
                       show.stat.name = TRUE,
                       show.stat.value = TRUE,
                       show.stat.ci = TRUE,
                       show.p.value.exact = TRUE,
                       show.p.value = TRUE,
                       show.p.symbols = TRUE,
                       show.estimate = TRUE,
                       decimals=2,
                       lang="es",
                       stop.on.error = TRUE){

  if(missing(x)) {
    feR:::.error.msg(er="MEAN_COMP_X_MISSING", lang=lang, stop.on.error = stop.on.error)
    return(NA)
  }

  if(paired){
    if (!is.data.frame(x) & missing(y)) {
      feR:::.error.msg(er="MEAN_COMP_Y_MISSING", lang=lang, stop.on.error = stop.on.error)
      return(NA)
    }
    if (is.data.fram(x) & missing(y)) {
      if(ncol(x) <2 | ncol(x)>2) {
        feR:::.error.msg(er="MEAN_COMP_PAIRED_MUST_BE_2", lang=lang, stop.on.error = stop.on.error)
        return(NA)
      }
    }
  }
  else if (missing(by)) {
    feR:::.error.msg(er="MEAN_COMP_BY_MISSING", lang=lang, stop.on.error = stop.on.error)
    return(NA)
  }



  UseMethod("comp.means")

}

#' comp.means.default
#'
#' @export
comp.means.default <- function(x, ..., xname=feR:::.var.name(deparse(substitute(x))),
                               by=NULL, byname = feR:::.var.name(deparse(substitute(by))),
                               DEBUG=FALSE,
                               paired = FALSE,
                               show.descriptives = TRUE,
                               show.variance.test = TRUE,
                               show.comp.test = TRUE,
                               show.df = TRUE,
                               show.stat.name = TRUE,
                               show.stat.value = TRUE,
                               show.stat.ci = TRUE,
                               show.p.value.exact = TRUE,
                               show.p.value = TRUE,
                               show.p.symbols = TRUE,
                               show.estimate = TRUE,
                               p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001, ci = 0.95,
                               decimals=2,
                               lang = "es",
                               stop.on.error = TRUE) {
      if(DEBUG) cat("\n[comp.menans.default] A call for mean comparison was made with a variable that is not numeric")
}



#' comp.means.numeric
#'
#' @export
comp.means.numeric <- function(x,y=NULL, ..., xname=feR:::.var.name(deparse(substitute(x))),
                       by=NULL, byname = feR:::.var.name(deparse(substitute(by))),
                       DEBUG=FALSE,
                       paired = FALSE,
                       show.descriptives = TRUE,
                       show.variance.test = TRUE,
                       show.comp.test = TRUE,
                       show.df = TRUE,
                       show.stat.name = TRUE,
                       show.stat.value = TRUE,
                       show.stat.ci = TRUE,
                       show.p.value.exact = TRUE,
                       show.p.value = TRUE,
                       show.p.symbols = TRUE,
                       show.estimate = TRUE,
                       p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001, ci = 0.95,
                       decimals=2,
                       stop.on.error = TRUE, lang = "es"){

  if(DEBUG) cat("\n[comp.means.numeric] START...\n")

  if(missing(by)) {
    feR:::.error.msg(er="MEAN_COMP_BY_MISSING", lang=lang, stop.on.error = stop.on.error)
    return(NA)
  }
  else {

    #............ checking BY options, that must be:
    #................... data.frame
    #................... vector (factor or otherwise)
    if(!is.data.frame(by)) by <- as.data.frame(by) #... if its a vector


    if(is.data.frame(by)) {
        for (by.var in names(by)){
          by.value <- dplyr::pull(by,by.var)
          if(!is.factor(by.value)) by.value <- as.factor(by.value)
          if(length(levels(by.value))==2) {
            res <- feR:::.comp.means.2.groups(x=x, xname=xname, by = by.value, byname = by.var, DEBUG = DEBUG,
                                     p.sig = p.sig, p.sig.small = p.sig.small,
                                     p.sig.very.small = p.sig.very.small, ci = ci)
          }
          if (!exists("result")) result <- res
          else result <- feR:::.comp.means.form.result(result, res)
        }
    }
    else {
      feR:::.error.msg(er="MEAN_COMP_BY_ERROR", lang=lang, stop.on.error = stop.on.error)
      return(NA)
    }

  }

  if(exists("result")) {
    result <- feR:::.comp.means.form.result(result,show.descriptives=show.descriptives,
                                            show.variance.test = show.variance.test,
                                            show.comp.test=show.comp.test,
                                            show.df=show.df,show.stat.name=show.stat.name,
                                            show.stat.value=show.stat.value,
                                            show.stat.ci=show.stat.ci,
                                            show.p.value.exact=show.p.value.exact,
                                            show.p.value=show.p.value,
                                            show.p.symbols=show.p.symbols,
                                            show.estimate=show.estimate, DEBUG=DEBUG)
    return(result)
  }
  else return(NA)
}



#' comp.means.data.frame
#'
#' @export
comp.means.data.frame <- function(x,y=NULL, ..., xname=feR:::.var.name(deparse(substitute(x))),
                               by=NULL, byname = feR:::.var.name(deparse(substitute(by))),
                               DEBUG=FALSE,
                               show.descriptives=TRUE,
                               show.variance.test = TRUE,
                               show.comp.test = TRUE,
                               show.df = TRUE,
                               show.stat.name = TRUE,
                               show.stat.value = TRUE,
                               show.stat.ci = TRUE,
                               show.p.value.exact = TRUE,
                               show.p.value = TRUE,
                               show.p.symbols = TRUE,
                               show.estimate = TRUE,
                               paired = FALSE, p.sig = 0.05,
                               p.sig.small = 0.01, p.sig.very.small = 0.001, ci = 0.95,
                               decimals=2,
                               lang="es", stop.on.error = TRUE, method = "auto"){

  if(DEBUG) cat("\n[comp.means.data.frame] START...\n")
  if(!paired) {
    for(var in names(x)) {
      if(DEBUG) cat("\n       var: ",var, sep = "")
      var.value <- dplyr::pull(x,var)
      if(is.numeric(var.value)) {

        #.... check by options
        #........ can be:
        #................. vector
        #................. variable names
        #................. data.frame

        by.name = byname

        if(!is.data.frame(by)) {
          if(length(by) == length(var.value)) by.value = by     #... vector
          else if (any(by %in% names(x))) by.value <- x[,by]    #... variable names
        } else by.value <- by                                   #... data.frame

        res <- feR::comp.means(var.value, xname=var, by = by.value, byname = by.name, DEBUG = DEBUG,
                               p.sig = p.sig, p.sig.small = p.sig.small,
                               p.sig.very.small = p.sig.very.small, ci = ci)
        if (!exists("result")) result <- res
        else result <- feR:::.comp.means.form.result(result, res)
      }
    }
  }
  else {
    #................... PAIRED
  }

  if(exists("result")) {
    result <- feR:::.comp.means.form.result(result,show.descriptives=show.descriptives,
                                            show.variance.test = show.variance.test,
                                            show.comp.test=show.comp.test,
                                            show.df=show.df,show.stat.name=show.stat.name,
                                            show.stat.value=show.stat.value,
                                            show.stat.ci=show.stat.ci,
                                            show.p.value.exact=show.p.value.exact,
                                            show.p.value=show.p.value,
                                            show.p.symbols=show.p.symbols,
                                            show.estimate=show.estimate, DEBUG=DEBUG)
    # attr(result,"SHOW.DESCRIPTIVES") <- show.descriptives
    # attr(result, "DEBUG") <- TRUE
    return(result)
  }
  else return(NA)
}


#' comp.means.numeric
#'
.comp.means.2.groups <- function(x,y=NULL, ..., xname=feR:::.var.name(deparse(substitute(x))),
                               by=NULL, byname = feR:::.var.name(deparse(substitute(by))),
                               DEBUG=FALSE,
                               paired = FALSE, p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001, ci = 0.95,
                               lang = "es", stop.on.error = TRUE, method = "auto"){

  #................................ START.... CHECK PARAMETERS
  if(missing(by)) {
    feR:::.error.msg("MEAN_COMP_BY_MISSING", lang = lang, stop.on.error = stop.on.error)
    return(NA)
  }

  if(!is.factor(by)) by <- as.factor(by)
  #................................ END.... CHECK PARAMETERS




  x.mean <- feR::means(x=x, xname=xname, by=by, byname = byname)
  normal <- all(x.mean$is.normal)
  x.test <- x.mean[,c("var.name","group.var","group","n.valid","n.missing")]

  if(method == "Student") {
    .test <- cbind(x.test,feR:::.welch.t.test(x,by=by,ci=ci
                                                            ))
    attr(x.test,"TEST") <- "t.test"
  }
  else if (method == "Welch") {
    .test <- cbind(x.test,feR:::.welch.t.test(x,by=by,ci=ci
                                                            ))
    attr(x.test,"TEST") <- "t.test"
  }
  else if (method == "auto")
  {
    if(normal) {
      x.variance <- feR::variance.test(x=x, xname=xname, by = by, byname = byname, DEBUG = DEBUG,
                                       p.sig = p.sig, p.sig.small = p.sig.small,
                                       p.sig.very.small = p.sig.very.small, ci = ci,
                                       decimals=decimals, ...)

      if(!is.data.frame(x.variance)) homocedasticity = FALSE
      else homocedasticity = x.variance$homocedasticity



      if(homocedasticity) x.test <- cbind(x.test,feR::t.test.welch(x,by=by,ci=ci))
      else x.test <- cbind(x.test,feR::t.test.student(x,by=by,ci=ci))



      attr(x.test,"TEST") <- "t.test"

      #.... tamaño del efecto Cohen's d for Welch t-test
      # https://www.datanovia.com/en/lessons/types-of-t-test/unpaired-t-test/welch-t-test/
      #
      #.... tamaño del efecto Cohen's d for Students t-test
      # https://www.datanovia.com/en/lessons/types-of-t-test/unpaired-t-test/students-t-test/#report

    }
    else {
      #.......... NOT NORMAL
      test <- wilcox.test(x ~ by, conf.int = TRUE, conf.int = conf.int)
      test.name <- "Wilcoxon rank"
      # x.test <- x.mean[,c("var.name","group.var","group","n.valid","n.missing")]
      x.test$test.name <- test.name
      x.test$stat.name <- "W"
      x.test$stat.value <- test$statistic
      x.test$p.value <- test$p.value

      # x.test$p.symbols[test$p.value >= p.sig] <- "-"
      # x.test$p.symbols[test$p.value < p.sig] <- "*"
      # x.test$p.symbols[test$p.value < p.sig.small] <- "**"
      # x.test$p.symbols[test$p.value < p.sig.very.small] <- "***"

      x.test$estimate <- test$estimate
      x.test$estimate.ci.low <- test$conf.int[[1]]
      x.test$estimate.ci.high <- test$conf.int[[2]]

      attr(x.test,"TEST") <- "Wilcoxon-Mann-Whitney"
    }
  } #... end method == "auto"
  x.test$p.symbols[x.test$p.value[1] >= p.sig] <- "-"
  x.test$p.symbols[x.test$p.value[1] < p.sig] <- "*"
  x.test$p.symbols[x.test$p.value[1] < p.sig.small] <- "**"
  x.test$p.symbols[x.test$p.value[1] < p.sig.very.small] <- "***"

  class(x.test) <- append("feR.comp.mean", "data.frame")
  x.mean.part <- x.mean
  names(x.mean.part)[-c(1:4)] <- paste0("desc.", names(x.mean.part)[-c(1:4)])
  x.test.part <- x.test[,-c(1:5)]
  names(x.test.part) <- paste0("comp.", names(x.test.part))

  if(exists("x.variance")) {
    x.variance.part <- x.variance[,-c(1:4)]
    names(x.variance.part) <- paste0("var.test.", names(x.variance.part))
    result <- cbind(x.mean.part, x.variance.part, x.test.part)
    attr(result,"VARIANCE") <- x.variance
  } else result <- cbind(x.mean.part, x.test.part)
  class(result) <- append("feR.comp.mean.total", class(result))
  attr(result,"DESC") <- x.mean
  attr(result,"COMP") <- x.test
  attr(result,"TEST") <- "t.test"
  return(result)
}

#'
#' .comp.means.form.result
#'
#' Merges results and creates the proper attributes
#'
#' If the `res` object is present it merges it with `result`
#' If not pressent it just applies all the attributes to `result`
#'
#'
.comp.means.form.result <- function(result, res=NULL,
                                    DEBUG=FALSE,
                                    show.descriptives=TRUE,
                                    show.variance.test = TRUE,
                                    show.comp.test = TRUE,
                                    show.df = TRUE,
                                    show.stat.name = TRUE,
                                    show.stat.value = TRUE,
                                    show.stat.ci = TRUE,
                                    show.p.value.exact = TRUE,
                                    show.p.value = TRUE,
                                    show.p.symbols = TRUE,
                                    show.estimate = TRUE) {
  if(!is.null(res)){
    if(any(class(result) == "feR.comp.mean.total") &
       any(class(res) == "feR.comp.mean.total")) {

      res.desc <- attr(res,"DESC")
      res.comp <- attr(res, "COMP")
      res.var <- attr(res, "VARIANCE")
      result.desc <- attr(result,"DESC")
      result.comp <- attr(result,"COMP")
      result.var <- attr(result,"VARIANCE")
      result.desc <- rbind(result.desc, res.desc)
      result.comp <- rbind(result.comp, res.comp)
      result.var <- rbind(result.var, res.var)
      result <- rbind(result, res)
      attr(result,"DESC") <- result.desc
      attr(result,"COMP") <- result.comp
      attr(result,"VARIANCE") <- result.var
      return(result)
    } else return(NA)
  } else {
    attr(result,"SHOW.DESCRIPTIVES") <- show.descriptives
    attr(result,"SHOW.VARIANCE.TEST") <- show.variance.test
    attr(result,"SHOW.COMP.TEST") <- show.comp.test
    attr(result,"SHOW.DF") <- show.df
    attr(result,"SHOW.STAT.NAME") <- show.stat.name
    attr(result,"SHOW.STAT.VALUE") <- show.stat.value
    attr(result,"SHOW.STAT.CI") <- show.stat.ci
    attr(result,"SHOW.P.VALUE.EXACT") <- show.p.value.exact
    attr(result,"SHOW.P.VALUE") <- show.p.value
    attr(result,"SHOW.P.SYMBOLS") <- show.p.symbols
    attr(result,"SHOW.ESTIMATE") <- show.estimate
    attr(result,"DEBUG") <- DEBUG
    return(result)
  }


}



#' print.feR.comp.mean.total
#'
#'  Esta función deberá controlar si se imprimen los descriptivos y formato y esas cosas
#'
#' @export
print.feR.comp.mean.total <- function(x) {
    DEBUG <- attr(x,"DEBUG")
    if(attr(x,"SHOW.DESCRIPTIVES")) {
      x.mean <- attr(x,"DESC")
      attr(x.mean,"DEBUG") <- DEBUG
      print(x.mean)
    }

    if(attr(x,"SHOW.VARIANCE.TEST")) {
      x.var <- attr(x,"VARIANCE")
      attr(x.var,"DEBUG") <- DEBUG
      print(x.var)
    }

  x.comp <- attr(x,"COMP")
  attr(x.comp,"DEBUG") <- DEBUG

  attr(x.comp,"SHOW.DESCRIPTIVES") <- attr(x,"SHOW.DESCRIPTIVES")
  attr(x.comp,"SHOW.COMP.TEST") <- attr(x,"SHOW.COMP.TEST")
  attr(x.comp,"SHOW.DF") <- attr(x,"SHOW.DF")
  attr(x.comp,"SHOW.STAT.NAME") <- attr(x,"SHOW.STAT.NAME")
  attr(x.comp,"SHOW.STAT.VALUE") <- attr(x,"SHOW.STAT.VALUE")
  attr(x.comp,"SHOW.STAT.CI") <- attr(x,"SHOW.STAT.CI")
  attr(x.comp,"SHOW.P.VALUE.EXACT") <- attr(x,"SHOW.P.VALUE.EXACT")
  attr(x.comp,"SHOW.P.VALUE") <- attr(x,"SHOW.P.VALUE")
  attr(x.comp,"SHOW.P.SYMBOLS") <- attr(x,"SHOW.P.SYMBOLS")
  attr(x.comp,"SHOW.ESTIMATE") <- attr(x,"SHOW.ESTIMATE")
  print(x.comp)

}


#' print.feR.comp.mean
#'
#'  Esta función deberá controlar si se imprimen los descriptivos y formato y esas cosas
#'
#' @export
print.feR.comp.mean <- function(x) {
  DEBUG <- attr(x,"DEBUG")
  x <- feR:::.clean.table.var.names(x, DEBUG=DEBUG)
  if("comp.test" %in% names(x)) if(attr(x,"SHOW.COMP.TEST") == F) x$comp.test <- NULL
  if("df" %in% names(x)) if(attr(x,"SHOW.DF") == F) x$df <- NULL
  if("stat.name" %in% names(x)) if(attr(x,"SHOW.STAT.NAME") == F) x$stat.name <- NULL
  if("stat.value" %in% names(x)) if(attr(x,"SHOW.STAT.VALUE") == F) x$stat.value <- NULL
  if("stat.ci.low" %in% names(x) & "stat.ci.high" %in% names(x)) if(attr(x,"SHOW.STAT.CI") == F) {
    x$stat.ci.low <- NULL
    x$stat.ci.high <- NULL
  }
  if("p.value.exact" %in% names(x)) if(attr(x,"SHOW.P.VALUE.EXACT") == F) x$p.value.exact <- NULL
  if("p.value" %in% names(x)) if(attr(x,"SHOW.P.VALUE") == F) x$p.value <- NULL
  if("p.symbols" %in% names(x)) if(attr(x,"SHOW.P.SYMBOLS") == F) x$p.symbols <- NULL
  if("estimate" %in% names(x)) if(attr(x,"SHOW.ESTIMATE") == F) x$estimate <- NULL
  print(knitr::kable(x))

}





