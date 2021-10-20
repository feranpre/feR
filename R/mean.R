#' TODO:
#'
#' Controlar warning de normalidad (salta por pocas observaciones en KS)
#'
#' df <- ToothGrowth
#' df$supp[1:2]<- NA
#' df$len[1:5]<- NA
#'
#' feR::means(df, by = "supp", stop.on.error = T, lang = "es",  show.global = T)
#'





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
                          show.var.name=TRUE,show.group.var=TRUE,show.group=TRUE,show.n.valid=TRUE,
                          show.n.missing=TRUE,show.min=TRUE,show.max=TRUE,
                          show.mean=TRUE,show.sd=TRUE,show.median=TRUE,
                          show.IQR=TRUE, show.se = FALSE, show.ci.upper = FALSE, show.ci.lower = FALSE,
                          show.p.norm=TRUE,show.p.norm.exact=FALSE,
                          show.nor.test=TRUE,show.is.normal=TRUE,
                          show.interpretation=FALSE,lang="es",show.global=TRUE,
                          p.sig = 0.05, p.sig.small = 0.01, p.sig.very.small = 0.001, ci = 0.95,
                          show.desc=TRUE, paired = F, stop.on.error=TRUE, show.errors = TRUE, DEBUG=FALSE){
  if(DEBUG) cat("\n [means.numeric] START...\n")


  has.groups = FALSE
  if(!missing(by)) has.groups = TRUE

  #....................................................... GLOBAL MEAN (or mean if not by present)
  res.global <- feR:::.means(x, decimals=decimals, p.sig=p.sig,
                        p.sig.small=p.sig.small, p.sig.very.small=p.sig.very.small,
                        ci=ci)
  if(exists("res.global") && is.data.frame(res.global)) {
    res.global$var.name = xname
    res.global <- res.global[,c("var.name",names(res.global)[-length(names(res.global))])]
  }

  #....................................................... GROUPS
  if(has.groups) {
    #................... format and get by.value
    if(!is.factor(by)) by.value = as.factor(by)
    else by.value = by

    #.................. split by.value by factors
    for (level in levels(by.value)){
      res <- feR:::.means(x[by.value == level & !is.na(by.value)], decimals=decimals, p.sig=p.sig,
                    p.sig.small=p.sig.small, p.sig.very.small=p.sig.very.small,
                    ci=ci)
      res$group = level
      if(exists("group.res")) group.res <- rbind(group.res, res)
      else group.res <- res
      # res <- NULL
    }

    if(sum(is.na(by.value))>0) {
      res <- feR:::.means(x[is.na(by.value)], decimals=decimals, p.sig=p.sig,
                    p.sig.small=p.sig.small, p.sig.very.small=p.sig.very.small,
                    ci=ci)
      res$group = "."
      if(exists("group.res")) group.res <- rbind(group.res, res)
      else group.res <- res
    }
    if(exists("group.res") && is.data.frame(group.res)) {
      group.res$var.name = xname
      group.res$group.var = byname
      group.res <- group.res[,c("var.name","group.var","group",names(group.res)[1:(length(names(group.res))-3)])]
    }
  }

  final.res <- res.global
  # class(final.res) <- c("feR.means", class(final.res))
  #......................................... RESULT FORMATION
  if(has.groups) {
    if(exists("group.res")) final.res <- group.res
    else final.res <- NA

    if(show.global && exists("res.global")) attr(final.res, "GLOBAL") <- feR::means(x=x, xname = xname,
                                                                                    decimals=decimals, p.sig=p.sig,
                                                                                    p.sig.small=p.sig.small, p.sig.very.small=p.sig.very.small,
                                                                                    ci=ci,show.var.name = show.var.name,
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
                                                                                    show.global = show.global,
                                                                                    lang=lang, stop.on.error = stop.on.error,
                                                                                    show.error = show.errors)
  }

  attr(final.res,"DECIMALS") <- decimals
  attr(final.res,"LANG") <- lang
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
  num.var.exists <- FALSE

  #.... checking if there is a by and if its a vector or a data.frame
  has.groups <- !missing(by)
  if(has.groups) {
    #... by is a vector
    if(length(by) == nrow(x.data.frame)) {
      by.data = as.data.frame(by)
      names(by.data) <- byname
    }
    #... is not a vector, lets check for var names
    else {
      if(any(by %in% names(x.data.frame))) {
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
  }

  #.... checking if there are var names in ...
  if(!missing(...)){
    parametros <- unlist(list(...))

    param.all <- all(parametros %in% names(x.data.frame))
    if(param.all) {
      x.data.frame <- x.data.frame[,parametros]
    }
  }

  for(var in names(x.data.frame)){
    x <-  dplyr::pull(x.data.frame, var)
    if(is.numeric(x)) {
      if(has.groups){
            for(by.name in names(by.data)) {

              by.value <- dplyr::pull(by.data, by.name)
              by.res <- feR::means(x, xname = var,
                                   by = by.value, byname = feR:::.var.name(by.name),
                                   decimals=decimals, p.sig=p.sig,
                                   p.sig.small=p.sig.small, p.sig.very.small=p.sig.very.small,
                                   ci=ci,show.var.name = show.var.name,
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
                                   show.global = show.global,
                                   lang=lang, stop.on.error = stop.on.error,
                                   show.error = show.errors)
              if(!exists("res")) res <- by.res
              else res <- rbind(res, by.res)

            }
            # if(show.global) attr(res,"GLOBAL") <- eR::means(x, xname = var, show.global = show.global)
      }
      #... no by
      else res <- feR::means(x, xname = var, decimals=decimals, p.sig=p.sig,
                             p.sig.small=p.sig.small, p.sig.very.small=p.sig.very.small,
                             ci=ci,show.var.name = show.var.name,
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
                             show.global = show.global,
                             lang=lang, stop.on.error = stop.on.error,
                             show.error = show.errors)

      if(!exists("final.res")) final.res <- res
      else {
        final.res <- rbind(final.res, res)
        if(!is.null(attr(final.res,"GLOBAL"))) attr(final.res,"GLOBAL")[[length(attr(final.res,"GLOBAL"))]] <- attr(res,"GLOBAL")
      }
      res <- NULL
      num.var.exists <- TRUE
    }
  }

  if(!num.var.exists) {
    e <- feR:::.error.msg("MEAN_NOT_NUMERIC", lang = lang)
    if(stop.on.error) stop(e)
    else if (show.errors) message(e)
    return(NA)
  }
  else {
    #.................................... at least there is one numeric variable
    if(!exists("final.res")) return(NA)
    else {
      attr(final.res,"DECIMALS") <- decimals
      attr(final.res,"LANG") <- lang

      # class(final.res) <- c("feR.means", class(final.res))
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
  }
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
  lang = attr(x,"LANG")

  caption.y <- ifelse(lang== "es"," y ", " & ")
  caption.by <- ifelse(lang== "es"," en función de ", " by ")
  caption.mean <- ifelse(lang == "es","Media de", "Mean of")

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


  if(!is.null(x$group.var) && (show.global) )   print(attr(x,"GLOBAL"))

  #............................ first caption and global......

  cont = 1
  for (vn in unique(x$var.name)) {
    if(cont == 1) caption = vn
    else caption <- paste0(caption, ifelse(cont == length(unique(x$var.name)),caption.y,", "), vn)
    cont = cont + 1
  }

  if(!is.null(x$group.var)) {
    caption <- paste0(caption, caption.by)
    cont = 1
    for (vn in unique(x$group.var)) {
      if(cont == 1) caption <- paste0(caption, vn)
      else caption <- paste0(caption, ifelse(cont == length(x$var.name),caption.y,", "), vn)
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
  if(lang == "es") {
    names(x)[names(x) == "var.name"] <- "var"
    names(x)[names(x) == "group.var"] <- "var.grupo"
    names(x)[names(x) == "group"] <- "grupos"
    names(x)[names(x) == "n.valid"] <- "n.validos"
    names(x)[names(x) == "n.missing"] <- "n.perdidos"
    names(x)[names(x) == "min"] <- "min"
    names(x)[names(x) == "max"] <- "max"
    names(x)[names(x) == "mean"] <- "media"
    names(x)[names(x) == "sd"] <- "desv.est"
    names(x)[names(x) == "median"] <- "mediana"
    names(x)[names(x) == "IQR"] <- "RIQ"
    names(x)[names(x) == "p.norm"] <- "p.norm"
    names(x)[names(x) == "nor.test "] <- "test.norm"
    names(x)[names(x) == "is.normal"] <- "es.normal"
  }
  print(knitr::kable(x, caption = paste(caption.mean,caption)))
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
