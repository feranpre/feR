#'
#' @author Fernando Andres-Pretel
#'




#' print.feR.means
#'
#' Prints a feR.means objecto with the desired output as indicated in the object
#'
#' @param x feR.means object
#'
#' @details
#'
#' Some of the attributes that can be tweaked are:
#' - DECIMALS               number of decimals to show
#' - LANG                   languaje of the text
#' - P.SIG                  level for witch p values will be found to be 'significant'
#' - P.SIG.SMALL            level for witch p values will be found to be 'significant' and 'small'
#' - P.SIG.VERY.SMALL       level for witch p values will be found to be 'significant' and 'very small'
#' - CI                     confidence interval
#' - SHOW.VAR.NAME
#' - SHOW.GROUP.VAR
#' - SHOW.GROUP
#' - SHOW.N.VALID
#' - SHOW.N.MISSING
#' - SHOW.MIN
#' - SHOW.MAX
#' - SHOW.MEAN
#' - SHOW.SD
#' - SHOW.MEDIAN
#' - SHOW.IQR
#' - SHOW.SE
#' - SHOW.CI.UPPER
#' - SHOW.CI.LOWER
#' - SHOW.P.NORM
#' - SHOW.P.NORM.EXACT
#' - SHOW.NOR.TEST
#' - SHOW.IS.NORMAL
#' - SHOW.GLOBAL
#'
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
  show.help = attr(x,"SHOW.HELP")
  # print(show.global)

  if (!is.null(show.help) && show.help) {
    if (lang != "es"){
      text ="
      \nThe mean function returns a data.frame with these columns:
      \n
      \n* n.valid: number of valid (not missing) observations
      \n* n.missing: number of missing observations
      \n* min: minimun
      \n* max: maximun
      \n* mean: mean value
      \n* sd: standard deviation
      \n* median: median
      \n* IQR: interquanntile range
      \n* p.normal: p value for normality test
      \n* nor.test: test used to check normality
      \n  - SW: Shapiro-Wilks
      \n  - Lille (KS): Lilliefor's correction of Kolmogorov-Smirnov
      \n* p.normal.exact: p value for normality test (without rounding)
      \n* is.normal:
      \n  - TRUE -> the variable **IS** normal (follows a normal distribution)
      \n  - FALSE-> the variable is **NOT** normal (doesn't follow a normal distribution)
      \n"
   } else {
     text = "
          \nLas funcion devuelve una tabla con las siguientes columnas:
          \n
          \n* n.valid: cuantas observaciones teneis (valores no perdidos)
          \n* n.missing: cuantas respuestas perdidas teneis
          \n* min: valor mínimo
          \n* max: valor máximo
          \n* mean: media
          \n* sd: desviación estandar
          \n* median: mediana
          \n* IQR: rango intercuartílico
          \n* p.normal: valor p de la prueba de normalidad
          \n* nor.test: prueba usada para la normalidad
          \n  - SW: Shapiro-Wilks
          \n  - Lille (KS): Kolmogorov-Smirnov con la corrección de Lilliefors
          \n* p.normal.exact: valor p de la prueba de normalidad exacto (sin redondeo)
          \n* is.normal:
          \n  - TRUE -> signfica que la variable **ES** normal
          \n  - FALSE-> significa que la variable **NO** es normal
          \n
          \n"
   }
    cat(text)
  }

  if(!is.null(x$group.var) && (show.global) ) {
    global <- attr(x,"GLOBAL")
    attr(global,"DECIMALS") = decimals
    attr(global,"LANG") = lang
    attr(global,"SHOW.VAR.NAME") = show.var.name
    attr(global,"SHOW.GROUP.VAR") = show.group.var
    attr(global,"SHOW.GROUP") = show.group
    attr(global,"SHOW.N.VALID") = show.n.valid
    attr(global,"SHOW.N.MISSING") = show.n.missing
    attr(global,"SHOW.MIN") = show.min
    attr(global,"SHOW.MAX") = show.max
    attr(global,"SHOW.MEAN") = show.mean
    attr(global,"SHOW.SD") = show.sd
    attr(global,"SHOW.MEDIAN") = show.median
    attr(global,"SHOW.IQR") = show.IQR
    attr(global,"SHOW.SE") = show.se
    attr(global,"SHOW.CI.UPPER") = show.ci.upper
    attr(global,"SHOW.CI.LOWER") = show.ci.lower
    attr(global,"SHOW.P.NORM") = show.p.norm
    attr(global,"SHOW.P.NORM.EXACT") = show.p.norm.exact
    attr(global,"SHOW.NOR.TEST") = show.nor.test
    attr(global,"SHOW.IS.NORMAL") = show.is.normal
    attr(global,"SHOW.GLOBAL") = FALSE
    class(global) <- c("feR.means", "data.frame")
    print(global)
  }

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
