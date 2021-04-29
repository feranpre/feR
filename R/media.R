#' Title
#'
#' @param ...                   data/variables to work with
#' @param by					          default value: NULL
#' @param decimals				      default value: 2
#' @param DEBUG					        default value: FALSE
#' @param show.vars				      default value: TRUE
#' @param show.by				        default value: TRUE
#' @param show.groups			      default value: TRUE
#' @param show.n.valid			    default value: TRUE
#' @param show.n.missing		    default value: TRUE
#' @param show.min				      default value: TRUE
#' @param show.max				      default value: TRUE
#' @param show.mean				      default value: TRUE
#' @param show.sd				        default value: TRUE
#' @param show.median			      default value: TRUE
#' @param show.IRQ				      default value: TRUE
#' @param show.p.norm			      default value: TRUE
#' @param show.p.norm.exact		  default value: FALSE
#' @param show.nor.test			    default value: TRUE
#' @param show.is.normal		    default value: TRUE
#' @param show.interpretation	  default value: TRUE
#' @param lang					        default value: "es"
#' @param show.global			      default value: TRUE
#'
#' @return
#'
#' @examples
media <- function(...,by=NULL,decimals=2,DEBUG=FALSE,show.vars=TRUE,
                  show.by=TRUE,show.groups=TRUE,show.n.valid=TRUE,
                  show.n.missing=TRUE,show.min=TRUE,show.max=TRUE,
                  show.mean=TRUE,show.sd=TRUE,show.median=TRUE,
                  show.IRQ=TRUE,show.p.norm=TRUE,show.p.norm.exact=FALSE,
                  show.nor.test=TRUE,show.is.normal=TRUE,
                  show.interpretation=TRUE,lang="es",show.global=TRUE){

  library("dplyr", quietly = TRUE)
  library("nortest", quietly = TRUE)

  FORMA.DATOS <- forma.datos(..., by = by, DEBUG = FALSE, DEBUG.CALL = FALSE)
  HAS.BY <- attr(FORMA.DATOS, "HAS.BY")
  DATOS <- attr(FORMA.DATOS, "DATA")
  if (DEBUG) {
    cat("\n[media] DATOS:\n")
    print(DATOS)
    cat("\n")
  }
  # print(FORMA.DATOS)DATOS
  if (!HAS.BY | show.global == TRUE) {
    for(var in names(DATOS)) {
      var.values <- DATOS %>% pull(var)
      temp.mean <- .feR.media(var.values, var,decimals = decimals)
      if (is.data.frame(temp.mean)){
        if (!HAS.BY) res.global <- cbind(var = var, temp.mean)
        else res.global <- cbind(var = var, by = "-", group = "-", temp.mean)
        if (!exists("result.global")) result.global <- res.global
        else result.global <- rbind(result.global, res.global)
      }
    }
  }


  if (HAS.BY) {
    BY.DATA <- attr(FORMA.DATOS, "BY.DATA")
    if (DEBUG) {
      cat("\n[media] BY.DATA:\n")
      print(BY.DATA)
      cat("\n")
    }
    for(var in names(DATOS)) {
      if (DEBUG) cat("\n[media] VAR:",var, "\n")
      for(by.var in names(BY.DATA)) {
        if (DEBUG) cat("\n[media] BY.VAR:",by.var, "\n")
        by.values <- as.factor(BY.DATA %>% pull(by.var))
        for(by.level in levels(by.values)){
          var.values <- DATOS %>% pull(var)
          var.values <- var.values[by.values == by.level]
          temp.mean <- .feR.media(var.values, var, decimals = decimals)
          if (is.data.frame(temp.mean)){
            res <- cbind(var = var, by = by.var, group = by.level, temp.mean)
            if (!exists("result.group")) result.group <- res
            else result.group <- rbind(result.group, res)
          }
        }
      }
    }
  }



    if (exists("result.global")) result.temp <- result.global
    if (exists("result.temp")){
      if (HAS.BY & exists("result.group")) result.temp <- rbind(result.temp, result.group)
    } else if (HAS.BY & exists("result.group")) result.temp <- result.group


  if (!exists("result.temp")) return(NA)
  else {
    if(!show.vars) result.temp$var <- NULL
    if(!show.by) result.temp$by <- NULL
    if(!show.groups) result.temp$groups <- NULL
    if(!show.n.valid) result.temp$n.valid <- NULL
    if(!show.n.missing) result.temp$n.missing <- NULL
    if(!show.min) result.temp$min <- NULL
    if(!show.max) result.temp$max <- NULL
    if(!show.mean) result.temp$mean <- NULL
    if(!show.sd) result.temp$sd <- NULL
    if(!show.median) result.temp$median <- NULL
    if(!show.IRQ) result.temp$IRQ <- NULL
    if(!show.p.norm) result.temp$p.norm <- NULL
    if(!show.p.norm.exact) result.temp$p.norm.exact <- NULL
    if(!show.nor.test) result.temp$nor.test <- NULL
    if(!show.is.normal) result.temp$is.normal <- NULL

    attr(result.temp, "SHOW.INTERPRETATION") <- show.interpretation
    if(exists("result.global")) attr(result.temp, "RESULT.GLOBAL") <- result.global
    if(exists("result.group")) attr(result.temp, "RESULT.GROUP") <- result.group
    if(show.interpretation) attr(result.temp, "INTERPRETATION") <- .media.interpretation(result.temp, lang = lang, code = "HELP")

    class(result.temp) <- append("feR.media",class(result.temp))
    return(result.temp)
  }
}


#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
print.feR.media <- function(x) {
  res.global <- attr(x,"RESULT.GLOBAL")
  res.grupo <- attr(x, "RESULT.GROUP")
  show.interpretation <- attr(x,"SHOW.INTERPRETATION")
  interp <- attr(x, "INTERPRETATION")

  if (show.interpretation) cat("\n",interp,"\n")
  if (exists("res.global") & !is.null(res.global)) print(knitr::kable(res.global))
  if (exists("res.grupo") & !is.null(res.grupo)) print(knitr::kable(res.grupo))

}

.media.interpretation <- function(x, lang="es", code = ""){
  if (lang == "es") {
    if (code == "HELP") {
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
      \nSi la variable ES normal la describimos como:
      \nLa edad media de los participantes era de XX años con una desviación estandar (DE) de xxx.
      \n
      \nSi la variable NO es normal la describimos como:
      \nLa mediana de edad de los participantes era XXX años y su rango intercuartílico es igual a xxxx
      \n"
    }
  }
  else {
    if (code == "HELP") {
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
      \n
      \nThe the variable follows a normal distribution we can describe it as:
      \nThe mean age for the participant was XXX years with a stardard deviation (SD) of xxx.
      \n
      \nIf the varible doesn't follow a normal distribution, we use median:
      \nThe median age for the participants was XXX years, with an interquanntile range of XXXX.
      \n"
    }
  }
  if (exists("text")) return(text)
}

.feR.media <- function(x, x.name, decimals = 2, p.value = 0.05) {
  # require("nortest")

  if (!is.numeric(x)) {
    warning(paste0("[.feR.media] ERROR - Variable ",x.name," is not numeric"), call. = FALSE)
    return(NA)
  }

  n.valid = length(x) - sum(is.na(x))
  n.missing = sum(is.na(x))
  min = min(x, na.rm = TRUE)
  max = max(x, na.rm = TRUE)
  mean = round(mean(x, na.rm = TRUE), digits = decimals)
  sd = round(sd(x, na.rm = TRUE), digits = decimals)
  median = median(x, na.rm = TRUE)
  IQR = IQR(x, na.rm = TRUE)
  if (n.valid > 3 & n.valid < 5000) {
    p.norm.exact = shapiro.test(x)$p.value
    nor.test = "SW"
  }
  else if (n.valid > 4) {
    p.norm.exact = nortest::lillie.test(x)$p.value
    nor.test = "Lillie (KS)"
  } else {
    p.norm.exact = ks.test(x, "pnorm")$p.value
    nor.test = "KS"
  }


  is.normal = p.norm.exact > p.value
  small.p <- 10^((decimals+1)*-1)
  if(p.norm.exact <= small.p) p.norm <- paste0(" <",small.p)
  else p.norm <- round(p.norm.exact, digits = decimals+1)
  result <- data.frame("n.valid" = n.valid,
                       "n.missing" = n.missing,
                       "min" = min,
                       "max" = max,
                       "mean" = mean,
                       "sd" = sd,
                       "median" = median,
                       "IQR" = IQR,
                       "p.norm" = p.norm,
                       "p.norm.exact" = p.norm.exact,
                       "nor.test" = nor.test,
                       "is.normal" =  is.normal)
  class(result) <- append("udaic",class(result))

  return(result)
}


