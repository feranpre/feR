#' means.default
#'

.error.msg <- function(er="DEFAULT", lang = "es", as.text = TRUE) {
  error_msg.es <- c(
    "DEFAULT"="Ha habido un problema inespecífico",
    "MEAN_NOT_NUMERIC" = "Se requiere un vector numérico o un data.frame con al menos una variable numérica",
    "MEAN_COMP_X_MISSING" = "Falta vector de datos numéricos 'X' para comparación de medias",
    "MEAN_COMP_Y_MISSING" = "Falta vector de datos numéricos 'Y' para comparación de medias",
    "MEAN_COMP_BY_MISSING" = "Falta factor de agrupacion para comparación de medias"
  )

  if(lang == "es") {
    if (!is.na(error_msg.es[er])) msg <- error_msg.es[er]
    else if(!is.na(error_msg.en[er])) msg <- error_msg.en[er]
    else msg <- "Error"
  } else {
    if(!is.na(error_msg.en[er])) msg <- error_msg.en[er]
    else msg <- "Error"
  }

  if(as.text) cat("\n[ERROR]: ", msg,"\n")
  else stop(paste0("\n[ERROR]: ", msg,"\n"))

}


.var.name <- function(xname) {
    if (grepl("$", xname, fixed=TRUE)) {
    xname <- stringr::str_extract(xname, '(?<=\\$).*')
  }
  else if (grepl("c(", xname, fixed=TRUE)) {
    xname <- xname[1:ifelse(length(xname)>=10,10,length(xname))]
  }
  return(xname)
}


