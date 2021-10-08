#' means.default
#'

.error.msg <- function(er="DEFAULT", lang = "es", as.text = TRUE) {
  error_msg.es <- c(
    "DEFAULT"="Ha habido un problema inespecífico",
    "MEAN_NOT_NUMERIC" = "Se requiere un vector numérico o un data.frame con al menos una variable numérica"
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



