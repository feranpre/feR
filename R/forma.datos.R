

#' Title
#'
#' @param ...
#' @param by
#' @param DEBUG
#' @param DEBUG.CALL
#'
#' @return
#' @export
#'
#' @examples
forma.datos <- function(..., by = NULL, DEBUG = FALSE, DEBUG.CALL = FALSE) {
  parametros <- list(...)
  CALL_DETAILS = feR:::.get.call.details()
  CALL_DETAILS.UNNAMED = attr(CALL_DETAILS, "LIST.UNNAMED.ARGS")

  if (DEBUG.CALL){
    cat("\n ------------------- CALL ----------------------- \n")
    print(CALL_DETAILS)
    cat("\n ------------------------------------------------ \n")
  }

  # TEMP.DATA <- data.frame()
  max.length = 0
  for(p in parametros) {

    if (is.data.frame(p)) temp.length = nrow(p)
    else temp.length = length(p)
    if (temp.length > max.length) max.length <- temp.length
  }
  # p.lengths <- lapply(parametros, length)
  # max.length= max(unlist(p.lengths))
  if(DEBUG) cat("\n\n[forma.datos] max.lengt:", max.length,"\n")

  #=================================================================== FORMING PARAMETERS -> DATA.FRAME
  param.number = 0
  for(p in parametros) {
    param.number = param.number + 1
    #----------------------------------------- parameter IS a data.frame
    if (is.data.frame(p)) {
      if (DEBUG) cat("\n[forma.datos] data.frame","\n")
      if (!exists("RAW.DATA")) RAW.DATA <- p
      else {
        if (DEBUG) cat("\n[forma.datos] data.frame -> RAW.DATA exists","\n")
        if (nrow(RAW.DATA) == nrow(p)) RAW.DATA <- cbind(RAW.DATA, p)
        else print("ERROR")
      }
    }
    #----------------------------------------- parameter IS NOT a data.frame
    else {
      if (DEBUG) cat("\n[forma.datos] CLASS: ", class(p)," P:",p," LENGTH:", length(p),"\n")
      len.p <- length(p)

      if (len.p == max.length) {
        # must be data
        if (DEBUG) cat("\n[forma.datos] len == max.length\n")
        if(!exists("TEMP.DATA")) TEMP.DATA <- as.data.frame(p)
        else TEMP.DATA <- cbind(TEMP.DATA, p)
        names(TEMP.DATA)[ncol(TEMP.DATA)] <- CALL_DETAILS.UNNAMED[param.number]
      }
      else if (len.p >= 1) {
        # cant be data, must be vars names

        if (DEBUG) cat("\n[forma.datos] len >= 1\n")
        if (exists("RAW.DATA")) {
          vars <- intersect(p,names(RAW.DATA))

          if(length(vars) != length(p)) warning(paste("[forma.datos] Data variables has/ve been specified but was/were not found in the data:", p))
          if(!exists("TEMP.DATA")) {
            TEMP.DATA <- as.data.frame(RAW.DATA[,p])
            names(TEMP.DATA) <- p
          }
          else {
            names.temp.data <- names(TEMP.DATA)
            TEMP.DATA <- cbind(TEMP.DATA, RAW.DATA[,p])
            names(TEMP.DATA) <- c(names.temp.data, p)
          }

        } else {
          # this should be a variable but no data.frame was loaded so far
          # i should check for data.frame regarding of place in the call
          # and THEN check for vars
          #
          # -- FORM RAW.DATA and after that FORM temp.data
          #
          # I leave this note here for now
          if (DEBUG) cat("\n[forma.datos] Variable name was given but no data.frame was detected\n")
        }
      }
    }
  }

  if(!exists("TEMP.DATA") & exists("RAW.DATA")) TEMP.DATA <- RAW.DATA
  if(!exists("RAW.DATA") & exists("TEMP.DATA")) RAW.DATA <- TEMP.DATA
  #=================================================================== FORMING BY -> DATA.FRAME
  if (!missing("by")) {
    if (DEBUG) cat("\n[forma.datos] BY present\n")
    # if (length(by) == max.length) {
    #   if(DEBUG)  cat("\n[forma.datos] BY must be data\n")
    #   BY.DATA <- by
    # }
    # else {
    #
    # }
    for(p in by) {
      #----------------------------------------- BY parameter IS a data.frame
      if (is.data.frame(p)) {
        if (DEBUG) cat("\n[forma.datos] by is data.frame","\n")
        if (!exists("BY.DATA")) BY.DATA <- p
        else {
          if (nrow(BY.DATA) == nrow(p)) BY.DATA <- cbind(BY.DATA, p)
          else print("ERROR")
        }
      }
      #----------------------------------------- BY parameter IS NOT a data.frame
      else {
        if (DEBUG) cat("\n[forma.datos] BY -> CLASS: ", class(p)," P:",p," LENGTH:", length(p),"\n")
        len.p <- length(p)

        if (len.p == max.length) {
          # must be data
          if (DEBUG) cat("\n[forma.datos] BY -> len == max.length\n")
          if(!exists("BY.DATA")) BY.DATA <- as.data.frame(p)
          else BY.DATA <- cbind(BY.DATA, p)
        }
        else if (len.p >= 1) {
          # cant be data, must be vars names

          if (DEBUG) cat("\n[forma.datos] BY -> len >= 1\n")
          vars <- intersect(p,names(RAW.DATA))

          if(length(vars) != length(p)) warning(paste("[forma.datos] by variables has/ve been specified but was/were not found in the data:", p))
          if (is.null(vars)) {
            warning(paste("[forma.datos] by variables has/ve been specified but was/were not found:", p))
          }
          else {
            temp.by.data <- as.data.frame(RAW.DATA[,vars])

            if(!exists("BY.DATA")) {
              BY.DATA <- temp.by.data
              names(BY.DATA) <- vars
            }
            else {
              names.temp.data <- names(BY.DATA)
              BY.DATA <- cbind(BY.DATA, temp.by.data)
              names(BY.DATA) <- c(names.temp.data, vars)
            }
          }
        }
      }
    }
  }


  if (DEBUG) {
    if (exists("TEMP.DATA")){
      cat("\n[forma.datos] TEMP_DATA:\n")
      print(TEMP.DATA[1:5,])
      print(names(TEMP.DATA))
    }
    if (exists("BY.DATA")){
      cat("\n[forma.datos] BY_DATA:\n")
      print(BY.DATA[1:5,])
      print(names(BY.DATA))
    }
  }



  if (exists("BY.DATA")) {
    RESULT.DATA <- cbind(TEMP.DATA, BY.DATA)
    class(RESULT.DATA) <- append("feR.DATA",class(RESULT.DATA))
    attr(RESULT.DATA, "DATA") <- TEMP.DATA
    attr(RESULT.DATA, "HAS.BY") <- TRUE
    attr(RESULT.DATA, "BY.DATA") <- BY.DATA
  }
  else {
    RESULT.DATA <- TEMP.DATA
    class(RESULT.DATA) <- append("feR.DATA",class(RESULT.DATA))
    attr(RESULT.DATA, "DATA") <- TEMP.DATA
    attr(RESULT.DATA, "HAS.BY") <- FALSE
  }
  attr(RESULT.DATA, "ORIGINAL.CALL") <- CALL_DETAILS

  return(RESULT.DATA)
}


