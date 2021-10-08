#------------ OBSOLETE ----------------------




.get.call.details <- function(f=NULL){
  origen = 0
  for(num in 1:sys.nframe()){
    origen = num-1
    # print(sys.call(which=num*-1))
    if (is.null(sys.call(which=num*-1))) break
    if (sys.call(which=num*-1) == "eval(ei, envir)") break

  }

  if(missing(f))  ORIGINAL.CALL <- deparse(sys.call(sys.parent(n=origen)), width.cutoff = 500L)
  else ORIGINAL.CALL <- f

  #----- if function call is longer than 500 char will be splited and I don't want it splitted
  if (length(ORIGINAL.CALL) > 1) {
    for (i in 1:length(ORIGINAL.CALL)) {
      if (!exists("TEMP.CALL")) TEMP.CALL <- ORIGINAL.CALL[i]
      else TEMP.CALL <- paste0(TEMP.CALL, ORIGINAL.CALL[i])
    }
    ORIGINAL.CALL <- TEMP.CALL
    rm(list="TEMP.CALL")
  }

  temp.call <- strsplit(ORIGINAL.CALL,"\\(")[[1]]
  FUNC.NAME <- temp.call[1]

  # cat("\n\nFUNCTION -> ", FUNC.NAME,"\n")
  if (length(temp.call) <= 1) return(FUNC.NAME) #---------------------- create function.call object
  temp.args <- stringr::str_extract(ORIGINAL.CALL, '(?<=\\().*(?=\\))')
  # print(ORIGINAL.CALL)
  # temp.args <- strsplit(temp.args,",")[[1]]
  temp.args <- strsplit(temp.args, ",(?![^()]*\\))", perl = T)[[1]]
  TOTAL.ARGS <- length(temp.args)
  # print(temp.args)
  arguments.list <- list()
  unnamed.arguments.list <- list()
  for(arg.num in 1:TOTAL.ARGS){
    arg.text <- temp.args[arg.num]
    # arg.split <- strsplit(temp.args[arg.num],"=")[[1]]
    arg.name <- stringr::str_extract(arg.text, '.*(?==)') # --- cogemos lo que hay ANTES de = si es que hay algo
    arg.value <- stringr::str_extract(arg.text, '(?<==)(?![^\\(]*\\))(.*)') # --- cogemos lo que hay DETRAS de '=' si hay algo y de paso IGNORAMOS lo que haya dentro de '()', espara evitar confundir 'by=c(asdf=asdf,sadf=asdf)'
    arg.value <- gsub("\"", "", arg.value)
    # print(paste("TEXT: ", arg.text))
    # print(paste("NAME: ", arg.name))
    # print(paste("VALUE: ", arg.value))
    #--- if there is a named argument BOTH must be not NA, else there is no =
   if(length(arg.name) > 0 & length(arg.value)>0) {
       if (is.na(arg.name) | is.na(arg.value)) {
        #-- if any of them are NA we have a not-named argument
        # print("ISNA")
        arg.value <- temp.args[arg.num]
        arg.name = "UNNAMED"
        arg.value <- gsub("\"", "", arg.value)
        #if ther is a vector like c("AGE", "HEIGHT")

        if (grepl("c(",arg.value, fixed = TRUE)) {
          arg.value <- stringr::str_extract(arg.value, '(?<=\\().*(?=\\))')
          arg.value <- strsplit(arg.value, ",",perl = T)[[1]]
        }
        #-- quitamos el simbolo de $ y nos aseguramos de que no se repite el nombre
        if(length(arg.value)>1) {
          arg.value.temp <- arg.value
          arg.value <- NULL
          for(a in arg.value.temp) {
            if (grepl("$", a, fixed=TRUE)) {
              if (exists("arg.value")) arg.value <- c(arg.value, stringr::str_extract(a, '(?<=\\$).*'))
              else arg.value <- stringr::str_extract(a, '(?<=\\$).*')
            }
            else {
              if (exists("arg.value")) arg.value <- c(arg.value, a)
              else arg.value <- a
            }

          }
        }
        else {
          if (grepl("$", arg.value, fixed=TRUE)) arg.value <- stringr::str_extract(arg.value, '(?<=\\$).*')
        }

      }
      arg.name <- gsub(" ","",arg.name)
      arguments.list[[arg.name]] <- c(arguments.list[[arg.name]],gsub(" ","",arg.value))
    }
  }
  # print(arguments.list)
  # attr(ORIGINAL.CALL,"ORIGINAL.CALL") <- FUNC.NAME
  attr(ORIGINAL.CALL,"FUN") <- FUNC.NAME
  attr(ORIGINAL.CALL,"TOTAL.ARGS") <- TOTAL.ARGS
  attr(ORIGINAL.CALL, "LIST.ARGS") <- arguments.list
  attr(ORIGINAL.CALL, "LIST.UNNAMED.ARGS") <- arguments.list$UNNAMED
  class(ORIGINAL.CALL) <- "udaicR.function.parse"

  return(ORIGINAL.CALL)
}


