.var.name <- function(xname) {
    if (grepl("$", xname, fixed=TRUE)) {
    xname <- stringr::str_extract(xname, '(?<=\\$).*')
  }
  else if (grepl("c(", xname, fixed=TRUE)) {
    xname <- xname[1:ifelse(length(xname)>=10,10,length(xname))]
  }
  xname <- gsub("\"","",xname)
  return(xname)
}
