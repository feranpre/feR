#' find_col
#'
#'
#' @param data              data.frame used
#' @param string            quoted string to search for (doesn't need wildcards) "dat" will look for "data" as well as "date"
#' @param show.col.number   sometimes it's useful to know the number of the columns found, this is where you ask for it
#'
#' @return list of names that contains the string with or without the column number besides it
#' @export
#'
#' @aliases busca_col
#'
#' @examples
find_col <- function(data, string="", show.col.number= FALSE) {
  if (!show.col.number) return(names(data)[grepl(string,names(data), ignore.case=TRUE)]  )
  return(grep(string,names(data), ignore.case=TRUE))
}

busca_col <- find_col
