#-----------------------------------------------------------------------
# There are many functions in wich guessing if something is a factor is usefull
# therefore this should act as a bridge.
.guess.factor <- function(x,..., DEBUG=FALSE){
  if(DEBUG) cat("\n[.describe] ->",class(x),"\n")

  args <- list(...)

  if(class(x) != "data.frame" & !is.factor(x)){
    if(args[["guess.factor"]]) {
      max.factor.cat <- args[["max.factor.cat"]]
      total.cat <- length(unique(x))
      length.limit <- (length(x)*.1)

      if(DEBUG) cat("\n[.describe] Guess factor ->",args[["guess.factor"]],
                    "\n max.factor =", max.factor.cat,
                    "\n total.cat =", total.cat,
                    "\n length.limit =", length.limit,
                    "\n")
      if((total.cat <= max.factor.cat) & (total.cat < length.limit)) {
        #.... if we have a numeric variable with less than max.factor.cat different values
        #     AND there are less than 10% of the length of the vector in different values
        #     then we can guess that it's a factor
        x <- factor(x)
        if(DEBUG) cat("\n[.describe] -> Factor guess: IS FACTOR =>",class(x),"\n")
      }
    }

  }

  return(x)
}
