#-----------------------------------------------------------------------
# There are many functions in wich guessing if something is a factor is usefull
# therefore this should act as a bridge.
.guess.factor <- function(x,..., DEBUG=FALSE, guess.factor = TRUE, max.factor.cat= 10){
  if(DEBUG) cat("\n[.guess.factor] ->",class(x),"\n")

  args <- list(...)

  if(class(x) != "data.frame" & !is.factor(x)){
    if(guess.factor) {
      total.cat <- length(unique(x))
      length.limit <- (length(x)*.1)

      if(DEBUG) cat("\n[.guess.factor] Guess factor ->",guess.factor,
                    "\n max.factor =", max.factor.cat,
                    "\n total.cat =", total.cat,
                    "\n length.limit =", length.limit,
                    "\n")
      if((total.cat <= max.factor.cat) & (total.cat < length.limit)) {
        #.... if we have a numeric variable with less than max.factor.cat different values
        #     AND there are less than 10% of the length of the vector in different values
        #     then we can guess that it's a factor
        x <- factor(x)
        if(DEBUG) cat("\n[.guess.factor] -> Factor guess: IS FACTOR =>",class(x),"\n")
      }
    }

  }

  return(x)
}
