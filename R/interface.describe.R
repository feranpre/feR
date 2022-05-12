# ..........................
# Need to solve the by part
#
# here?
#
# different function?
#
# ..........................


#' @export
describe <- function(x, ...,
                     xname=  feR:::.var.name(deparse(substitute(x))),
                     by = NULL,
                     byname =  feR:::.var.name(deparse(substitute(by))),
                     decimals = 4,
                     guess.factor = TRUE,
                     max.factor.cat = 10,
                     na.rm = TRUE,
                     ci = 0.95,
                     DEBUG = FALSE) {

  args <- formals(describe)
  args$... <- NULL

  passed.args <- as.list(match.call()[-1])
  final.args <- as.list(modifyList(args, passed.args))


  do.call(".describe",final.args)
}

#' @export
.describe <- function(x,..., DEBUG=FALSE){
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
        # UseMethod(".describe",a)

      }
    }

  }

  UseMethod(".describe",x)
}



#' @export
.describe.data.frame <- function(x,...) {

  args=list(...)
  results <- list()
  # result.num <- data.frame()
  # result.cat <- data.frame()
  for(var.name in names(x)){
    args$x <- x[,var.name]
    args$xname <- var.name
    var <- do.call(".describe",args)
    results[[var.name]] <- var

  }
  class(results) <- c("feR_describe_data_frame",class(results))
  return(results)
}



#' @export
.describe.numeric <- function(x,...,
                              decimals = 4,
                              ci = 0.95,
                              na.rm = TRUE,
                              DEBUG = FALSE) {
  if(DEBUG) cat("\n[.describe.numeric] Called\n")
  args <- list(...)

  var.name = args[["xname"]]
  if(DEBUG) cat("\n[.describe.numeric] var.name =",var.name,"\n x =",x,"\n")
  n.missing = sum(is.na(x))
  n.valid = length(x) - n.missing
  x.normal = feR:::normal.test(x, n.valid=n.valid, decimals = decimals)
  is.x.normal = ifelse(x.normal$is.normal == TRUE & !is.na(x.normal$is.normal),TRUE,FALSE)
  alpha_2 <- ci+((1-ci)/2) #... alpha halves for confidence interval



  min = ifelse(n.valid > 1, min(x, na.rm = na.rm), NA)
  max = ifelse(n.valid > 1, max(x, na.rm = na.rm), NA)
  mean = ifelse(n.valid > 1, mean(x, na.rm = na.rm), NA)
  sd = ifelse(n.valid > 1, sd(x, na.rm = na.rm), NA)
  median = ifelse(n.valid > 1, median(x, na.rm = na.rm), NA)
  IQR = ifelse(n.valid > 1, IQR(x, na.rm = na.rm), NA)
  se <- ifelse(n.valid > 1, sd(x, na.rm = na.rm)/sqrt(n.valid), NA)
  if (n.valid > 1) {

    if(is.x.normal){
      error <- qnorm(alpha_2)*se
    } else {
      error <- qt(alpha_2, df=n.valid -1)*se
    }
    ci.upper <- mean + error
    ci.lower <- mean - error
  } else {
    ci.upper = NA
    ci.lower = NA
  }

  result <- data.frame(stat = c("n.valid","n.missing",
                                "min","max","mean",
                                "ci.upper","ci.lower",
                                "sd","se","median","IQR",
                                "p.norm.exact"),
                       stat.value = c(n.valid,n.missing,
                                 round(c(min,max,mean,ci.upper,ci.lower,
                                         sd,se,median,IQR,
                                         x.normal$p.exact.value), digits = decimals)
                                 )
  )
  class(result) <- c("feR_describe_numeric",class(result))
  attr(result,"var.name") <- var.name
  attr(result,"nor.test") <- x.normal$test
  attr(result, "is.normal") <- x.normal$is.normal
  attr(result, "p.norm") <- x.normal$p.value
  return(result)
}


#' @export
.describe.factor <- function(x,..., by = NULL,
                             decimals = 4,
                             total.by.row = FALSE,
                             total.by.column = FALSE,
                             DEBUG = FALSE) {
  if(DEBUG) cat("\n[.describe.factor] Called\n")

  args <- list(...)
  var.name = args[["xname"]]

  if(class(x)!="factor") x <- factor(x)

  if(is.null(by)) {
    result <- data.frame("group" = c(levels(x),"NA"))
    t.n <- table(x, useNA = "always")
    result$n <- as.data.frame(t.n)$Freq
    result$rel.freq <- round(prop.table(t.n, margin = 1)*100,digits = decimals)
    result$rel.freq.valid <- c(round(prop.table(table(x, useNA = "no"), margin = 1)*100,digits = decimals),NA)
  } else {
    by.name <- args[["byname"]]

    if(class(by)!="factor") by <- factor(by)
    t.n <- table(x, by, useNA = "always")

    result_n <- data.frame(rbind(t.n))
    rownames(result_n) <- c(paste0(levels(x),"_n"),"NA_n")
    colnames(result_n) <- c(levels(by),"NA")

    if(total.by.row){
      result_rel_freq.row <- data.frame(rbind(round(prop.table(t.n, margin = 1)*100,digits = decimals)))
      colnames(result_rel_freq.row) <- c(levels(by),"NA")
      rownames(result_rel_freq.row) <- c(paste0(levels(x),"_rel.freq.row"),"NA_rel.freq.row")

      result_rel_freq.row$total.row <- rowSums(result_rel_freq.row)



      result_rel_freq_valid.row <- data.frame(rbind(round(prop.table(table(x, by, useNA = "no"), margin = 1)*100,digits = decimals)))
      result_rel_freq_valid.row <- rbind(result_rel_freq_valid.row,rep(NA,ncol(result_rel_freq_valid.row)))
      result_rel_freq_valid.row <- cbind(result_rel_freq_valid.row,rep(NA,nrow(result_rel_freq_valid.row)))

      names(result_rel_freq_valid.row) <- c(levels(by),"NA")
      rownames(result_rel_freq_valid.row) <- c(paste0(levels(x),"_rel.freq.valid.row"),"NA_rel.freq.valid.row")

      result_rel_freq_valid.row$total.row <- rowSums(result_rel_freq_valid.row)

      result_n$total.row <- rowSums(result_n)
      # print(result_rel_freq_valid.row)

      if(total.by.column) {
        result_rel_freq.row <- rbind(result_rel_freq.row, rep(NA,ncol(result_rel_freq.row)))
        result_rel_freq_valid.row <- rbind(result_rel_freq_valid.row, rep(NA,ncol(result_rel_freq_valid.row)))
        rownames(result_rel_freq.row)[nrow(result_rel_freq.row)] <- "total.column"
        rownames(result_rel_freq_valid.row)[nrow(result_rel_freq_valid.row)] <- "total.column"
      }
      # # print(result_rel_freq.row)
    }

    if(total.by.column){
      result_rel_freq.column <- data.frame(rbind(round(prop.table(t.n, margin = 2)*100,digits = decimals)))
      colnames(result_rel_freq.column) <- c(levels(by),"NA")
      rownames(result_rel_freq.column) <- c(paste0(levels(x),"_rel.freq.column"),"NA_rel.freq.column")

      result_rel_freq.column <- rbind(result_rel_freq.column, colSums(result_rel_freq.column, na.rm = TRUE))
      rownames(result_rel_freq.column)[nrow(result_rel_freq.column)] <- "total.column"


      result_rel_freq_valid.column <- data.frame(rbind(round(prop.table(table(x, by, useNA = "no"), margin = 2)*100,digits = decimals)))
      result_rel_freq_valid.column <- rbind(result_rel_freq_valid.column,rep(NA,ncol(result_rel_freq_valid.column)))
      result_rel_freq_valid.column <- cbind(result_rel_freq_valid.column,rep(NA,nrow(result_rel_freq_valid.column)))

      colnames(result_rel_freq_valid.column) <- c(levels(by),"NA")
      rownames(result_rel_freq_valid.column) <- paste0(c(levels(x),"NA"),"_rel.freq.valid.column")

      result_rel_freq_valid.column <- rbind(result_rel_freq_valid.column, colSums(result_rel_freq_valid.column, na.rm = TRUE))
      rownames(result_rel_freq_valid.column)[nrow(result_rel_freq_valid.column)] <- "total.column"

      result_n <- rbind(result_n, colSums(result_rel_freq.column, na.rm = TRUE))
      rownames(result_n)[nrow(result_n)] <- "total.column"

      if(total.by.row) {
        result_rel_freq.column <- cbind(result_rel_freq.column, rep(NA,nrow(result_rel_freq.column)))
        result_rel_freq_valid.column <- cbind(result_rel_freq_valid.column, rep(NA,nrow(result_rel_freq_valid.column)))
        names(result_rel_freq.column)[ncol(result_rel_freq.column)] <- "total.row"
        names(result_rel_freq_valid.column)[ncol(result_rel_freq_valid.column)] <- "total.row"
      }
      # print(result_rel_freq.column)
    }

    # print(result_n)
    if(total.by.row & total.by.column) totales <- "ambos"
    else if(total.by.row) totales <- "row"
    else if(total.by.column) totales <- "col"

    # print(totales)
    print(result_n)
    # print(result_rel_freq.column)
    # print(result_rel_freq_valid.column)

    result <- switch (totales,
      "ambos" = rbind(result_n, result_rel_freq.row,
                                result_rel_freq_valid.row,
                                result_rel_freq.column,
                                result_rel_freq_valid.column),

      "row" = rbind(result_n, result_rel_freq.row,
                              result_rel_freq_valid.row),

      "col" = rbind(result_n, result_rel_freq.column,
                                 result_rel_freq_valid.column)
    )


    row_order <- switch (totales,
                      "ambos" = rbind(rownames(result_n), rownames(result_rel_freq.row),
                                      rownames(result_rel_freq_valid.row),
                                      rownames(result_rel_freq.column),
                                      rownames(result_rel_freq_valid.column)),

                      "row" = rbind(rownames(result_n),
                                    rownames(result_rel_freq.row),
                                    rownames(result_rel_freq_valid.row)),

                      "col" = rbind(rownames(result_n),
                                       rownames(result_rel_freq.column),
                                       rownames(result_rel_freq_valid.column))
    )




    print(result)
    remove <- grep("total.column?",rownames(result),fixed = TRUE)
    if(length(remove) > 0 )row_order = row_order[-remove]


    result <- result[row_order,]
    attr(result,"by.name") <- by.name
  }


  class(result) <- c("feR_describe_factor",class(result))
  attr(result,"var.name") <- var.name

  return(result)

}

#' @export
print.feR_describe_numeric <- function(obj) {
  print(knitr::kable(obj, caption = attr(obj,"var.name")))
  cat("\nNormality test:",attr(obj,"nor.test"),"; p.value:",attr(obj,"p.norm"),"\n")
}


#' @export
print.feR_describe_factor <- function(obj) {
  print(knitr::kable(obj, caption = attr(obj,"var.name")))
}


#' @export
print.feR_describe_data_frame <- function(obj) {
  for(x in obj){
    print(x)
  }

}
