

#' @export
.describe.feR_math.factor <- function(x,..., y = NULL,
                                      decimals = 4,
                                      total.by.row = TRUE,
                                      total.by.column = FALSE,
                                      DEBUG = FALSE) {
  if(DEBUG) cat("\n[.describe.feR_math.factor] Called")

  args <- list(...)
  var.name = args[["xname"]]

  if(class(x)!="factor") x <- factor(x)

  if(is.null(y)) {
    if(DEBUG) cat("\n[.describe.feR_math.factor] No y")
    result <- data.frame("group" = c(levels(x),"NA"))
    t.n <- table(x, useNA = "always")
    result$n <- as.data.frame(t.n)$Freq
    result$rel.freq <- round(prop.table(t.n)*100,digits = decimals)
    result$rel.freq.valid <- c(round(prop.table(table(x, useNA = "no"))*100,digits = decimals),NA)
  } else {

    y.name <- args[["byname"]]
    if(DEBUG) cat("\n[.describe.feR_math.factor] By",y.name)


    if(class(y)!="factor") y <- factor(y)
    t.n <- table(x, y, useNA = "always")

    result_n <- data.frame(rbind(t.n))
    rownames(result_n) <- c(paste0(levels(x),"_n"),"NA_n")
    colnames(result_n) <- c(levels(y),"NA")


    #............. CALCULATING PERCENTAGES BY ROW
    if(total.y.row){
      result_rel_freq.row <- data.frame(rbind(round(prop.table(t.n, margin = 1)*100,digits = decimals)))
      result_rel_freq_valid.row <- data.frame(rbind(round(prop.table(table(x, y, useNA = "no"), margin = 1)*100,digits = decimals)))

      colnames(result_rel_freq.row) <- c(levels(y),"NA")
      rownames(result_rel_freq.row) <- c(paste0(levels(x),"_rel.freq.row"),"NA_rel.freq.row")

      result_rel_freq.row$total.row <- rowSums(result_rel_freq.row)


      result_rel_freq_valid.row <- rbind(result_rel_freq_valid.row,rep(NA,ncol(result_rel_freq_valid.row)))
      result_rel_freq_valid.row <- cbind(result_rel_freq_valid.row,rep(NA,nrow(result_rel_freq_valid.row)))

      names(result_rel_freq_valid.row) <- c(levels(y),"NA")
      rownames(result_rel_freq_valid.row) <- c(paste0(levels(x),"_rel.freq.valid.row"),"NA_rel.freq.valid.row")

      result_rel_freq_valid.row$total.row <- rowSums(result_rel_freq_valid.row)

      result_n$total.row <- rowSums(result_n)


      if(total.by.column) { #this rows are required for the mergin but will be destroyed later
        result_rel_freq.row <- rbind(result_rel_freq.row, rep(NA,ncol(result_rel_freq.row)))
        result_rel_freq_valid.row <- rbind(result_rel_freq_valid.row, rep(NA,ncol(result_rel_freq_valid.row)))
        rownames(result_rel_freq.row)[nrow(result_rel_freq.row)] <- "total.column_ROW_1"
        rownames(result_rel_freq_valid.row)[nrow(result_rel_freq_valid.row)] <- "total.column_ROW_2"
      }

    }

    #............. CALCULATING PERCENTAGES BY COLUMN
    if(total.by.column){
      result_rel_freq.column <- data.frame(rbind(round(prop.table(t.n, margin = 2)*100,digits = decimals)))
      colnames(result_rel_freq.column) <- c(levels(y),"NA")
      rownames(result_rel_freq.column) <- c(paste0(levels(x),"_rel.freq.column"),"NA_rel.freq.column")

      result_rel_freq.column <- rbind(result_rel_freq.column, colSums(result_rel_freq.column, na.rm = TRUE))
      rownames(result_rel_freq.column)[nrow(result_rel_freq.column)] <- "total.column.rel.freq"


      result_rel_freq_valid.column <- data.frame(rbind(round(prop.table(table(x, y, useNA = "no"), margin = 2)*100,digits = decimals)))
      result_rel_freq_valid.column <- rbind(result_rel_freq_valid.column,rep(NA,ncol(result_rel_freq_valid.column)))
      result_rel_freq_valid.column <- cbind(result_rel_freq_valid.column,rep(NA,nrow(result_rel_freq_valid.column)))

      colnames(result_rel_freq_valid.column) <- c(levels(y),"NA")
      rownames(result_rel_freq_valid.column) <- paste0(c(levels(x),"NA"),"_rel.freq.valid.column")

      result_rel_freq_valid.column <- rbind(result_rel_freq_valid.column, colSums(result_rel_freq_valid.column, na.rm = TRUE))
      rownames(result_rel_freq_valid.column)[nrow(result_rel_freq_valid.column)] <- "total.column.rel.freq.valid"

      result_n <- rbind(result_n, colSums(result_rel_freq.column, na.rm = TRUE))
      rownames(result_n)[nrow(result_n)] <- "total.column_n"

      if(total.by.row) {
        result_rel_freq.column <- cbind(result_rel_freq.column, rep(NA,nrow(result_rel_freq.column)))
        result_rel_freq_valid.column <- cbind(result_rel_freq_valid.column, rep(NA,nrow(result_rel_freq_valid.column)))
        names(result_rel_freq.column)[ncol(result_rel_freq.column)] <- "total.row"
        names(result_rel_freq_valid.column)[ncol(result_rel_freq_valid.column)] <- "total.row"
      }

    }


    if(total.by.row & total.by.column) totales <- "ambos"
    else if(total.by.row) totales <- "row"
    else if(total.by.column) totales <- "col"

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

    #........ removing totals that are extra
    if(totales=="ambos") row_order = row_order[!(row_order %in% c("total.column_ROW_1","total.column_ROW_2"))]


    result <- result[row_order,]
    attr(result,"y.name") <- y.name
  }


  class(result) <- c("feR_describe_factor",class(result))
  attr(result,"var.name") <- var.name
  return(result)
}
