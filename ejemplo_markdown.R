#' ---
#' title: "Prueba de exportación de resultados"
#' output:
#'    html_document:
#'         toc: yes
#'         toc_depth: 4
#'         number_sections: true
#'         toc_float:
#'            collapsed: true
#' ---

#+ , echo=FALSE, include=FALSE
rm(list=ls())

data_ <- data.frame(AGE=rnorm(30, mean = 30, sd = 5),
                    HEIGHT=sample(x = 120:205, size=30, replace = TRUE ),
                    SEX=sample(x = c("Male", "Female"), prob = c(.5,.5), size = 30, replace = TRUE),
                    BLOND=sample(x = c("Yes", "No"), prob = c(.2,.8), size = 30, replace = TRUE),
                    HEALTH=sample(x = c("Bad", "Normal", "Excelent"), prob = c(0.3,.3,.3), size = 30, replace = TRUE)
)
data_ <- rbind(data_, list(34,NA,NA,NA,NA))
data_ <- rbind(data_, list(33,NA,"Male",NA,NA))
data_ <- rbind(data_, list(22,NA,NA,"No",NA))
data_ <- rbind(data_, list(NA,NA,NA,"No","Bad"))
data_$EMPTY <- rep(NA,nrow(data_))
data_$HEALTH <- as.factor(data_$HEALTH)

#' # Mean
#'
#' ## Global
#'
#' ### Passing vector
#+ , echo = FALSE, message = FALSE, warning = FALSE, eval =TRUE, width = 25, height = 25, unit = "cm"
feR::medias(data_$AGE, DEBUG = T)

#'
#' ### Passing data.frame and var as string
#'
#+ , results = 'asis', echo = FALSE, message = FALSE, warning = FALSE, eval =TRUE, width = 25, height = 25, unit = "cm"
feR::medias(data_, "AGE")

