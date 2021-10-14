#' =============================================
#' ORDENANDO COSAS
#' =============================================
#' - Estas funciones deberían controlar sus errores básicos
#' - Devolver un data.frame con la información
#' - Tener un print que saque la información adecuadamente
#' - Tener su set de pruebas
#' - USAR FORMULAS
#'
#' Una vez que esté esto resuelto usaré este sistema para ampliarlo a todo
#' - Primero archivo con las pruebas matemáticas y sus soluciones
#' - Despues muestra de resultados y ampliación de información (otro archivo)
#' - Por último un wrapper que permita discriminar qué función usar y agregue todo
#'     - Este wrapper será el que determine:
#'           - qué prueba usar
#'           - si se muestran todas las columnas o no
#'           - si se muestra interpretación o no
#'           - gestión más sofisticada de errores
#'
#'
#' PERO LO PRIMERO es hacer que todo esto funcione suave y pueda tener toda la estadística en orden









#' t_test.welch
#'
#' @export
t_test.welch <- function(x,xname=feR:::.var.name(deparse(substitute(x))),
                         by=NULL, byname = feR:::.var.name(deparse(substitute(by))),
                         ci=0.95, stop.on.error = FALSE){
  test <- tryCatch(t.test(x ~ by, conf.level = ci, var.equal = TRUE),
                    error = function(e) {
                      feR:::.error.msg(er = "T_WELCH")
                      message("Error original: ")
                      message(e,"\n")
                      if(stop.on.error) stop()
                      else return(NA)
                    })

  if(is.na(test)) return(NA)
  x.test <- feR:::.t.test.results(test)
  x.test$var.name <- xname
  x.test$by.name <- byname
  x.test$test.name <- "Welch t-test"
  return(x.test)
}




#' t_test.student
#'
#' @export
t_test.student <- function(x,xname=feR:::.var.name(deparse(substitute(x))),
                           by=NULL, byname = feR:::.var.name(deparse(substitute(by)))
                           ,ci=0.95, stop.on.error = FALSE){
  test <- tryCatch(t.test(x ~ by, conf.level = ci, var.equal = FALSE),
                    error = function(e) {
                     feR:::.error.msg(er = "T_STUDENT")
                      message("Error original: ")
                      message(e,"\n")
                      if(stop.on.error) stop()
                      else return(NA)
                    })

  if(length(test) == 1) if(is.na(test)) return(NA)
  x.test <- feR:::.t.test.results(test)
  x.test$var.name <- xname
  x.test$by.name <- byname
  x.test$test.name <- "Student t-test"
  return(x.test)
}


.t.test.results <- function(test){
  x.test <- data.frame(var.name = c("",""),by.name = c("",""),test.name = c("",""))
  x.test$df <- test$parameter
  x.test$stat.name <- "t"
  x.test$stat.value <- test$statistic
  x.test$stat.ci.low <- test$conf.int[[1]]
  x.test$stat.ci.high <- test$conf.int[[2]]
  x.test$p.value <- test$p.value



  x.test$estimate <- test$estimate
  x.test$mean.diff <- test$estimate[[1]] - test$estimate[[2]]
  x.test$mean.diff.ci.low <- test$conf.int[[1]]
  x.test$mean.diff.ci.high <- test$conf.int[[2]]
  return(x.test)

}



.t.test.PRUEBAS <- function() {
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

  #................................................................. ERRORES
  feR::t_test.student(data_$BLOND, by= data_$SEX) #... error no numeric
  feR::t_test.student(data_$AGE) #... error no BY
  feR::t_test.student(data_$AGE, by= data_$HEALTH) #... by with more than 2 levels


  #................................................................. OK
  feR::t_test.student(data_$AGE, by= data_$SEX)
}
