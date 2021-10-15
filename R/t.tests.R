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
                         ci=0.95, decimals = 2,
                         stop.on.error = FALSE){
  tryCatch(feR:::.check.t_test.parameters(x,by), error = function(e) {
    if(stop.on.error) stop(e)
    else return(NA)
  })

  test <- tryCatch(t.test(x ~ by, conf.level = ci, var.equal = TRUE),
                    error = function(e) {
                      feR:::.error.msg(er = "T_WELCH")
                      message("Error original: ")
                      message(e,"\n")
                      if(stop.on.error) stop()
                      else return(NA)
                    })

  if(is.na(test)) return(NA)
  x.test <- feR:::.t.test.results(test, xname, byname, levels(as.factor(by)), "Welch t-test")
  return(x.test)
}



#' t_test.student
#'
#' @export
t_test.student <- function(x,xname=feR:::.var.name(deparse(substitute(x))),
                           by=NULL, byname = feR:::.var.name(deparse(substitute(by)))
                           ,ci=0.95, stop.on.error = FALSE){
  tryCatch(feR:::.check.t_test.parameters(x,by), error = function(e) {
    if(stop.on.error) stop(e)
    else return(NA)
  })

  test <- tryCatch(t.test(x ~ by, conf.level = ci, var.equal = FALSE),
                    error = function(e) {
                     feR:::.error.msg(er = "T_STUDENT")
                      message("Error original: ")
                      message(e,"\n")
                      if(stop.on.error) stop(e)
                      else return(NA)
                    })

  if(length(test) == 1) if(is.na(test)) return(NA)
  x.test <- feR:::.t.test.results(test, xname, byname, levels(as.factor(by)),"Student t-test")
  return(x.test)
}


.t.test.results <- function(test, xname, byname, by.levels, testname){
  x.test <- data.frame(var.name = c(xname,xname))
  x.test$by.name <- byname
  x.test$by.levels <- by.levels
  x.test$test.name <- testname
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
  data("ToothGrowth")
  #................................................................. ERRORES
  testthat::expect_error(feR::t_test.student(stop.on.error = T, lang = "en"),feR:::.error.msg("MISSING_X", lang="en"))
  testthat::expect_error(feR::t_test.student(as.character(ToothGrowth$len), stop.on.error = T, lang = "en"),feR:::.error.msg("NON_NUM_VECTOR", lang="en"))
  testthat::expect_error(feR::t_test.student(ToothGrowth$len, stop.on.error = T, lang = "en"),feR:::.error.msg("DIFF_LEN_VECTOR", lang="en"))
  testthat::expect_error(feR::t_test.student(ToothGrowth$len, by = ToothGrowth$supp[1:3], stop.on.error = T, lang = "en"),feR:::.error.msg("DIFF_LEN_VECTOR", lang="en"))
  testthat::expect_error(feR::t_test.student(ToothGrowth$len, by = ToothGrowth$dose, stop.on.error = T, lang = "en"),feR:::.error.msg("2_GROUPS", lang="en"))
  testthat::expect_error(feR::t_test.student(as.numeric(c(1,2,rep(NA,2))), by= as.factor(c("a","b","a","b")), stop.on.error = T, lang = "en"), feR:::.error.msg("NOT_ENOUGH_X_OBS", lang="en"))
  testthat::expect_error(feR::t_test.student(ToothGrowth$len[1:4], by= as.factor(c("a","b",rep(NA,2))), stop.on.error = T, lang = "en"), feR:::.error.msg("NOT_ENOUGH_BY_OBS", lang="en"))

  #................................................................. OK
  feR::t_test.student(ToothGrowth$len, by=ToothGrowth$supp, stop.on.error = T, ci = 2)
}
