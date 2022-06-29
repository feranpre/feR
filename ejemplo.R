
rm(list = ls())



  data_ <- data.frame(age = rnorm(30, mean = 30, sd = 5),
                    height = sample(x = 120:205, size = 30, replace = TRUE),
                    sex = sample(x = c("Male", "Female"), prob = c(.5, .5),
                          size = 30, replace = TRUE),
                    blond = sample(x = c("Yes", "No"), prob = c(.2, .8),
                          size = 30, replace = TRUE),
                    health = sample(x = c("Bad", "Normal", "Excelent"),
                          prob = c(0.3, .3, .3), size = 30, replace = TRUE)
  )

data_ <- rbind(data_, list(34, NA, NA, NA, NA))
data_ <- rbind(data_, list(33, NA, "Male", NA, NA))
data_ <- rbind(data_, list(22, NA, NA, "No", NA))
data_ <- rbind(data_, list(NA, NA, NA, "No", "Bad"))
data_$empty <- rep(NA, nrow(data_))
data_$health <- as.factor(data_$health)

c <- feR::describe(data_$age)
feR::describe(data_$age)

feR::describe(data_$age, x.name = "asf", y = data_$sex, DEBUG = FALSE)

s <- feR::describe(data_$sex)
feR::describe(data_$sex)
feR::describe(data_$age, y = data_$sex, show.general = TRUE)

feR::welch_test(data_$age, y = data_$sex, lang = "es")



feR::t_test(data_$age, y = data_$sex)
feR::compare(data_$age, y = data_$sex)



feR:::.check.comp_means.parameters()
mean(data_$age)
is.data.frame(data_$age)

library(feR)

feR::medias(data_$age, decimals = 4)
feR::medias(data_$sex, decimals = 4)
feR::medias(data_$age[1:10], by = data_$sex, decimals = 4)
feR::medias(data_$HEALTH, by = data_$sex, decimals = 4)



feR::medias(data_, "age", by = "sex", show.interpretation =T, show.global = F)
feR::medias(data_, "age", by = "HEALTH", show.interpretation =T, show.global = F)
feR::medias(data_, "age", by = "sex", show.interpretation =T, show.global = F, comp = T)
feR::medias(data_, "age", by = "sex", show.interpretation =T, show.global = F, comp = T)
feR::comp.media(data_, "age", by = "sex", DEBUG.FORMA = T, DEBUG.CALL = T)
feR::comp.media(data_$age, by = data_$sex, DEBUG.FORMA = T, DEBUG.CALL = T)

feR::comp.media(data_, "age", by = "HEALTH")
feR::comp.media(data_, "age", by = "HEALTH", show.interpretation = T, show.global = F, show.desc = F)
feR::comp.media(data_[1:6,], "age", by = "HEALTH", show.interpretation =T, show.global = F, show.desc = F)

feR::medias(data_[1:6,], "age", by = "HEALTH", show.interpretation =T, show.global = F, show.desc = T, DEBUG.CALL = F, comp = T)


PRUEBAS.UDAIC.MEDIA = FALSE
if (PRUEBAS.UDAIC.MEDIA){
  # udaic.media("age", DEBUG=TRUE)
  # udaic.media(data_, "age", DEBUG=TRUE)
  # udaic.media(data_, "age", "HEIGHT", DEBUG=TRUE)
  udaic.media(data_, by = "sex", data_$age, DEBUG=TRUE)
  udaic.media(data_$age)
  c <- udaic.media(data_, by = "sex", data_$age, data_$HEALTH, "BLOND", DEBUG=TRUE)
  udaic.media(data_, by = "sex", c("age", "HEIGHT"), DEBUG=TRUE)
  udaic.media(data_, by = c("sex","BLOND"), c("age", "HEIGHT"), DEBUG=FALSE)
}



#----------------------------------------------------------------------------------- FORMA.DATOS

PRUEBAS.FORMA.DATOS = FALSE
if (PRUEBAS.FORMA.DATOS){
  forma.datos(by = "sex", "age", DEBUG=TRUE)
  forma.datos(data_, by = "sex", data_$age, DEBUG=TRUE)
  c <- forma.datos(data_, by = "sex", data_$age, data_$HEALTH, "BLOND", DEBUG=TRUE)
  forma.datos(data_, by = "sex", c("age", "HEIGHT"), DEBUG=TRUE)
  forma.datos(data_, by = c("sex","BLOND"), c("age", "HEIGHT"), DEBUG=TRUE)
}

#----------------------------------------------------------------------------------- CALLS
prueba.call <- function(..., by=NULL, DEBUG = FALSE, DEBUG.CALL = FALSE) {
  forma.datos(..., by = by, DEBUG = DEBUG, DEBUG.CALL = DEBUG.CALL)
}
PRUEBAS.CALL = FALSE
if (PRUEBAS.CALL){
  prueba.call(by = "sex", "age", DEBUG.CALL=TRUE)
  prueba.call(data_, by = "sex", data_$age, DEBUG.CALL=TRUE, DEBUG=TRUE)
  prueba.call(data_, by = "sex", data_$age, data_$HEALTH, "BLOND", DEBUG.CALL=TRUE, DEBUG = TRUE)
  prueba.call(data_, by = "sex", c("age", "HEIGHT"), DEBUG.CALL=TRUE, DEBUG = TRUE)
  prueba.call(data_, by = c("sex","BLOND"), c("age", "HEIGHT"), DEBUG.CALL=TRUE, DEBUG = TRUE)
  prueba.call(data_, by = c("sex","BLOND"), "age", "HEIGHT", DEBUG.CALL=TRUE, DEBUG = TRUE)
}
