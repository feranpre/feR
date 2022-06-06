# install.packages(c('nortest','PMCMRplus','car'))
# install.packages("janitor")
# install.packages("magrittr")

rm(list = ls())






# library(udaicR)
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


feR::describe(data_$AGE)
feR::describe(data_$SEX)
feR::describe(data_$AGE, data_$SEX)
feR::compare(data_$AGE, data_$SEX, DEBUG =F)



mean(data_$AGE)
is.data.frame(data_$AGE)

library(feR)

feR::medias(data_$AGE, decimals = 4)
feR::medias(data_$SEX, decimals = 4)
feR::medias(data_$AGE[1:10], by = data_$SEX, decimals = 4)
feR::medias(data_$HEALTH, by = data_$SEX, decimals = 4)

# test github token, for real
# a


feR::medias(data_, "AGE", by = "SEX", show.interpretation =T, show.global = F)
feR::medias(data_, "AGE", by = "HEALTH", show.interpretation =T, show.global = F)
feR::medias(data_, "AGE", by = "SEX", show.interpretation =T, show.global = F, comp = T)
feR::medias(data_, "AGE", by = "SEX", show.interpretation =T, show.global = F, comp = T)
feR::comp.media(data_, "AGE", by = "SEX", DEBUG.FORMA = T, DEBUG.CALL = T)
feR::comp.media(data_$AGE, by = data_$SEX, DEBUG.FORMA = T, DEBUG.CALL = T)

feR::comp.media(data_, "AGE", by = "HEALTH")
feR::comp.media(data_, "AGE", by = "HEALTH", show.interpretation =T, show.global = F, show.desc = F)
feR::comp.media(data_[1:6,], "AGE", by = "HEALTH", show.interpretation =T, show.global = F, show.desc = F)

feR::medias(data_[1:6,], "AGE", by = "HEALTH", show.interpretation =T, show.global = F, show.desc = T, DEBUG.CALL = F, comp = T)


PRUEBAS.UDAIC.MEDIA = FALSE
if (PRUEBAS.UDAIC.MEDIA){
  # udaic.media("AGE", DEBUG=TRUE)
  # udaic.media(data_, "AGE", DEBUG=TRUE)
  # udaic.media(data_, "AGE", "HEIGHT", DEBUG=TRUE)
  udaic.media(data_, by = "SEX", data_$AGE, DEBUG=TRUE)
  udaic.media(data_$AGE)
  c <- udaic.media(data_, by = "SEX", data_$AGE, data_$HEALTH, "BLOND", DEBUG=TRUE)
  udaic.media(data_, by = "SEX", c("AGE", "HEIGHT"), DEBUG=TRUE)
  udaic.media(data_, by = c("SEX","BLOND"), c("AGE", "HEIGHT"), DEBUG=FALSE)
}



#----------------------------------------------------------------------------------- FORMA.DATOS

PRUEBAS.FORMA.DATOS = FALSE
if (PRUEBAS.FORMA.DATOS){
  forma.datos(by = "SEX", "AGE", DEBUG=TRUE)
  forma.datos(data_, by = "SEX", data_$AGE, DEBUG=TRUE)
  c <- forma.datos(data_, by = "SEX", data_$AGE, data_$HEALTH, "BLOND", DEBUG=TRUE)
  forma.datos(data_, by = "SEX", c("AGE", "HEIGHT"), DEBUG=TRUE)
  forma.datos(data_, by = c("SEX","BLOND"), c("AGE", "HEIGHT"), DEBUG=TRUE)
}

#----------------------------------------------------------------------------------- CALLS
prueba.call <- function(..., by=NULL, DEBUG = FALSE, DEBUG.CALL = FALSE) {
  forma.datos(..., by = by, DEBUG = DEBUG, DEBUG.CALL = DEBUG.CALL)
}
PRUEBAS.CALL = FALSE
if (PRUEBAS.CALL){
  prueba.call(by = "SEX", "AGE", DEBUG.CALL=TRUE)
  prueba.call(data_, by = "SEX", data_$AGE, DEBUG.CALL=TRUE, DEBUG=TRUE)
  prueba.call(data_, by = "SEX", data_$AGE, data_$HEALTH, "BLOND", DEBUG.CALL=TRUE, DEBUG = TRUE)
  prueba.call(data_, by = "SEX", c("AGE", "HEIGHT"), DEBUG.CALL=TRUE, DEBUG = TRUE)
  prueba.call(data_, by = c("SEX","BLOND"), c("AGE", "HEIGHT"), DEBUG.CALL=TRUE, DEBUG = TRUE)
  prueba.call(data_, by = c("SEX","BLOND"), "AGE", "HEIGHT", DEBUG.CALL=TRUE, DEBUG = TRUE)
}

