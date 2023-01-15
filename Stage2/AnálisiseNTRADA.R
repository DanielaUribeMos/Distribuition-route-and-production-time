##instalaci?n de paquetes
install.packages("MASS")
install.packages("survival")
install.packages("fitdistrplus")
install.packages("readxl")
##cargar los paquetes

library(MASS)
library(survival)
library(fitdistrplus)
library(readxl)

##cargar serie de datos
ruta<-"Downloads/Datos Tomados.xlsx"


# Generar series
mantenimiento<- read_excel(ruta,sheet = 'Mantenimiento')
alistamiento<- read_excel(ruta,sheet = 'Alistamiento')
paradas_correctivas<- read_excel(ruta,sheet = 'Paradas_Correctivas')
Paradas_Programadas<- read_excel(ruta,sheet = 'Paradas_Programadas')


#-----HOMOGENEIDAD-----

# Razon de varianzas

##Mantenimiento

var.test(mantenimiento$Troqueladora1,mantenimiento$Troqueladora2, conf.level=0.95)$conf.int ##si incluye el 1
var.test(mantenimiento$Troqueladora1,mantenimiento$Troqueladora3,conf.level=0.95)$conf.int ##no incluye el 1
var.test(mantenimiento$Troqueladora2, mantenimiento$Troqueladora3,conf.level=0.95)$conf.int ##no
var.test(mantenimiento$Impresora1, mantenimiento$Impresora2, conf.level = 0.95)$conf.int
var.test(mantenimiento$Impresora1, mantenimiento$Impresora3, conf.level = 0.95)$conf.int##no incluye el 1
var.test(mantenimiento$Impresora1, mantenimiento$Impresora4, conf.level = 0.95)$conf.int
var.test(mantenimiento$Impresora1, mantenimiento$Impresora5, conf.level = 0.95)$conf.int
var.test(mantenimiento$Impresora2, mantenimiento$Impresora3, conf.level = 0.95)$conf.int
var.test(mantenimiento$Impresora2, mantenimiento$Impresora4, conf.level = 0.95)$conf.int
var.test(mantenimiento$Impresora2, mantenimiento$Impresora5, conf.level = 0.95)$conf.int
var.test(mantenimiento$Impresora3, mantenimiento$Impresora4, conf.level = 0.95)$conf.int
var.test(mantenimiento$Impresora3, mantenimiento$Impresora5, conf.level = 0.95)$conf.int
var.test(mantenimiento$Impresora4, mantenimiento$Impresora5, conf.level = 0.95)$conf.int

##no tiene la misma varianza estadisticamente

       
# En ninguna pareja de series se presentan varianzas homogéneas, por lo tanto no hay necesidad de realizar diferencia de medias y se concluye que las series NO son homogéneas entre sí.


#-----AUTOCORRELACION-----

#no hay autocorrelacion
a__mantenimiento_t1<- acf(mantenimiento$Troqueladora1, lag.max = 5, na.action = na.pass, main= "Autocorrelograma Tiempo mantenimiento Troqueladora 1")
#no hay autocorrelacion
a__mantenimiento_t2<- acf(mantenimiento$Troqueladora2, lag.max = 5, na.action = na.pass, main= "Autocorrelograma Tiempo mantenimiento Troqueladora 2")
#no hay autocorrelacion
a__mantenimiento_t3<- acf(mantenimiento$Troqueladora3, lag.max = 5, na.action = na.pass, main= "Autocorrelograma Tiempo mantenimiento Troqueladora 3")
#no hay
a_mantenimiento_i1<- acf(mantenimiento$Impresora1, lag.max = 5, na.action = na.pass, main ="Autocorrelograma Tiempo mantenimiento impresora 1")
#no hay
a_mantenimiento_i2<- acf(mantenimiento$Impresora2, lag.max = 5, na.action = na.pass, main ="Autocorrelograma Tiempo mantenimiento impresora 2")
#hay nivel 3 pero no nos importa
a_mantenimiento_i3<- acf(mantenimiento$Impresora3, lag.max = 5, na.action = na.pass, main ="Autocorrelograma Tiempo mantenimiento impresora 3")
#no hay
a_mantenimiento_i4<- acf(mantenimiento$Impresora4, lag.max = 5, na.action = na.pass, main ="Autocorrelograma Tiempo mantenimiento impresora 4")
#no hay
a_mantenimiento_i5<- acf(mantenimiento$Impresora5, lag.max = 5, na.action = na.pass, main ="Autocorrelograma Tiempo mantenimiento impresora 5")


#no hay
a_alistamiento_t1<- acf(alistamiento$Troqueladora1, lag.max = 5, na.action = na.pass, main= "Autocorrelograma Tiempo alistamiento Troqueladora 1")
#no hay
a_alistamiento_t2<- acf(alistamiento$Troqueladora2, lag.max = 5, na.action = na.pass, main= "Autocorrelograma Tiempo alistamiento Troqueladora 2")
#no hay
a_alistamiento_t3<- acf(alistamiento$Troqueladora3, lag.max = 5, na.action = na.pass, main= "Autocorrelograma Tiempo alistamiento Troqueladora 3")
#no hay
a_alistamiento_i1<-acf(alistamiento$Impresora1, lag.max = 5, na.action = na.pass, main= "Autocorrelograma Tiempo alistamiento Impresora 1")
#no hay
a_alistamiento_i2<-acf(alistamiento$Impresora2, lag.max = 5, na.action = na.pass, main= "Autocorrelograma Tiempo alistamiento Impresora 2")
#no
a_alistamiento_i3<-acf(alistamiento$Impresora3, lag.max = 5, na.action = na.pass, main= "Autocorrelograma Tiempo alistamiento Impresora 3")
#hay nivel 5, dont care
a_alistamiento_i4<-acf(alistamiento$Impresora4, lag.max = 5, na.action = na.pass, main= "Autocorrelograma Tiempo alistamiento Impresora 4")
#no
a_alistamiento_i5<-acf(alistamiento$Impresora5, lag.max = 5, na.action = na.pass, main= "Autocorrelograma Tiempo alistamiento Impresora 5")

#no
a_paradascorrectivas_t1<- acf(paradas_correctivas$Troqueladora1, lag.max = 5, na.action = na.pass, main= "Autocorrelograma Paradas Correctivas Troqueladora 1")
#no 
a_paradascorrectivas_t2<- acf(paradas_correctivas$Troqueladora2, lag.max = 5, na.action = na.pass, main= "Autocorrelograma Paradas Correctivas Troqueladora 2")
#no
a_paradascorrectivas_t3<- acf(paradas_correctivas$Troqueladora3, lag.max = 5, na.action = na.pass, main= "Autocorrelograma Paradas Correctivas Troqueladora 3")
#no
a_paradascorrectivas_i1<- acf(paradas_correctivas$Impresora1, lag.max = 5, na.action = na.pass, main= "Autocorrelograma Paradas Correctivas Impresora 1")
#no
a_paradascorrectivas_i2<- acf(paradas_correctivas$Impresora2, lag.max = 5, na.action = na.pass, main= "Autocorrelograma Paradas Correctivas Impresora 2")
#no
a_paradascorrectivas_i3<- acf(paradas_correctivas$Impresora3, lag.max = 5, na.action = na.pass, main= "Autocorrelograma Paradas Correctivas Impresora 3")
#no
a_paradascorrectivas_i4<- acf(paradas_correctivas$Impresora4, lag.max = 5, na.action = na.pass, main= "Autocorrelograma Paradas Correctivas Impresora 4")
#no
a_paradascorrectivas_i5<- acf(paradas_correctivas$Impresora5, lag.max = 5, na.action = na.pass, main= "Autocorrelograma Paradas Correctivas Impresora 5")


#no
a_paradasprogramadas_t1<- acf(Paradas_Programadas$Troqueladora1, lag.max = 5, na.action = na.pass, main= "Autocorrelograma Paradas Programadas Troqueladora 1")
#no
a_paradasprogramadas_t2<- acf(Paradas_Programadas$Troqueladora2, lag.max = 5, na.action = na.pass, main= "Autocorrelograma Paradas Programadas Troqueladora 2")
#hay nivel 2
a_paradasprogramadas_t3<- acf(Paradas_Programadas$Troqueladora3, lag.max = 5, na.action = na.pass, main= "Autocorrelograma Paradas Programadas Troqueladora 3")
#no
a_paradasprogramadas_i1 <- acf(Paradas_Programadas$Impresora1,lag.max = 5, na.action = na.pass, main= "Autocorrelograma Paradas Programadas Impresora 1")
#no
a_paradasprogramadas_i2 <- acf(Paradas_Programadas$Impresora2,lag.max = 5, na.action = na.pass, main= "Autocorrelograma Paradas Programadas Impresora 2")
#no
a_paradasprogramadas_i3 <- acf(Paradas_Programadas$Impresora3,lag.max = 5, na.action = na.pass, main= "Autocorrelograma Paradas Programadas Impresora 3")
#no
a_paradasprogramadas_i4 <- acf(Paradas_Programadas$Impresora4,lag.max = 5, na.action = na.pass, main= "Autocorrelograma Paradas Programadas Impresora 4")
#no
a_paradasprogramadas_i5 <- acf(Paradas_Programadas$Impresora5,lag.max = 5, na.action = na.pass, main= "Autocorrelograma Paradas Programadas Impresora 5")

##ninguno es nivel 1 entonces no se hace nada

#-----PRUEBAS DE BONDAD DE AJUSTE


###Mantenimiento
#histograma

hist(mantenimiento$Troqueladora1, main = "Histograma Tiempo Mantenimiento Troqueladora 1", las=1, prob=FALSE)
hist(mantenimiento$Troqueladora2, main = "Histograma Tiempo Mantenimiento Troqueladora 2", las=1, prob=FALSE)
hist(mantenimiento$Troqueladora3, main = "Histograma Tiempo Mantenimiento Troqueladora 3", las=1, prob=FALSE)
hist(mantenimiento$Impresora1,main = "Histograma Tiempo Mantenimiento Impresora1", las=1, prob=FALSE)
hist(mantenimiento$Impresora2,main = "Histograma Tiempo Mantenimiento Impresora 2", las=1, prob=FALSE)
hist(mantenimiento$Impresora3,main = "Histograma Tiempo Mantenimiento Impresora 3", las=1, prob=FALSE)
hist(mantenimiento$Impresora4,main = "Histograma Tiempo Mantenimiento Impresora 4", las=1, prob=FALSE)
hist(mantenimiento$Impresora5,main = "Histograma Tiempo Mantenimiento Impresora 5", las=1, prob=FALSE)

hist(alistamiento$Troqueladora1, main = "Histograma Tiempo alistamiento Troqueladora 1", las=1, prob=FALSE)
hist(alistamiento$Troqueladora2, main = "Histograma Tiempo alistamiento Troqueladora 2", las=1, prob=FALSE)
hist(alistamiento$Troqueladora3, main = "Histograma Tiempo alistamiento Troqueladora 3", las=1, prob=FALSE)
hist(alistamiento$Impresora1,main = "Histograma Tiempo alistamiento Impresora1", las=1, prob=FALSE)
hist(alistamiento$Impresora2,main = "Histograma Tiempo alistamiento Impresora2", las=1, prob=FALSE)  
hist(alistamiento$Impresora3,main = "Histograma Tiempo alistamiento Impresora3", las=1, prob=FALSE)
hist(alistamiento$Impresora4,main = "Histograma Tiempo alistamiento Impresora4", las=1, prob=FALSE)
hist(alistamiento$Impresora5,main = "Histograma Tiempo alistamiento Impresora5", las=1, prob=FALSE)

hist(paradas_correctivas$Troqueladora1,main = "Histograma paradas correctivas Troqueladora 1", las=1, prob=FALSE)
hist(paradas_correctivas$Troqueladora2,main = "Histograma paradas correctivas Troqueladora 2", las=1, prob=FALSE)
hist(paradas_correctivas$Troqueladora3,main = "Histograma paradas correctivas Troqueladora 3", las=1, prob=FALSE)
hist(paradas_correctivas$Impresora1, main ="Histograma paradas correctivas impresora 1", las=1, prob=FALSE) 
hist(paradas_correctivas$Impresora2, main ="Histograma paradas correctivas impresora 2", las=1, prob=FALSE) 
hist(paradas_correctivas$Impresora3, main ="Histograma paradas correctivas impresora 3", las=1, prob=FALSE) 
hist(paradas_correctivas$Impresora4, main ="Histograma paradas correctivas impresora 4", las=1, prob=FALSE) 
hist(paradas_correctivas$Impresora5, main ="Histograma paradas correctivas impresora 5", las=1, prob=FALSE) 

hist(Paradas_Programadas$Troqueladora1, main = "Histograma paraadas programadas troqueladora 1",las=1, prob=FALSE )
hist(Paradas_Programadas$Troqueladora2, main = "Histograma paraadas programadas troqueladora 2",las=1, prob=FALSE )
hist(Paradas_Programadas$Troqueladora3, main = "Histograma paraadas programadas troqueladora 3",las=1, prob=FALSE )
hist(Paradas_Programadas$Impresora1, main = "Histograma paraadas programadas impresora 1",las=1, prob=FALSE )
hist(Paradas_Programadas$Impresora2, main = "Histograma paraadas programadas impresora 2",las=1, prob=FALSE )
hist(Paradas_Programadas$Impresora3, main = "Histograma paraadas programadas impresora 3",las=1, prob=FALSE )
hist(Paradas_Programadas$Impresora4, main = "Histograma paraadas programadas impresora 4",las=1, prob=FALSE )
hist(Paradas_Programadas$Impresora5, main = "Histograma paraadas programadas impresora 5",las=1, prob=FALSE )


#Ajustes

##Manetnimiento

###T1
ajuste_mantenimiento_t1 <- fitdist(mantenimiento$Troqueladora1,"lnorm",keepdata = TRUE)
ajuste_mantenimiento_t1$estimate
plot(ajuste_mantenimiento_t1)

resultados_mantenimiento_t1 <- gofstat(ajuste_mantenimiento_t1)
resultados_mantenimiento_t1$kstest
resultados_mantenimiento_t1$chisqpvalue

###T2
ajuste_mantenimiento_t2 <- fitdist(mantenimiento$Troqueladora2,"lnorm",keepdata = TRUE)
ajuste_mantenimiento_t2$estimate
plot(ajuste_mantenimiento_t2)

resultados_mantenimiento_t2 <- gofstat(ajuste_mantenimiento_t2)
resultados_mantenimiento_t2$kstest
resultados_mantenimiento_t2$chisqpvalue

###T3
ajuste_mantenimiento_t3 <- fitdist(mantenimiento$Troqueladora3,"lnorm",keepdata = TRUE)
ajuste_mantenimiento_t3$estimate
plot(ajuste_mantenimiento_t3)

resultados_mantenimiento_t3 <- gofstat(ajuste_mantenimiento_t3)
resultados_mantenimiento_t3$kstest
resultados_mantenimiento_t3$chisqpvalue

###I1
ajuste_mantenimiento_i1 <- fitdist(mantenimiento$Impresora1,"lnorm",keepdata = TRUE)
ajuste_mantenimiento_i1$estimate
plot(ajuste_mantenimiento_i1)

resultados_mantenimiento_i1 <- gofstat(ajuste_mantenimiento_i1)
resultados_mantenimiento_i1$kstest
resultados_mantenimiento_i1$chisqpvalue

###I2
ajuste_mantenimiento_i2 <- fitdist(mantenimiento$Impresora2,"lnorm",keepdata = TRUE)
ajuste_mantenimiento_i2$estimate
plot(ajuste_mantenimiento_i2)

resultados_mantenimiento_i2 <- gofstat(ajuste_mantenimiento_i2)
resultados_mantenimiento_i2$kstest
resultados_mantenimiento_i2$chisqpvalue

###I3
ajuste_mantenimiento_i3 <- fitdist(mantenimiento$Impresora3,"unif",keepdata = TRUE)
ajuste_mantenimiento_i3$estimate
plot(ajuste_mantenimiento_i3)

resultados_mantenimiento_i3 <- gofstat(ajuste_mantenimiento_i3)
resultados_mantenimiento_i3$kstest
resultados_mantenimiento_i3$chisqpvalue

###I4
ajuste_mantenimiento_i4 <- fitdist(mantenimiento$Impresora4,"lnorm",keepdata = TRUE)
ajuste_mantenimiento_i4$estimate
plot(ajuste_mantenimiento_i4)

resultados_mantenimiento_i4 <- gofstat(ajuste_mantenimiento_i4)

resultados_mantenimiento_i4$kstest
resultados_mantenimiento_i4$chisqpvalue

###I5
ajuste_mantenimiento_i5 <- fitdist(mantenimiento$Impresora5,"lnorm",keepdata = TRUE)
ajuste_mantenimiento_i5$estimate
plot(ajuste_mantenimiento_i5)

resultados_mantenimiento_i5 <- gofstat(ajuste_mantenimiento_i5)
resultados_mantenimiento_i5$kstest
resultados_mantenimiento_i5$chisqpvalue

##Alistamiento

###T1
ajuste_alistamiento_t1 <- fitdist(alistamiento$Troqueladora1,"lnorm",keepdata = TRUE)
ajuste_alistamiento_t1$estimate
plot(ajuste_alistamiento_t1)

resultados_alistamiento_t1 <- gofstat(ajuste_alistamiento_t1)
resultados_alistamiento_t1$kstest
resultados_alistamiento_t1$chisqpvalue

###T2
ajuste_alistamiento_t2 <- fitdist(alistamiento$Troqueladora2,"lnorm",keepdata = TRUE)
ajuste_alistamiento_t2$estimate
plot(ajuste_alistamiento_t2)

resultados_alistamiento_t2 <- gofstat(ajuste_alistamiento_t2)
resultados_alistamiento_t2$kstest
resultados_alistamiento_t2$chisqpvalue

###T3
ajuste_alistamiento_t3 <- fitdist(alistamiento$Troqueladora3,"lnorm",keepdata = TRUE)
ajuste_alistamiento_t3$estimate
plot(ajuste_alistamiento_t3)

resultados_alistamiento_t3 <- gofstat(ajuste_alistamiento_t3)
resultados_alistamiento_t3$kstest
resultados_alistamiento_t3$chisqpvalue

###I1
ajuste_alistamiento_i1 <- fitdist(alistamiento$Impresora1,"lnorm",keepdata = TRUE)
ajuste_alistamiento_i1$estimate
plot(ajuste_alistamiento_i1)

resultados_alistamiento_i1 <- gofstat(ajuste_alistamiento_i1)
resultados_alistamiento_i1$kstest
resultados_alistamiento_i1$chisqpvalue

###I2
ajuste_alistamiento_i2 <- fitdist(alistamiento$Impresora2,"lnorm",keepdata = TRUE)
ajuste_alistamiento_i2$estimate
plot(ajuste_alistamiento_i2)

resultados_alistamiento_i2 <- gofstat(ajuste_alistamiento_i2)
resultados_alistamiento_i2$kstest
resultados_alistamiento_i2$chisqpvalue

###I3
ajuste_alistamiento_i3 <- fitdist(alistamiento$Impresora3,"lnorm",keepdata = TRUE)
ajuste_alistamiento_i3$estimate
plot(ajuste_alistamiento_i3)

resultados_alistamiento_i3 <- gofstat(ajuste_alistamiento_i3)
resultados_alistamiento_i3$kstest
resultados_alistamiento_i3$chisqpvalue

###I4
ajuste_alistamiento_i4 <- fitdist(alistamiento$Impresora4,"lnorm",keepdata = TRUE)
ajuste_alistamiento_i4$estimate
plot(ajuste_alistamiento_i4)

resultados_alistamiento_i4 <- gofstat(ajuste_alistamiento_i4)
resultados_alistamiento_i4$kstest
resultados_alistamiento_i4$chisqpvalue

###I5
ajuste_alistamiento_i5 <- fitdist(alistamiento$Impresora5,"lnorm",keepdata = TRUE)
ajuste_alistamiento_i5$estimate
plot(ajuste_alistamiento_i5)

resultados_alistamiento_i5 <- gofstat(ajuste_alistamiento_i5)
resultados_alistamiento_i5$kstest
resultados_alistamiento_i5$chisqpvalue

##Parada Correctiva

###T1
ajuste_paradas_correctivas_t1 <- fitdist(paradas_correctivas$Troqueladora1,"lnorm",keepdata = TRUE)
ajuste_paradas_correctivas_t1$estimate
plot(ajuste_paradas_correctivas_t1)

resultados_paradas_correctivas_t1 <- gofstat(ajuste_paradas_correctivas_t1)
resultados_paradas_correctivas_t1$kstest

resultados_paradas_correctivas_t1$chisqpvalue

###T2 
ajuste_paradas_correctivas_t2 <- fitdist(paradas_correctivas$Troqueladora2,"lnorm",keepdata = TRUE)
ajuste_paradas_correctivas_t2$estimate
plot(ajuste_paradas_correctivas_t2)

resultados_paradas_correctivas_t2 <- gofstat(ajuste_paradas_correctivas_t2)
resultados_paradas_correctivas_t2$kstest
resultados_paradas_correctivas_t2$chisqpvalue

###T3
ajuste_paradas_correctivas_t3 <- fitdist(paradas_correctivas$Troqueladora3,"lnorm",keepdata = TRUE)
ajuste_paradas_correctivas_t3$estimate
plot(ajuste_paradas_correctivas_t3)

resultados_paradas_correctivas_t3 <- gofstat(ajuste_paradas_correctivas_t3)
resultados_paradas_correctivas_t3$kstest
resultados_paradas_correctivas_t3$chisqpvalue

###I1
ajuste_paradas_correctivas_i1 <- fitdist(paradas_correctivas$Impresora1,"lnorm",keepdata = TRUE)
ajuste_paradas_correctivas_i1$estimate
plot(ajuste_paradas_correctivas_i1)

resultados_paradas_correctivas_i1 <- gofstat(ajuste_paradas_correctivas_i1)
resultados_paradas_correctivas_i1$kstest
resultados_paradas_correctivas_i1$chisqpvalue

###I2 
ajuste_paradas_correctivas_i2 <- fitdist(paradas_correctivas$Impresora2,"lnorm",keepdata = TRUE)
ajuste_paradas_correctivas_i2$estimate
plot(ajuste_paradas_correctivas_i2)

resultados_paradas_correctivas_i2 <- gofstat(ajuste_paradas_correctivas_i2)
resultados_paradas_correctivas_i2$kstest
resultados_paradas_correctivas_i2$chisqpvalue

###I3 
ajuste_paradas_correctivas_i3 <- fitdist(paradas_correctivas$Impresora3,"lnorm",keepdata = TRUE)
ajuste_paradas_correctivas_i3$estimate
plot(ajuste_paradas_correctivas_i3)

resultados_paradas_correctivas_i3 <- gofstat(ajuste_paradas_correctivas_i3)
resultados_paradas_correctivas_i3$kstest
resultados_paradas_correctivas_i3$chisqpvalue

###I4
ajuste_paradas_correctivas_i4 <- fitdist(paradas_correctivas$Impresora4,"lnorm",keepdata = TRUE)
ajuste_paradas_correctivas_i4$estimate
plot(ajuste_paradas_correctivas_i4)

resultados_paradas_correctivas_i4 <- gofstat(ajuste_paradas_correctivas_i4)
resultados_paradas_correctivas_i4$kstest
resultados_paradas_correctivas_i4$chisqpvalue

###I5
ajuste_paradas_correctivas_i5 <- fitdist(paradas_correctivas$Impresora5,"lnorm",keepdata = TRUE)
ajuste_paradas_correctivas_i5$estimate
plot(ajuste_paradas_correctivas_i5)

resultados_paradas_correctivas_i5 <- gofstat(ajuste_paradas_correctivas_i5)
resultados_paradas_correctivas_i5$kstest

resultados_paradas_correctivas_i5$chisqpvalue


##Parada Programada

###T1 
ajuste_Paradas_Programadas_t1 <- fitdist(Paradas_Programadas$Troqueladora1,"lnorm",keepdata = TRUE)
ajuste_Paradas_Programadas_t1$estimate
plot(ajuste_Paradas_Programadas_t1)


resultados_Paradas_Programadas_t1 <- gofstat(ajuste_Paradas_Programadas_t1)
resultados_Paradas_Programadas_t1$kstest
resultados_Paradas_Programadas_t1$chisqpvalue

###T2 
ajuste_Paradas_Programadas_t2 <- fitdist(Paradas_Programadas$Troqueladora2,"unif",keepdata = TRUE)
ajuste_Paradas_Programadas_t2$estimate
plot(ajuste_Paradas_Programadas_t2)

resultados_Paradas_Programadas_t2 <- gofstat(ajuste_Paradas_Programadas_t2)
resultados_Paradas_Programadas_t2$kstest
resultados_Paradas_Programadas_t2$chisqpvalue

###T3
ajuste_Paradas_Programadas_t3 <- fitdist(Paradas_Programadas$Troqueladora3,"lnorm",keepdata = TRUE)
ajuste_Paradas_Programadas_t3$estimate
plot(ajuste_Paradas_Programadas_t3)

resultados_Paradas_Programadas_t3 <- gofstat(ajuste_Paradas_Programadas_t3)
resultados_Paradas_Programadas_t3$kstest
resultados_Paradas_Programadas_t3$chisqpvalue

###I1
ajuste_Paradas_Programadas_i1 <- fitdist(Paradas_Programadas$Impresora1,"lnorm",keepdata = TRUE)
ajuste_Paradas_Programadas_i1$estimate
plot(ajuste_Paradas_Programadas_i1)

resultados_Paradas_Programadas_i1 <- gofstat(ajuste_Paradas_Programadas_i1)
resultados_Paradas_Programadas_i1$kstest
resultados_Paradas_Programadas_i1$chisqpvalue

###I2
ajuste_Paradas_Programadas_i2 <- fitdist(Paradas_Programadas$Impresora2,"lnorm",keepdata = TRUE)
ajuste_Paradas_Programadas_i2$estimate
plot(ajuste_Paradas_Programadas_i2)

resultados_Paradas_Programadas_i2 <- gofstat(ajuste_Paradas_Programadas_i2)
resultados_Paradas_Programadas_i2$kstest
resultados_Paradas_Programadas_i2$chisqpvalue

###I3
ajuste_Paradas_Programadas_i3 <- fitdist(Paradas_Programadas$Impresora3,"lnorm",keepdata = TRUE)
ajuste_Paradas_Programadas_i3$estimate
plot(ajuste_Paradas_Programadas_i3)

resultados_Paradas_Programadas_i3 <- gofstat(ajuste_Paradas_Programadas_i3)
resultados_Paradas_Programadas_i3$kstest
resultados_Paradas_Programadas_i3$chisqpvalue

###I4
ajuste_Paradas_Programadas_i4 <- fitdist(Paradas_Programadas$Impresora4,"lnorm",keepdata = TRUE)
ajuste_Paradas_Programadas_i4$estimate
plot(ajuste_Paradas_Programadas_i4)

resultados_Paradas_Programadas_i4 <- gofstat(ajuste_Paradas_Programadas_i4)
resultados_Paradas_Programadas_i4$kstest
resultados_Paradas_Programadas_i4$chisqpvalue

###I5
ajuste_Paradas_Programadas_i5 <- fitdist(Paradas_Programadas$Impresora5,"lnorm",keepdata = TRUE)
ajuste_Paradas_Programadas_i5$estimate
plot(ajuste_Paradas_Programadas_i5)

resultados_Paradas_Programadas_i5 <- gofstat(ajuste_Paradas_Programadas_i5)
resultados_Paradas_Programadas_i5$kstest
resultados_Paradas_Programadas_i5$chisqpvalue




