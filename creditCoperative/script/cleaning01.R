####
###TESIS ASOCIATIVIDAD Y SERVICIOS FINANCIEROS
###Alisson Garcia

rm(list = ls())

library(readxl)
library(ggplot2)
library(xtable)
library(stargazer)

setwd("E:/Asesorías de tesis/Economía/Asociatividad y servicios financieros")

carac <- readxl::read_xlsx("raw/caracteristicas.xlsx")
carac2 <- readxl::read_xlsx("raw/caracteristicas2.xlsx")
aso <- readxl::read_xlsx("raw/asociatividad.xlsx")
finan <- readxl::read_xlsx("raw/financieros.xlsx")
superfi <- readxl::read_xlsx("raw/superficie.xlsx")
hogar <- readxl::read_xlsx("raw/hogar.xlsx")

str(carac)

carac2[, c(2:10, 21, 31:36)]
table(carac2[, 31])

#por lo menos una parcela como propietario
carac2$propietario <- ifelse(carac2$P110_1 == "Propietario/",1 , 0)
table(carac2$propietario)

carac2$id <- paste(carac2$CCDD, carac2$CCPP, carac2$CCDI, carac2$CONGLOMERADO, carac2$NSELUA, carac2$UA, sep = "")
table(carac2$id)

carac$id <- paste(carac$CCDD, carac$CCPP, carac$CCDI, carac$CONGLOMERADO, carac$NSELUA, carac$UA, sep ="")
class(carac$id)

propietario <- aggregate(carac2$propietario, by = list(carac2$id), FUN = sum)
propietario$x <- ifelse(propietario$x >= 1, 1, 0)
colnames(propietario)[1] <- "id"
colnames(propietario)[2] <- "prop"
class(propietario$id)

#no dups
colnames(carac)
data1 <- merge(carac, propietario, by = "id")
length(unique(data1$id))
colnames(data1)

data1 <- data1[, c(1, 3:23, 36, 45)]
str(data1)

#merge de asoci
str(aso)
aso$id <- paste(aso$CCDD, aso$CCPP, aso$CCDI, aso$CONGLOMERADO, aso$NSELUA, aso$UA, sep = "")

#merge con finan

data2 <- merge(data1, aso[, c("id", "P801", "P803_1")], by = "id")

str(finan)
finan$id <- paste(finan$CCDD, finan$CCPP, finan$CCDI, finan$CONGLOMERADO, finan$NSELUA, finan$UA, sep = "")
colnames(finan)
data2 <- merge(data2, finan[, 20:54], by = "id")

##clean hogar
str(hogar)
hogar <- hogar[hogar$P1100 == 1, ]
hogar$id <- paste(hogar$CCDD, hogar$CCPP, hogar$CCDI, hogar$CONGLOMERADO, hogar$NSELUA, hogar$UA, sep = "")
colnames(hogar)
data2 <- merge(data2, hogar[, 21:40], by = "id")

str(data2)

colnames(data2)
data3 <- data2[, c(1:39, 66:67, 76:77)]

#agregar variable credito formal (1 = formal, 2 = informal)

data3$credFormal <- 0
table(data3$P903_11)
data3$credFormal[data3$P903_1 == "AGROBANCO"] <- 1
data3$credFormal[data3$P903_2 == "Caja Municip"] <- 1
data3$credFormal[data3$P903_3 == "Caja Rural"] <- 1
data3$credFormal[data3$P903_4 == "Banca privad"] <- 1
data3$credFormal[data3$P903_5 == "Financiera/E"] <- 1
data3$credFormal[data3$P903_6 == "Organismo No"] <- 1
data3$credFormal[data3$P903_7 == "Cooperativa"] <- 1
data3$credFormal[data3$P903_8 == "Establecimie"] <- 2
data3$credFormal[data3$P903_9 == "Prestamista/"] <- 2
data3$credFormal[data3$P903_10 == "Programa de"] <- 1

table(data3$credFormal)

#data con todas las variables
write.csv(data2, "forAnalysis/data.csv")

#data con solo las variables pedidas
write.csv(data3, "forAnalysis/data2.csv")
saveRDS(data3, "forAnalysis/data2.Rdata")

