###
###RBIND 215, 2016 AND 2018 ALUMNO DOCENTE

rm(list = ls())

library(foreign)
library(dplyr)

ad15 <- readxl::read_xlsx("E:/Asesor�as de tesis/Econom�a/Eficiencia del gasto p�blico en la educaci�n/dataDocAlum15/alumDoc15.xlsx")
ad16 <- readRDS("E:/Asesor�as de tesis/Econom�a/Eficiencia del gasto p�blico en la educaci�n/dataDocAlum16/alumDoc16.Rdata")
ad18 <- readRDS("E:/Asesor�as de tesis/Econom�a/Eficiencia del gasto p�blico en la educaci�n/alumDoc18.Rdata")

ad <- rbind(ad15, ad16, ad18)

saveRDS(ad, "E:/Asesor�as de tesis/Econom�a/Eficiencia del gasto p�blico en la educaci�n/for_analysis/ad.Rdta")
