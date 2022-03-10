###
###RBIND 215, 2016 AND 2018 ALUMNO DOCENTE

rm(list = ls())

library(foreign)
library(dplyr)

ad15 <- readxl::read_xlsx("E:/Asesorías de tesis/Economía/Eficiencia del gasto público en la educación/dataDocAlum15/alumDoc15.xlsx")
ad16 <- readRDS("E:/Asesorías de tesis/Economía/Eficiencia del gasto público en la educación/dataDocAlum16/alumDoc16.Rdata")
ad18 <- readRDS("E:/Asesorías de tesis/Economía/Eficiencia del gasto público en la educación/alumDoc18.Rdata")

ad <- rbind(ad15, ad16, ad18)

saveRDS(ad, "E:/Asesorías de tesis/Economía/Eficiencia del gasto público en la educación/for_analysis/ad.Rdta")
