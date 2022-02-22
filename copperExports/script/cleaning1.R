#####
#####TESIS ALVARO
#### EFECTO DEL PBI DE CHINA EN LA EXPORT DE COBRE
#####3/11

#empty
rm(list=ls())

#library
library(dplyr)
library(readxl)
library(tidyr)

#set
setwd("E:/Asesorías de tesis/Economía/Crecimiento económico de China y exportación de cobre")

#read
ied <- readxl::read_xlsx("data/ied.xlsx")
pbi <- read_xls("data/pbiChina.xls")
precio <- read_xlsx("data/precioCobre.xlsx")
tipo <- read_xlsx("data/tipoCambio.xlsx")
expor <- read_xlsx("data/volCobre.xlsx")

#clean periodos
x <- c(0:87)
x <- x*3+1
precio <- precio[x, ]
x <- c(0:123)
x <- x*3+1
tipo <- tipo[x, ]

#
ied <- ied[, c(2:4)]
expor <- expor[, c(2:4)]
pbi <- pbi[, c(3:5)]
precio <- precio[, c(3:5)]
tipo <- tipo[, c(3:5)]

#merge
data <- merge(expor, ied, by = c("year", "tri"))
data <- merge(data, pbi, by = c("year", "tri"))
data <- merge(data, precio, by = c("year", "tri"))
data <- merge(data, tipo, by = c("year", "tri"))

#percent variation
data <- data %>%
  mutate(volCobre_var = (volCobre/lag(volCobre) - 1) * 100,
         ied_var = (ied/lag(ied)-1)*100,
         pbiChina_var = (pbiChina/lag(pbiChina)-1)*100,
         precioCobre_var = (precioCobre/lag(precioCobre)-1)*100,
         tipoCambio_var = (tipoCambio/lag(tipoCambio)-1)*100)
  
#write.csv
write.csv(data, "forAnalysis/dataClean.csv")
saveRDS(data, "forAnalysis/dataClean.Rdata")

