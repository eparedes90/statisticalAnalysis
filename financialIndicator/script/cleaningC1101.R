#C-1101- Estado de ganancia y perdida (sheet 2)
#Provisiones Creditos Directos
#Millones de soles

rm(list = ls())

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(tseries)
library(ggplot2)
library(forecast)
library(fpp2)
library(zoo)
library(ggpubr)
theme_set(theme_pubr())
theme_set(theme_bw())

#set directory
basedir <- "raw/C-1101-/"
months <- list.files(basedir, full.names = F, recursive = F)

for (i in 1:length(months)){
  temp <- read_xls(paste0(basedir, months[i]), sheet= 2)
  temp <- temp[34, c(1,4,8,16, 32)]
  colnames(temp) <- c("Concepto", "CMAC Arequipa", "CMAC Cusco", "CMAC Huancayo", "CMAC Piura")

  period <- unlist(strsplit(months[i], split = "_"))
  temp$year <- period[1]
  temp$mes <- period[2]
  temp$mes <- gsub(temp$mes, pattern = ".xls", replacement = "")
  
  if (i == 1){
    temp2 <- temp
  } else {
    temp2 <- rbind(temp2, temp)
  }
}

#format
temp2[, 2:7] <- apply(temp2[, 2:7], 2, function(x) as.numeric(x))

#order
temp2 <- temp2[order(temp2$year, temp2$mes), ]

#formato fecha
temp2$Mes <- paste(temp2$year, temp2$mes, 1, sep ="-")
temp2$Mes <- as.Date(temp2$Mes, format = "%Y-%m-%d")
temp2 <- temp2[order(temp2$Mes, decreasing = F),  ]

#enero a cero
tempx <- temp2
temp2$`CMAC Arequipa`[2:69] <- temp2$`CMAC Arequipa`[2:69]- temp2$`CMAC Arequipa`[1:68]
temp2$`CMAC Cusco`[2:69] <- temp2$`CMAC Cusco`[2:69]- temp2$`CMAC Cusco`[1:68]
temp2$`CMAC Huancayo`[2:69] <- temp2$`CMAC Huancayo`[2:69]- temp2$`CMAC Huancayo`[1:68]
temp2$`CMAC Piura`[2:69] <- temp2$`CMAC Piura`[2:69]- temp2$`CMAC Piura`[1:68]

temp2 <- temp2[!temp2$mes == 1, ]

tempx <- tempx[tempx$mes == 1, ]

temp2 <- rbind(temp2, tempx)
temp2 <- temp2[order(temp2$Mes, decreasing = F), ]

#ggplot

temp3 <- pivot_longer(temp2[, 2:8], cols = 1:4, values_to = "Provisiones", names_to = "caja")

#cambiar de miles a millones de soles
temp3$Provisiones <- temp3$Provisiones/1000
temp3 <- temp3[temp3$caja == "CMAC Arequipa"|
                 temp3$caja == "CMAC Huancayo", ]

millones <- ggplot(temp3, aes(x = Mes, y = Provisiones, group = caja))+
  geom_line(aes(linetype = caja, color = caja), size = 1)+
  geom_point(aes(color = caja, shape = caja), size = 2)+
  theme(legend.title = element_blank())+
  labs(x = "",
       y = "Millones de soles",
       title = "Gastos de Provisiones Créditos Directos")
millones
ggsave("plots/C-1101-Provisiones.jpg", width = 8, height = 6.5)
