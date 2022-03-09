#C-1205- Ranking de Cr?ditos, Dep?sitos y Patrimonio
#Ranking de Cr?ditos, Dep?sitos y Patrimonio de las Cajas Municipales
#Porcentaje

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

theme_set(theme_bw())

basedir <- "raw/C-1205-/"
months <- list.files(basedir, full.names = F, recursive = F)
months

for (i in 1:length(months)){
  temp <- read_xls(paste0(basedir, months[i]))
  colnames(temp) <- temp[7, ]
  colnames(temp)
  temp <- temp[7:20, ]
  temp <- temp[temp$Empresas == "CMAC Arequipa"|
                 temp$Empresas == "CMAC Cusco"|
                 temp$Empresas == "CMAC Huancayo"|
                 temp$Empresas == "CMAC Piura",
               c(2, 4)]
  
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

temp2 <- temp2[!is.na(temp2$Empresas), ]

temp2 <- pivot_wider(temp2, names_from = "Empresas", values_from = "Participación")

#format
temp2[, ] <- apply(temp2[, ], 2, function(x) as.numeric(x))

#order
temp2 <- temp2[order(temp2$year, temp2$mes), ]

#formato fecha
temp2$Mes <- paste(temp2$year, temp2$mes, 1, sep ="-")
temp2$Mes <- as.Date(temp2$Mes, format = "%Y-%m-%d")

##ggplot
temp3 <- pivot_longer(temp2, cols = 3:6, names_to = "caja", values_to = "ranking")
temp3 <- temp3[temp3$caja == "CMAC Arequipa"|
                 temp3$caja == "CMAC Huancayo", ]
ggplot(temp3, aes(x = Mes, y = ranking, group = caja))+
  geom_line(aes(linetype = caja, color = caja), size = 1)+
  geom_point(aes(color = caja, shape = caja), size = 2)+
  theme(legend.title = element_blank())+
  labs(x = "",
       y = "Porcentaje",
       title = "Participación de mercado 2020 (%)")
ggsave("plots/C-1205-Participacion de mercado.jpg", width = 8, height = 6.5)



