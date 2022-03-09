#BSFCV19- Reactiva
#Saldos de cr?ditos desembolsados en el marco de REACTIVA PER?
#(En millones de Soles)

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

months <- list.files("raw/BSFCV19-", full.names = F, recursive = F)

for (i in 2:length(months)){
  temp <- read_xlsx(paste0("raw/BSFCV19-/", months[2]), sheet= 10)
  colnames(temp) <- temp[4, ]
  colnames(temp)
  temp <- temp[c(36, 37, 39, 43), c(1, 5, 6)]
  colnames(temp)[1] <- "caja"

  period <- unlist(strsplit(months[i], split = "_"))
  temp$year <- period[1]
  temp$mes <- period[2]
  temp$mes <- gsub(temp$mes, pattern = ".xls", replacement = "")
  
  if (i == 2){
    temp2 <- temp
  } else {
    temp2 <- rbind(temp2, temp)
  }
}

temp <- read_xlsx(paste0("raw/BSFCV19-/", months[1]), sheet= 9)
colnames(temp) <- temp[4, ]
colnames(temp)
temp <- temp[c(36, 37, 39, 43), c(1, 5, 6)]
colnames(temp)[1] <- "caja"

period <- unlist(strsplit(months[1], split = "_"))
temp$year <- period[1]
temp$mes <- period[2]
temp$mes <- gsub(temp$mes, pattern = ".xls", replacement = "")

temp2 <- rbind(temp2, temp)

#pivot
temp3 <- pivot_longer(temp2, cols = 2:3, values_to = "Saldos", names_to = "Categoria")
temp3 <- pivot_wider(temp3, names_from = "caja", values_from = "Saldos")

#format
temp3[, 4:7] <- apply(temp3[, 4:7], 2, function(x) as.numeric(x))

#order
temp3 <- temp3[order(temp3$year, temp3$mes), ]

#formato fecha
temp3$Mes <- paste(temp3$year, temp3$mes, 1, sep ="-")
temp3$Mes <- as.Date(temp3$Mes, format = "%Y-%m-%d")

write.csv(temp3, "forAnalysis/BSFCV19.csv")

#ggplot

temp2$Mes <- paste(temp2$year, temp2$mes, 1, sep ="-")
temp2$Mes <- as.Date(temp2$Mes, format = "%Y-%m-%d")
temp2[, 2:3] <- apply(temp2[, 2:3], 2, function(x) as.numeric(x))
temp2 <- temp2[temp2$caja == "CMAC Arequipa"|
                 temp2$caja == "CMAC Huancayo", ]

ggplot(temp2[temp2$Mes >= "2019-01-01", ], aes(x = Mes, y = `Pequeñas empresas`, group = caja))+
  geom_line(aes(linetype = caja, color = caja), size = 1)+
  geom_point(aes(color = caja, shape = caja), size = 2)+
  theme(legend.title = element_blank())+
  labs(x = "",
       y = "Millones de soles",
       title = "Saldos de creditos desembolsados Reactiva Perú",
       subtitle = "Pequeñas Empresas")
ggsave("plots/BSFCV19-PeqEmpresas.jpg", width = 8, height = 6.5)

ggplot(temp2[temp2$Mes >= "2019-01-01", ], aes(x = Mes, y = `Microempresas`, group = caja))+
  geom_line(aes(linetype = caja, color = caja), size = 1)+
  geom_point(aes(color = caja), size = 2)+
  theme(legend.title = element_blank())+
  labs(x = "",
       y = "Millones de soles",
       title = "Saldos de creditos desembolsados Reactiva Perú",
       subtitle = "Microempresas") 
ggsave("plots/BSFCV19-Microempresas.jpg", width = 8, height = 6.5)

