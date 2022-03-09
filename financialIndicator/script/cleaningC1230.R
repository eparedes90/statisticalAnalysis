#Morosidad por días de uncumplimiento

rm(list = ls())

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(tseries)
library(ggplot2)
library(forecast)
library(fpp2)

theme_set(theme_bw())

basedir <- "/raw/C-1230-/"
months <- list.files(paste0(getwd(), basedir), full.names = F, recursive = F)
length(months)
months[1]
cajas <- c("CMAC Arequipa",
           "CMAC Huancayo")

for (n in 1:length(months)){
  data <- read_xls(paste0("raw/C-1230-/", months[n]))
  data <- data[, c(1, 6)]
  colnames(data) <- data[3, ]
  
  for (i in 1:length(cajas)){
    temp1 <- data[data$Empresas == cajas[i], ]
    if (i == 1){
      temp2 <- temp1
    } else {
      temp2 <- rbind(temp2, temp1)
    }
  }
  temp2 <- temp2[!is.na(temp2$Empresas), ]
  period <- unlist(strsplit(months[n], split = "_"))
  temp2$year <- period[1]
  temp2$mes <- period[2]
  temp2$mes <- gsub(temp2$mes, pattern = ".xls", replacement = "")
  
  if (n == 1){
    temp3 <- temp2
  } else {
    temp3 <- rbind(temp3, temp2)
  }
}

#temp 3 = long! (this is for ggplot2)
MorosidadSBS <- pivot_wider(temp3, names_from = Empresas, values_from = `Morosidad según criterio contable SBS2/`)
#format
MorosidadSBS[, ] <- apply(MorosidadSBS[, ], 2, function(x) as.numeric(x))
#order by year and month
MorosidadSBS <- MorosidadSBS[order(MorosidadSBS$year, MorosidadSBS$mes), ]

MorosidadSBS$Mes <- paste(MorosidadSBS$year, MorosidadSBS$mes, 1, sep ="-")
MorosidadSBS$Mes <- as.Date(MorosidadSBS$Mes, format = "%Y-%m-%d")

#return to table temp2 
temp2 <- pivot_longer(MorosidadSBS, cols = 3:4, names_to = "caja", values_to = "morosidad")

#ggplot2
ggplot(temp2, aes(x = Mes, y = morosidad, group = caja))+
  geom_line(aes(linetype = caja, color = caja), size = 1)+
  geom_point(aes(color = caja, shape = caja), size = 2)+
  theme(legend.title = element_blank())+
  labs(x = "",
       y = "Porcentaje",
       title = "Morosidad criterio SBS")
ggsave("plots/C-1230-MorosidadSBS.jpg", width = 8, height = 6.5)



