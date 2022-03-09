#Morosidad por sector econ?mico
#Cr?ditos Directos Corporativos, a Grandes, Medianas, Peque?as y 
#a Microempresas  por Sector Econ?mico y  Caja Municipal

rm(list = ls())

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(tseries)
library(ggplot2)
library(forecast)
library(fpp2)
library(ggpubr)
library(scales)
theme_set(theme_pubr())
theme_set(theme_bw())

basedir <- "/raw/C-1216-/"
months <- list.files(paste0(getwd(), basedir), full.names = F, recursive = F)

for (i in 1:length(months)){
  temp <- read_xls(paste0("raw/C-1216-/", months[i]))
  temp <- temp[, c(1, 2, 3, 5, 9)]
  colnames(temp) <- temp[5, ]
  temp <- temp[27, ]
  
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

#ggplot
temp3 <- pivot_longer(temp2, cols = 2:5, names_to = "caja", values_to = "morosidad")

temp3$Mes <- paste(temp3$year, temp3$mes, 1, sep ="-")
temp3$Mes <- as.Date(temp3$Mes, format = "%Y-%m-%d")
temp3$morosidad <- temp3$morosidad/1000
temp3 <- temp3 <- temp3[temp3$caja == "CMAC Arequipa"|
                          temp3$caja == "CMAC Huancayo", ]

millones <- ggplot(temp3, aes(x = Mes, y = morosidad, group = caja))+
  geom_line(aes(linetype = caja, color = caja), size = 1)+
  geom_point(aes(color = caja, shape = caja), size = 2)+
  theme(legend.title = element_blank())+
  labs(x = "",
       y = "Millones de soles",
       title = "Créditos Directos",
       subtitle = "Corporativos, Grandes, Medianas, Pequeñas y Microempresas")
millones
ggsave("plots/C-1216-CreditosDirectos.jpg", width = 8, height = 6.5)

###cambiar a variaci?n porcentual
temp4 <- temp2
temp4$`CMAC Arequipa`[13:69] <- (temp4$`CMAC Arequipa`[13:69]/temp4$`CMAC Arequipa`[1:57]-1)*100 
temp4$`CMAC Cusco`[13:69] <- (temp4$`CMAC Cusco`[13:69]/temp4$`CMAC Cusco`[1:57]-1)*100 
temp4$`CMAC Huancayo`[13:69] <- (temp4$`CMAC Huancayo`[13:69]/temp4$`CMAC Huancayo`[1:57]-1)*100 
temp4$`CMAC Piura`[13:69] <- (temp4$`CMAC Piura`[13:69]/temp4$`CMAC Piura`[1:57]-1)*100 


#pivot temp3 to temp4 to make the plot
temp4 <- pivot_longer(temp4, cols = 2:5, names_to = "caja", values_to = "morosidad_var")
temp4$Mes <- paste(temp4$year, temp4$mes, 1, sep ="-")
temp4$Mes <- as.Date(temp4$Mes, format = "%Y-%m-%d")
temp4 <- temp4[temp4$caja == "CMAC Arequipa"|
                 temp4$caja == "CMAC Huancayo", ]

varia <- ggplot(temp4[temp4$Mes>="2017-01-01",], aes(x = Mes, y = morosidad_var, group = caja))+
  geom_line(aes(linetype = caja, color = caja), size = 1)+
  geom_point(aes(color = caja, shape = caja), size = 2)+
  theme(legend.title = element_blank())+
  scale_y_continuous(breaks = c(0, 20.75, 41.5, 62.25))+
  labs(x = "",
       y = "Variación porcentual")
varia
ggsave("plots/C-1216-CreditosDirectos_var.jpg", width = 8, height = 6.5)

grid_creditos <- ggarrange(millones, varia,
                    labels = c("", ""),
                    common.legend = T,
                    legend = "right",
                    nrow = 2)
grid_creditos
ggsave("plots/C-1216-CreditosDirectos_2.png", width = 8, height = 6.5)
