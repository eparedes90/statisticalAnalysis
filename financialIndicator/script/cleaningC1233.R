#C-1233- Estructura de Cr?ditos Directos y Contingentes por Tipo de Cr?dito y Categor?a de Riesgo del Deudor

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

basedir <- "/raw/C-1233-/"
months <- list.files(paste0(getwd(), basedir), full.names = F, recursive = F)

for (i in 1:length(months)){
  temp <- read_xls(paste0("raw/C-1233-/", months[i]))
  colnames(temp) <- temp[4, ]
  colnames(temp)
  temp <- temp[c(26:38), c(1,2, 4, 5, 7, 11)]
  temp[2:6, 1] <- temp[1, 1]
  temp[9:13, 1] <- temp[8, 1]
  temp <- temp[-7, ]
  period <- unlist(strsplit(months[i], split = "_"))
  temp$year <- period[1]
  temp$mes <- period[2]
  temp$mes <- gsub(temp$mes, pattern = ".xls", replacement = "")
  
  if (i == 1){
    temp4 <- temp
  } else {
    temp4 <- rbind(temp4, temp)
  }
}

#format
temp4[, 3:8] <- apply(temp4[, 3:8], 2, function(x) as.numeric(x))
options(scipen = 999)
#order
temp4 <- temp4[order(temp4$year, temp4$mes), ]
#formato fecha
temp4$Mes <- paste(temp4$year, temp4$mes, 1, sep ="-")
temp4$Mes <- as.Date(temp4$Mes, format = "%Y-%m-%d")

#more cleaning
temp4 <- temp4[!temp4$`Categoría de Riesgo` == "Peq. Empr. (Miles S/.)", ]
temp4 <- temp4[!temp4$`Categoría de Riesgo` == "Peq. Empr. (Miles S/)", ]
temp4 <- temp4[!temp4$`Categoría de Riesgo` == "Microempr. (Miles S/.)", ]
temp4 <- temp4[!temp4$`Categoría de Riesgo` == "Microempr. (Miles S/)", ]
temp4$`Categoría de Riesgo`[temp4$`Categoría de Riesgo`== "Pérdida"] <- "Perdida"
temp4 <- pivot_longer(temp4, cols = 3:6, names_to = "caja", values_to = "estructura")
temp4$caja[temp4$caja == "CMAC AREQUIPA"] <- "CMAC Arequipa"
temp4$caja[temp4$caja == "CMAC CUSCO S A"] <- "CMAC Cusco"
temp4$caja[temp4$caja == "CMAC PIURA"] <- "CMAC Piura"
temp4$caja[temp4$caja == "CMAC HUANCAYO"] <- "CMAC Huancayo"

##ggplot
categ <- unlist(unique(temp4$`Tipo de Crédito`))
categ

empty_list <- vector(mode = "list", len = 0)
empty_list

table(temp4$`Categoría de Riesgo`)

categ2 <- unlist(unique(temp4$`Categoría de Riesgo`))
temp4 <- temp4[temp4$caja == "CMAC Arequipa"|
                 temp4$caja == "CMAC Huancayo", ]

for (n in 1:length(categ2)){
  for (i in 1:length(categ)){
    temp5 <- temp4[temp4$`Tipo de Crédito` == categ[i] &
                     temp4$`Categoría de Riesgo`== categ2[n], ]
    empty_list[[i]] <- local({
      i <- i
      a <- ggplot(temp5, aes(x = Mes, y = estructura, group = caja))+
        geom_line(aes(linetype = caja, color = caja), size = 1)+
        geom_point(aes(color = caja, shape = caja), size = 2)+
        theme(legend.title = element_blank())+
        labs(x = "",
             y = "Porcentaje",
             title = paste("Categoría de Riesgo", categ2[n],
                           "-",
                           categ[i],
                           sep = " "))
      print(a)
    })
  }
  
  grid_creditos <- ggarrange(empty_list[[1]], empty_list[[2]],
                             labels = c("", ""),
                             common.legend = T,
                             legend = "right",
                             nrow = 2)
  grid_creditos
  ggsave(paste("plots/C-1233-", categ2[n], "_2.png", sep = ""), width = 8, height = 6.5)

}


for (i in 1:length(categ)){
  temp5 <- temp4[temp4$`Categoría de Riesgo` == categ[i] &
                   temp4$`Tipo de Crédito` == "Microempresas", ]
  ggplot(temp5[temp5$Mes >= "2019-01-01", ], aes(x = Mes, y = estructura, group = caja))+
    geom_line(aes(linetype = caja, color = caja), size = 1)+
    geom_point(aes(color = caja, shape = caja), size = 2)+
    theme(legend.title = element_blank())+
    labs(x = "",
         y = "Porcentaje",
         title = "Estructura de Créditos Directos y Contingentes - Microempresas",
         subtitle = paste("Categoría de Riesgo", categ[i], sep = " "))
  ggsave(filename = paste("plots/C-1233-Microempresas", categ[i], ".jpg", sep = ""),
         plot = last_plot(),
         device = "jpg",
         height = 6,
         width = 8)
}
