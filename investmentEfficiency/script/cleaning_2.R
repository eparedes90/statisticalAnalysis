####TESIS ADRIANA ACOSTA
####EFIENCIA GASTO PUBLICO EDUCACION REGIONES PERU
#### 1/10/2021

rm(list = ls())

library(foreign)
library(plm)
library(xtable)
library(rDEA)
library(ggplot2)
library(psych)
library(tidyr)

setwd("E:/Asesorías de tesis/Economía/Eficiencia del gasto público en la educación")
ggplot2::theme_set(theme_classic())

#read and clean the data
baseDir <- getwd()
archivo <- list.files(paste(baseDir, "data2", sep = "/"), pattern = ".xlsx")

#input:hojas de excel
#out:hojas pivoteadas y merged por Departamento y año
for (i in 1:12){
  temp <- readxl::read_xlsx(paste(baseDir, 
                                  "data2",
                                  archivo[i], 
                                  sep = "/"))
  temp <- pivot_longer(temp, 
                       cols = 2:length(temp), 
                       names_to = "year", 
                       values_to = archivo[i])
  if (i == 1){
    data <- temp
    } else{
      data <- merge(data, 
                    temp, 
                    by = c("Departamento", "year"))
    }
}

#Dea robust for each year

for (i in c("2015", 
            "2016",
            "2018")){ 
    Y <- data[data$year == i, 
              c("compLectora.xlsx",
              "matematica.xlsx",
              "tasaMatricula.xlsx",
              "tasaConclusion.xlsx")]
    X <- data[data$year == i, "gastoPublico.xlsx"]
    
    dea <- dea.robust(X = X, 
                      Y = Y,
                      model = "output")
    
    temp2 <- data[data$year == i, ]
    
    temp2$deaNaive <- dea$theta_hat
    temp2$deaRobust <- dea$theta_hat_hat
    temp2$ciLow <- dea$theta_ci_low
    temp2$ciHigh <- dea$theta_ci_high
    
    if (i == "2015"){
      data2 <- temp2
    } else {
      data2 <- rbind(data2, temp2)
    }
}

write.csv(data2, file = "for_analysis/data2Clean.csv")
saveRDS(data2, file = "for_analysis/data2Clean.Rdata")

