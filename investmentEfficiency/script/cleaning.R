####
####TESIS 

####EFICIENCIA EN EL GASTO PUBLICO EN EDUCACIÓN
####ADRIANA ACOSTA
####22/09/21

rm(list = ls())

library(foreign)
library(plm)
library(xtable)
library(rDEA)
library(ggplot2)
library(psych)
library(tidyr)

setwd("E:/Asesorías de tesis/Economía/Educación_Adriana/")
ggplot2::theme_set(theme_classic())

#read and clean the data
data <- readxl::read_xlsx("data/raw.xlsx")
un <- readxl::read_xls("data/undernourishment.xls")
gdp <- readxl::read_xls("data/gdp.xls")
rural <- readxl::read_xls("data/rural.xls")

colnames(un)
#pivot un, gdp, ru
un <- un[, c(1, 54:62)] %>%
  pivot_longer(cols = colnames(un)[54:62], names_to = "year", values_to = "undernourishment")
colnames(un)[1] <- "country"

rural <- rural[, c(1, 54:62)] %>%
  pivot_longer(cols = colnames(rural)[54:62], names_to = "year", values_to = "rural")
colnames(rural)[1] <- "country"

gdp <- gdp[, c(1, 54:62)] %>%
  pivot_longer(cols = colnames(gdp)[54:62], names_to = "year", values_to = "gdp")
colnames(gdp)[1] <- "country"

#merge rural, gdp, un
second <- merge(rural, gdp, by = c("country", "year"))
second <- merge(second, un, by = c("country", "year"))

#clean data
str(data)
colnames(data)
data <- data[1:285, c(1:3, 44:52)]
data[, 4:12] <- apply(data[, 4:12], 2, function(x) as.numeric(x))
data <- data[data$`Country Name` == "Argentina"|
               data$`Country Name` == "Chile"|
               data$`Country Name` == "Colombia"|
               data$`Country Name` == "Ecuador"|
               data$`Country Name` == "El Salvador"|
               data$`Country Name` == "Guatemala"|
               data$`Country Name` == "Mexico"|
               data$`Country Name` == "Peru",
             ]

#build the data
colnames(data)
for (i in 4:12){
  forone <- data[, c(1:3, i)]
  forone <- pivot_wider(forone, names_from = "Series", values_from = colnames(forone)[4])
  dea1 <- dea.robust(X = forone$`Government expenditure on secondary education, constant PPP$ (millions)`,
                     Y = forone$`Gross enrolment ratio, lower secondary, both sexes (%)`,
                     model = "output")
  temp1 <- data.frame(forone[, c(1, 10, 15, 16)], dea1$theta_hat, dea1$theta_hat_hat, dea1$theta_ci_high, dea1$theta_ci_low)
  year <- c(2006:2017)
  temp1$temp2 <- year[i]
  colnames(temp1)[9]<-"year"
  colnames(temp1)[1]<-"country"
  
  #rbind
  if (i == 4){
    data2 <- temp1
  } else {
    data2 <- rbind(data2, temp1)
  }
}

#merge with second
data2 <- merge(data2, second, by= c("country", "year"))

write.csv(data2, "for_analysis/cleandata.csv")
saveRDS(data2, "for_analysis/cleandata.Rdata")
