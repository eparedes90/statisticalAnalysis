#####
#####TESIS ADRIANA EDUCACION
#####UESAN
#####24/09/21

rm(list = ls())

library(foreign)
library(plm)
library(xtable)
library(ggplot2)
library(tidyr)

setwd("E:/Asesorías de tesis/Economía/Educación_Adriana/")
ggplot2::theme_set(theme_classic())

data <- readRDS("for_analysis/cleandata.Rdata")
data$dl_gdp[2:72] <- diff(log(data$gdp))
data$dl_rural[2:72] <- diff(log(data$rural))
data$dl_undernourishment[2:72] <- diff(log(data$undernourishment))

data <- plm::pdata.frame(data[!data$year==2009, ],
                 index = c("country", "year"))

#descriptive
cor(data[, c(7, 13:15)])

#plm
m_plm_1 <- plm(data = data,
               formula = dea1.theta_hat_hat ~
                 dl_undernourishment +
                 dl_rural +
                 dl_gdp,
               model = "within")
summary(m_plm_1)

#eficiencia ~ beta(i) + rural + gpd + un + error

#eficiencia ~ rural + gpd + un + error(temporal + individual)
m_plm_2 <- plm(data = data,
               formula = dea1.theta_hat_hat ~
                 dl_undernourishment +
                 dl_rural +
                 dl_gdp,
               model = "random")
summary(m_plm_2)
