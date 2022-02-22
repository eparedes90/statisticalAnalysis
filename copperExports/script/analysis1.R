##########
#####4/11

rm(list=ls())

library(tidyverse)
library(readxl)
library(tseries)
library(ggplot2)
library(stargazer)
library(xtable)
library(AER)
library(dynlm)
library(scales)
library(quantmod)
library(urca)
library(tsoutliers)
library(strucchange)

theme_set(theme_classic())

#read
data <- readRDS("forAnalysis/dataClean.Rdata")
data <- na.omit(data[!data$year == 21 &
               !data$year == 20, ])

#an?lisis
#DF aumentado
lapply(na.omit(data[, 8:12]), adf.test)
"Pasar todas las variables a variaciones porcentuales"

#resumen de variables
stargazer(data[, 8:12], 
          type = "html",
          out = "plots/estadDescriptivos.htm")

#correlaciones
"Existe una correlaci?n media (0.64) entre el volumen exportado
del cobre y el crecimiento del pbi de China"
corr <- round(data.frame(cor(data[, 8:12])), 2)
corr
corr <- xtable(corr)
xtable::print.xtable(corr,
                     type = "html",
                     file = "plots/correlaciones.htm")
pairs(data[, 8:12])
savePlot(filename = "plots/correlationsGraph.jpg",
         type = "jpg")

#ts objects
data_ts <- ts(data[, 8:12], start = c(2000,2), frequency = 4)
data_ts
plot(data_ts)
plot(data$pbiChina)
mean(data$pbiChina)

#######
##AN?LISIS ECONOM?TRICO
######

#modelo1
m1 <- dynlm(data = data_ts, formula = volCobre_var ~ 
              pbiChina_var + precioCobre_var + tipoCambio_var + ied_var)
summary(m1)

stargazer(m1,
         align=TRUE, 
         type="html", 
         label = "m1", out="plots/m1.htm", header = FALSE)

#multicolinealidad
car::vif(m1)

#autocorrelaci?n, acf y pacf
plot(residuals(m1), ylab = "residuos")
savePlot(filename = "plots/residuosm1.jpg",
         type = "jpg")
acf(residuals(m1), main = "")
savePlot(filename = "plots/acfm1.jpg",
         type = "jpg")
pacf(residuals(m1), main = "")
savePlot(filename = "plots/pacfm1.jpg",
         type = "jpg")
dwtest(m1)

#normalidad
jarque.bera.test(residuals(m1))
plot(density(residuals(m1)))
savePlot(filename = "plots/normalidadm1.jpg",
         type = "jpg")

#bptest
lmtest::bptest(m1)

####
#modelo 2
m2 <- dynlm(data = data_ts, formula = volCobre_var ~ pbiChina_var)
summary(m2)

stargazer(m1,
          align=TRUE, 
          type="html", 
          label = "m2", out="plots/m2.htm", header = FALSE)

#autocorrelaci?n, acf y pacf
plot(residuals(m2))
acf(residuals(m2))
pacf(residuals(m2))
dwtest(m2)

#normalidad
jarque.bera.test(residuals(m2))
plot(density(residuals(m2)))

###
#modelo 3

#reducci?n de datos
data_ts2 <- ts(data[-c(1:8), 8:12], start = c(2002,2), frequency = 4)
data_ts2
plot(data_ts2)

m3 <- dynlm(data = data_ts2, formula = volCobre_var ~ 
              pbiChina_var + precioCobre_var + tipoCambio_var + ied_var)
summary(m3)
stargazer(m3,
          align=TRUE, 
          type="html", 
          label = "m3", out="plots/m3.htm", header = FALSE)
#multicolinealidad
car::vif(m3)

#autocorrelaci?n, acf y pacf
plot(residuals(m3), ylab = "residuos")
savePlot(filename = "plots/residuosm3.jpg",
         type = "jpg")
acf(residuals(m3), main = "")
savePlot(filename = "plots/acfm3.jpg",
         type = "jpg")
pacf(residuals(m3), main = "")
savePlot(filename = "plots/pacfm3.jpg",
         type = "jpg")
dwtest(m3)

#normalidad
jarque.bera.test(residuals(m3))
plot(density(residuals(m3)))
savePlot(filename = "plots/normalidadm3.jpg",
         type = "jpg")

#bptest
lmtest::bptest(m3)

#modelo4
m4 <- dynlm(data = data_ts2, formula = volCobre_var ~ 
              pbiChina_var)
summary(m4)

#modelo5
m5 <- dynlm(data = data_ts2, formula = volCobre_var ~ 
              pbiChina_var +
              L(volCobre_var, c(1, 2)) + 
              precioCobre_var + 
              tipoCambio_var + 
              ied_var)
summary(m5)

acf(residuals(m5))
pacf(residuals(m5))
dwtest(m5)

m6 <- dynlm(data = data_ts2, formula = volCobre_var ~ 
              L(volCobre_var, 1) +
              pbiChina_var + precioCobre_var + tipoCambio_var + ied_var)
summary(m6)
stargazer(m6,
          align=TRUE, 
          type="html", 
          label = "m6", out="plots/m6.htm", header = FALSE)
#multicolinealidad
car::vif(m6)

#autocorrelaci?n, acf y pacf
plot(residuals(m6), ylab = "residuos")
savePlot(filename = "plots/residuosm6.jpg",
         type = "jpg")
acf(residuals(m6), main = "")
savePlot(filename = "plots/acfm6.jpg",
         type = "jpg")
pacf(residuals(m6), main = "")
savePlot(filename = "plots/pacfm6.jpg",
         type = "jpg")
dwtest(m6)

#normalidad
jarque.bera.test(residuals(m6))
plot(density(residuals(m6)))
savePlot(filename = "plots/normalidadm6.jpg",
         type = "jpg")

#bptest
lmtest::bptest(m6)

#####
#cambiar rango
#ts objects
data_ts3 <- ts(data[-c(1:15), 8:12], start = c(2004,1), frequency = 4)
data_ts3
plot(data_ts3)
savePlot(filename = "plots/seriesGraph2.jpg",
         type = "jpg")

#resumen de variables
stargazer(data[-c(1:15), 8:12], 
          type = "html",
          out = "plots/estadDescriptivos2.htm")

#correlaciones
"Existe una correlaci?n media (0.64) entre el volumen exportado
del cobre y el crecimiento del pbi de China"
corr <- round(data.frame(cor(data[-c(1:15), 8:12])), 2)
corr <- xtable(corr)
xtable::print.xtable(corr,
                     type = "html",
                     file = "plots/correlaciones2.htm")
pairs(data[-c(1:15), 8:12])
savePlot(filename = "plots/correlationsGraph2.jpg",
         type = "jpg")

#modelo 7
m7 <- dynlm(data = data_ts3, formula = volCobre_var ~ 
              pbiChina_var + precioCobre_var + tipoCambio_var + ied_var)
summary(m7)
stargazer(m7,
          align=TRUE, 
          type="html", 
          label = "m7", out="plots/m7.htm", header = FALSE)

#multicolinealidad
car::vif(m7)

#autocorrelaci?n, acf y pacf
plot(residuals(m7), ylab = "residuos")
savePlot(filename = "plots/residuosm7.jpg",
         type = "jpg")
acf(residuals(m7), main = "")
savePlot(filename = "plots/acfm7.jpg",
         type = "jpg")
pacf(residuals(m7), main = "")
savePlot(filename = "plots/pacfm7.jpg",
         type = "jpg")
dwtest(m7)

#normalidad
jarque.bera.test(residuals(m7))
plot(density(residuals(m7)))
savePlot(filename = "plots/normalidadm7.jpg",
         type = "jpg")

#bptest
lmtest::bptest(m7)

#chow test
sctest(m7,
       type = "Chow")

#cusum test
cusum <- efp(volCobre_var ~ pbiChina_var + precioCobre_var + tipoCambio_var + ied_var, data =  data_ts3,type = "Rec-CUSUM")
plot(cusum)
savePlot(filename = "plots/cusum_m7.jpg",
         type = "jpg")


#m8
m8 <- dynlm(data = data_ts3, formula = volCobre_var ~ 
              pbiChina_var + L(volCobre_var, 1:3) + precioCobre_var + tipoCambio_var + ied_var)
summary(m8)
stargazer(m8,
          align=TRUE, 
          type="html", 
          label = "m8", out="plots/m8.htm", header = FALSE)

m9 <- dynlm(data = data_ts3, formula = volCobre_var ~ 
              pbiChina_var + L(volCobre_var, 1) + precioCobre_var + tipoCambio_var + ied_var)
summary(m9)
stargazer(m9,
          align=TRUE, 
          type="html", 
          label = "m9", out="plots/m9.htm", header = FALSE)


#autocorrelaci?n, acf y pacf
plot(residuals(m8), ylab = "residuos")
savePlot(filename = "plots/residuosm7.jpg",
         type = "jpg")
acf(residuals(m8), main = "")
savePlot(filename = "plots/acfm7.jpg",
         type = "jpg")
pacf(residuals(m8), main = "")
savePlot(filename = "plots/pacfm7.jpg",
         type = "jpg")
dwtest(m8)
