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
library(stargazer)
library(stringr)
library(car)

setwd("E:/Asesor�as de tesis/Econom�a/Eficiencia del gasto p�blico en la educaci�n")
ggplot2::theme_set(theme_classic())
options(scipen = 999)

#abrir la bbdd and merge with AlumnoDocente
data <- readRDS("for_analysis/data2Clean.Rdata")
ad <- readRDS("E:/Asesor�as de tesis/Econom�a/Eficiencia del gasto p�blico en la educaci�n/for_analysis/ad.Rdta")
data <- merge(data, ad, by=c("Departamento", "year"))
data$nivelUrb <- data$pobUrb.xlsx/data$pobTot.xlsx*100

temp <- colnames(data)
temp <- gsub(".xlsx", "", temp)
colnames(data) <- temp

#CAMBIO alumDoc por alumDoc
data$alumDoc <- data$alumDoc
colnames(data)[19] <- "alumDoc"

#ESTAD�STICA DECRIPTIVA
#estad�sticos descriptivos
#Tabla resumen 2015
stargazer(data[data$year == 2015, c(7, 5, 8, 14, 12)],
          align = TRUE, 
          type = "html", 
          label = "resumen", 
          out= "plots/resumenPrimeraEtapa15.htm", 
          header = FALSE)
#Tabla resumen 2016
stargazer(data[data$year == 2016, c(7, 5, 8, 14, 12)],
          align = TRUE, 
          type = "html", 
          label = "resumen", 
          out= "plots/resumenPrimeraEtapa16.htm", 
          header = FALSE)
#Summary 2018 deaboots
stargazer(data[data$year == 2018, c(7, 5, 8, 14, 12)],
          align = TRUE, 
          type = "html", 
          label = "resumen", 
          out= "plots/resumenPrimeraEtapa18.htm", 
          header = FALSE)

#Eficiency scores by year 
colnames(data)
eficiency <- data[, c(1,2, 15:18)]
eficiency$bias <- eficiency$deaRobust - eficiency$deaNaive
eficiency <- eficiency[, c(1:4, 7, 5, 6)]
eficiency[, 3:7] <- round(eficiency[, 3:7], 3)

ef15 <- eficiency[eficiency$year == "2015", ]
ef15 <- xtable(ef15)
print.xtable(ef15, type = "html", file = "plots/eficiency15.htm")

ef16 <- eficiency[eficiency$year == "2016", ]
ef16 <- xtable(ef16)
print.xtable(ef16, type = "html", file = "plots/eficiency16.htm")

ef18 <- eficiency[eficiency$year == "2018", ]
ef18 <- xtable(ef18)
print.xtable(ef18, type = "html", file = "plots/eficiency18.htm")

eficiency2 <- pivot_wider(eficiency[, c(1, 2, 4)], names_from = "year", values_from = "deaRobust") 
eficiency2$Promedio <- round(rowMeans(eficiency2[, 2:4]), 3)
eficiency2 <- eficiency2[order(eficiency2$Promedio, decreasing = T), ]
eficiency2$Ranking <- 1:26
eficiency2 <- xtable(eficiency2)
print.xtable(eficiency2, type = "html", file = "plots/eficiencyRanking.htm")

#Plot Expenditure vs eficiency
ggplot(data, aes(x = data$gastoPublico, y = data$deaRobust, label = Departamento))+
  geom_point()+
  labs(x = "Gasto P�blico",
       y = "")+
  geom_text(vjust = 1, nudge_x = 0.5,size = 2, check_overlap = F)

#Plot Bars decreasing deaRobust
ggplot(eficiency2, aes(x = reorder(Departamento, -Promedio), 
                       y = Promedio, label = Promedio))+geom_bar(stat = "identity")+
  geom_text(vjust = -0.5, nudge_x = 0, size = 4, check_overlap = F, color = "black")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))+
  labs(x = "",
       y = "PromedioEficiencia")
savePlot(type="jpg", filename = "plots/DecreasingDeaRobust.jpg")

##SEGUNDA ETAPA
stargazer(data[data$year == "2015", c(4, 9, 13, 16, 19, 20)],
          align = TRUE, 
          type = "html", 
          label = "resumen", 
          out= "plots/resumenSegundaEtapa15.htm", 
          header = FALSE)
stargazer(data[data$year == "2016", c(4, 9, 13, 16, 19, 20)],
          align = TRUE, 
          type = "html", 
          label = "resumen", 
          out= "plots/resumenSegundaEtapa16.htm", 
          header = FALSE)
stargazer(data[data$year == "2018", c(4, 9, 13, 16, 19, 20)],
          align = TRUE, 
          type = "html", 
          label = "resumen", 
          out= "plots/resumenSegundaEtapa18.htm", 
          header = FALSE)

# #correlaci�n
# colnames(data)
# cor <- round(cor(data[, c(4, 9, 13, 16, 19, 20)]), 2)
# cor
# cor <- xtable(cor)
# print.xtable(cor, type = "html", file = "plots/correlaciones.htm")
# pairs(data[, c(4, 9, 13, 16, 19, 20)])
# savePlot(filename = "plots/correlacionesPairs.jpg",
#          type = "jpg")
# 
# #ECONOMETRIC ANALYSIS
# #panel data
# #set panel data
# colnames(data)
# dataPlm <- plm::pdata.frame(data[, c(1, 2, 4, 9, 13, 16, 19, 20)],
#                             index = c("Departamento", 
#                                       "year"))
# #plm Within
# plm_within <- plm(data = data,
#                formula = data$deaRobust ~ data$alumDoc +
#                  data$nivelUrb +
#                  data$analfabetismo + 
#                  data$pbiRegiones+
#                  data$tasaDesnutricion,
#                model = "within")
# summary(plm_within)
# stargazer(plm_within,
#           align = T,
#           type = "html",
#           out = "plots/plm_within.htm")
# #plm random
# plm_random <- plm(data = data,
#                formula = data$deaRobust ~ data$alumDoc +
#                  data$nivelUrb +
#                  data$analfabetismo + 
#                  data$pbiRegiones +
#                  data$tasaDesnutricion,
#                model = "random")
# summary(plm_random)
# stargazer(plm_random,
#           align = T,
#           type = "html",
#           out = "plots/plm_random.htm")
# 
# ###VIF
# m_vif <- lm(data = data, data$deaRobust ~ data$analfabetismo + 
#               data$nivelUrb +
#               data$alumDoc +
#               data$tasaDesnutricion +
#               data$pbiRegiones)
# car::vif(m_vif)
# 
# m_vif2 <- lm(data = data, data$deaRobust ~ data$analfabetismo +
#               data$alumDoc +
#               data$tasaDesnutricion +
#               data$pbiRegiones)
# car::vif(m_vif2)
# 
# ##MODELO SIN NIVELURB
# plm_within_2 <- plm(data = data,
#                   formula = data$deaRobust ~ data$alumDoc +
#                     data$analfabetismo + 
#                     data$pbiRegiones+
#                     data$tasaDesnutricion,
#                   model = "within")
# summary(plm_within_2)
# 
# plm_random_2 <- plm(data = data,
#                     formula = data$deaRobust ~ data$alumDoc +
#                       data$analfabetismo + 
#                       data$pbiRegiones+
#                       data$tasaDesnutricion,
#                     model = "random")
# summary(plm_random_2)
# 
# stargazer(plm_within_2,
#           align = T,
#           type = "html",
#           out = "plots/plm_within_2.htm")
# 
# stargazer(plm_random_2,
#           align = T,
#           type = "html",
#           out = "plots/plm_random_2.htm")
# 
# #Tests
# plm::phtest(plm_within, plm_random)
# plm::pcdtest(plm_within, test = c("lm"))
# pcdtest(plm_within, test = c("cd"))
# 
# library(lmtest)
# bptest(plm_within)
# bptest(plm_within_2)
# 
# bptest(plm_random)
# bptest(plm_random_2)
# 
# 
# #Correcci�n de heteroscedasticidad White
# stargazer(lmtest::coeftest(plm_within, vcovHC(plm_within, method = "white1")), align=TRUE, type="html", label = "fix", out="plots/plm_within_corrected.htm", header = FALSE)
# stargazer(lmtest::coeftest(plm_within_2, vcovHC(plm_within_2, method = "white1")), align=TRUE, type="html", label = "fix", out="plots/plm_within_2_corrected.htm", header = FALSE)
# 
# stargazer(lmtest::coeftest(plm_random, vcovHC(plm_random, method = "white1")), align=TRUE, type="html", label = "fix", out="plots/plm_random_corrected.htm", header = FALSE)
# stargazer(lmtest::coeftest(plm_random_2, vcovHC(plm_random_2, method = "white1")), align=TRUE, type="html", label = "fix", out="plots/plm_random_2_corrected.htm", header = FALSE)
# 
# phtest(plm_within_2, plm_random_2)
# 
# ##MODELO SIN NIVELURB y CON T�RMINO AL CUADRADO
# plm_within_3 <- plm(data = data,
#                     formula = data$deaRobust ~ data$alumDoc +
#                       I(data$alumDoc^2) +
#                       data$analfabetismo + 
#                       data$pbiRegiones+
#                       data$tasaDesnutricion,
#                     model = "within")
# summary(plm_within_3)
# 
# plm_random_3 <- plm(data = data,
#                     formula = data$deaRobust ~ data$alumDoc +
#                       I(data$alumDoc^2) +
#                       data$analfabetismo + 
#                       data$pbiRegiones+
#                       data$tasaDesnutricion,
#                     model = "random")
# summary(plm_random_3)
# 
# stargazer(plm_within_3,
#           align = T,
#           type = "html",
#           out = "plots/plm_within_3.htm")
# 
# stargazer(plm_random_3,
#           align = T,
#           type = "html",
#           out = "plots/plm_random_3.htm")
# 
# 
# #Tests
# 
# plm::phtest(plm_within_3, plm_random_3)
# plm::pcdtest(plm_within_3, test = c("lm"))
# pcdtest(plm_within_3, test = c("cd"))
# 
# library(lmtest)
# bptest(plm_within)
# bptest(plm_within_2)
# bptest(plm_within_3)
# 
# bptest(plm_random)
# bptest(plm_random_2)
# bptest(plm_random_3)
# 
# 
# #Correcci�n de heteroscedasticidad White
# stargazer(lmtest::coeftest(plm_within, vcovHC(plm_within, method = "white1")), align=TRUE, type="html", label = "fix", out="plots/plm_within_corrected.htm", header = FALSE)
# stargazer(lmtest::coeftest(plm_within_2, vcovHC(plm_within_2, method = "white1")), align=TRUE, type="html", label = "fix", out="plots/plm_within_2_corrected.htm", header = FALSE)
# stargazer(lmtest::coeftest(plm_within_3, vcovHC(plm_within_3, method = "white1")), align=TRUE, type="html", label = "fix", out="plots/plm_within_3_corrected.htm", header = FALSE)
# 
# stargazer(lmtest::coeftest(plm_random, vcovHC(plm_random, method = "white1")), align=TRUE, type="html", label = "fix", out="plots/plm_random_corrected.htm", header = FALSE)
# stargazer(lmtest::coeftest(plm_random_2, vcovHC(plm_random_2, method = "white1")), align=TRUE, type="html", label = "fix", out="plots/plm_random_2_corrected.htm", header = FALSE)
# stargazer(lmtest::coeftest(plm_random_3, vcovHC(plm_random_3, method = "white1")), align=TRUE, type="html", label = "fix", out="plots/plm_random_3_corrected.htm", header = FALSE)
# 
# phtest(plm_within_3, plm_random_3)
# 



###

##AGREGAR DOS nuevas variables
#read new data3
alumDocProxy <- readxl::read_xlsx("E:/Asesor�as de tesis/Econom�a/Eficiencia del gasto p�blico en la educaci�n/data3/alumDocProxy.xlsx")
schooling <- readxl::read_xlsx("E:/Asesor�as de tesis/Econom�a/Eficiencia del gasto p�blico en la educaci�n/data3/schooling.xlsx")

#pivot longer
alumDocProxy <- pivot_longer(alumDocProxy, cols=2:4, names_to = "year", values_to = "alumDocProxy")
schooling <- pivot_longer(schooling, cols = 2:13, names_to = "year", values_to = "schooling")

#merge
data <- merge(data, alumDocProxy, by = c("Departamento", "year"))
data <- merge(data, schooling, by = c("Departamento", "year"))

###ECONOMETRIC ANALYSIS 2
colnames(data)
cor <- round(cor(data[, c(9, 16, 19, 20, 22)]), 2)
cor
cor <- xtable(cor)
print.xtable(cor, type = "html", file = "plots2/correlaciones.htm")
pairs(data[, c(9, 16, 19, 20, 22)])
savePlot(filename = "plots2/correlacionesPairs_daproxy.jpg",
         type = "jpg")
# pairs(data[, c(9, 13, 16, 19, 20, 22)])
# savePlot(filename = "plots2/correlacionesPairs_dainicial.jpg",
#          type = "jpg")

###VIF
m_vif <- lm(data = data, data$deaRobust ~ data$schooling +
              data$alumDoc +
              data$pbiRegiones+
              data$nivelUrb)
car::vif(m_vif)

m_vif2 <- lm(data = data, data$deaRobust ~ data$schooling +
               data$alumDocProxy +
               data$tasaDesnutricion +
               data$pbiRegiones+
               data$nivelUrb)
car::vif(m_vif2)

#plm data3 fixed
plm_within_data3 <- plm(data = data,
                  formula = data$deaRobust ~
                    data$alumDoc +
                    data$schooling + 
                    data$pbiRegiones +
                    data$nivelUrb,
                  model = "within")
summary(plm_within_data3)

stargazer(plm_within_data3,
          align = T,
          type = "html",
          out = "plots2/plm_within_data3.htm")

bptest(plm_within_data3)

#plm data3 random
plm_random_data3 <- plm(data = data,
                        formula = data$deaRobust ~ data$alumDoc +
                          data$schooling + 
                          data$pbiRegiones + 
                          data$nivelUrb,
                        model = "random")
summary(plm_random_data3)
stargazer(plm_random_data3,
          align = T,
          type = "html",
          out = "plots2/plm_random_data3.htm")

bptest(plm_random_data3)

phtest(plm_random_data3, plm_within_data3)

# ##alumDocProxy fixed
# plm_within_2_data3 <- plm(data = data,
#                         formula = data$deaRobust ~ data$alumDocProxy +
#                           data$schooling + 
#                           data$pbiRegiones+
#                           data$tasaDesnutricion,
#                           model = "within")
# summary(plm_within_2_data3)
# 
# stargazer(plm_within_2_data3,
#           align = T,
#           type = "html",
#           out = "plots2/plm_within_2_data3.htm")
# 
# bptest(plm_within_2_data3)
# 
# #alumDocProxy random
# plm_random_2_data3 <- plm(data = data,
#                           formula = data$deaRobust ~ data$alumDocProxy +
#                             data$schooling + 
#                             data$pbiRegiones+
#                             data$tasaDesnutricion,
#                           model = "random")
# summary(plm_random_2_data3)
# 
# stargazer(plm_random_2_data3,
#           align = T,
#           type = "html",
#           out = "plots2/plm_random_2_data3.htm")
# 
# bptest(plm_random_2_data3)
# 
# 
# ####^2
# #plmdata3 fixed sq
# plm_within_data3_2 <- plm(data = data,
#                         formula = data$deaRobust ~ data$alumDoc +
#                           I(data$alumDoc^2) +
#                           data$schooling + 
#                           data$pbiRegiones +
#                           data$nivelUrb,
#                         model = "within")
# summary(plm_within_data3_2)
# 
# stargazer(plm_within_data3,
#           align = T,
#           type = "html",
#           out = "plots2/plm_within_data3_2.htm")
# 
# bptest(plm_within_data3_2)
# 
# #plm data3 random sq
# plm_random_data3_2 <- plm(data = data,
#                         formula = data$deaRobust ~ data$alumDoc +
#                           I(data$alumDoc^2) +
#                           data$schooling + 
#                           data$pbiRegiones +
#                           data$nivelUrb,
#                         model = "random")
# summary(plm_random_data3_2)
# stargazer(plm_random_data3_2,
#           align = T,
#           type = "html",
#           out = "plots2/plm_random_data3_2.htm")
# 
# bptest(plm_random_data3_2)
# 
# ##alumDocProxy fixed sq
# plm_within_2_data3_2 <- plm(data = data,
#                           formula = data$deaRobust ~ data$alumDocProxy +
#                             I(data$alumDocProxy^2) +
#                             data$schooling + 
#                             data$pbiRegiones+
#                             data$tasaDesnutricion,
#                           model = "within")
# summary(plm_within_2_data3_2)
# 
# stargazer(plm_within_2_data3_2,
#           align = T,
#           type = "html",
#           out = "plots2/plm_within_2_data3_2.htm")
# 
# bptest(plm_within_2_data3_2)
# 
# #alumDocProxy random sq
# plm_random_2_data3_2 <- plm(data = data,
#                           formula = data$deaRobust ~ data$alumDocProxy +
#                             I(data$alumDocProxy^2)+
#                             data$schooling + 
#                             data$pbiRegiones+
#                             data$tasaDesnutricion,
#                           model = "random")
# summary(plm_random_2_data3_2)
# 
# stargazer(plm_random_2_data3_2,
#           align = T,
#           type = "html",
#           out = "plots2/plm_random_2_data3_2.htm")
# 
# bptest(plm_random_2_data3_2)
# 
# plot(data$deaRobust)

ggplot(data, aes(y = deaRobust, x = alumDoc, label = Departamento)) +
  geom_point() +
  geom_text(aes(label = Departamento))+
  geom_smooth(method = "lm", se = F)
ggsave(filename = "plots2/alumDoc_dea.jpg", plot = last_plot())

ggplot(data, aes(y = nivelUrb, x = alumDoc, label = Departamento)) +
  geom_point() +
  geom_text(aes(label = Departamento))+
  geom_smooth(method = "lm", se = F)
ggsave(filename = "plots2/alumDoc_urb.jpg", plot = last_plot())

write.csv(data, "for_analysis/cleanData2.csv")
