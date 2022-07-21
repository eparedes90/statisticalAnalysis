### analysis

rm(list = ls())

library(ggplot2)
library(lmtest)
library(stargazer)

setwd("E:/Asesor??as de tesis/Econom??a/Asociatividad y servicios financieros")
data <- readRDS("forAnalysis/data2.Rdata")
theme_set(theme_bw())

#as factors
data[, ] <- apply(data[, ], 2, function(x) as.factor(x))

#as numeric
numeric <- colnames(data[, c("P101", "P104_SUP_ha")])
data[, numeric] <- apply(data[, numeric], 2, function(x) as.numeric(x))

#limitar a los peque??os y medianos agricultores (menos de 50ha)
data <- data[data$P104_SUP_ha<=50, ]

#solo creditos formales
data <- data[!data$credFormal == 2, ]

#analisis descriptivo
str(data)

#P15: idioma de la entrevista
ggplot(data, aes(P15, fill = credFormal)) +
  geom_bar(position = "dodge2") +
  ylab("")
table(data$P15, data$credFormal)
prop.table(table(data$P15, data$credFormal))

#P101: a??os trabajando sus tierras
ggplot(data, aes(credFormal, P101)) +
  geom_boxplot()

#P102
table(data$P102_1)
#100% utilizan sus tierras para agricultura
table(data$P102_2, data$credFormal)
prop.table(table(data$P102_2))
19224/25364
#75.79% utilizan sus tierras para un fin agropecuario
table(data$P102_3)
#Todos realizaron alguna actividad sea agr??cola o pecuaria

#P104_SUP_ha
ggplot(data, aes(credFormal, P104_SUP_ha)) +
  geom_boxplot()
ggsave(filename = "plots/P104_SUP_ha.png")

#prop
ggplot(data, aes(prop, fill = credFormal)) + 
  geom_bar(position = "dodge")+
  ylab("")
ggsave(filename = "plots/prop.png")






table(data$P901)
data[data == "No"] <- 0
data[data == "S???"] <- 1
table(data$P1109A)

table(data$P101A)



str(data)
colnames(data)
stargazer::stargazer(data[, "P1109A"], align=TRUE, type="html", out="plots/descrip.htm", header = FALSE)

table(data$P1109A)
