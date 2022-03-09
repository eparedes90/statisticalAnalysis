####
####

rm(list=ls())

library(ggplot2)
library(readxl)
library(tidyr)
library(ggpattern)
theme_set(theme_bw())

#
a <- read_xlsx("forAnalysis/Graficos Excel Barras.xlsx")


a <- pivot_longer(a, cols = 2:3, values_to = "porcentaje", names_to = "year")
a$porcentaje <- round(a$porcentaje, digits = 2)
colnames(a)[1] <- "tipo"

ggplot(data = a, aes(x = year, y = porcentaje, label = porcentaje, fill = tipo))+
  geom_bar_pattern(aes(x = year, y = porcentaje, pattern = tipo, pattern_angle = tipo, fill = tipo), 
                   stat="identity", 
                   color = "black",
                   position = position_dodge(),
                   pattern_density= 0.2,
                   pattern_spacing = 0.025)+
  ggtitle("Participacion de la Cartera de Creditos")+
  ylab("Porcentaje")+
  xlab("")+
  scale_fill_brewer(palette = 3)+
  theme(legend.key.size = unit(1.4, 'cm'), legend.position="right", legend.title = element_blank())+
  geom_text(aes(label=porcentaje), vjust=-0.5,
            position = position_dodge(0.9),
            color="black",size=3.5)

ggsave("plots/participacionCarteraCreditos.jpg", height = 7, width = 14)


###
#segundo gr?fico
b <- read_xlsx("forAnalysis/Graficos Excel Barras.xlsx",  sheet = 2)

b <- pivot_longer(b, cols = 2:3, values_to = "porcentaje", names_to = "year")
b$porcentaje <- round(b$porcentaje, digits = 4)
b$porcentaje <- b$porcentaje*100
colnames(b)[1] <- "tipo"


ggplot(data = b, aes(x = year, y = porcentaje, label = porcentaje, fill = tipo))+
  geom_bar_pattern(aes(x = year, y = porcentaje, pattern = tipo, pattern_angle = tipo, fill = tipo), 
                   stat="identity", 
                   color = "black",
                   position = position_dodge(),
                   pattern_density= 0.3,
                   pattern_spacing = 0.025)+
  ggtitle("Composicion de la Cartera de Creditos")+
  ylab("Porcentaje")+
  xlab("")+
  scale_fill_brewer(palette = 3)+
  theme(legend.key.size = unit(1.4, 'cm'), legend.position="right", legend.title = element_blank())+
  geom_text(aes(label=porcentaje), vjust=-0.5,
            position = position_dodge(0.9),
            color="black",size=3.5)

ggsave("plots/composicionCarteraCreditos.jpg", height = 7, width = 14)


