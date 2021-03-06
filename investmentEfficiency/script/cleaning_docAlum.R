rm(list = ls())

library(dplyr)
library(foreign)

docentes <- read.dbf("E:/Asesor�as de tesis/Econom�a/Eficiencia del gasto p�blico en la educaci�n/Docentes_01.dbf")

##
colnames(docentes)
docentes <- docentes[docentes$NIV_MOD == "F0", ]
docentes <- docentes[docentes$CUADRO == "C301", ]
docentes$hsum <- rowSums(docentes[, 7:30])
docentesCole <- aggregate(docentes$hsum, by = list(docentes$COD_MOD), sum)
colnames(docentesCole)[1] <- "COD_MOD"
colnames(docentesCole)[2] <- "DOCTOTAL"
docentesCole <- docentesCole[!duplicated(docentesCole$COD_MOD), ]

##
alumnos <- read.dbf("E:/Asesor�as de tesis/Econom�a/Eficiencia del gasto p�blico en la educaci�n/Matricula_01.dbf")
alumnos <- alumnos[alumnos$NIV_MOD == "F0", ]
alumnos <- alumnos[alumnos$CUADRO == "C201", ]
colnames(alumnos)
alumnos$hsum <- rowSums(alumnos[, 8:27])
alumnosCole <- aggregate(alumnos$hsum, by = list(alumnos$COD_MOD), sum)
colnames(alumnosCole)[1] <- "COD_MOD"
colnames(alumnosCole)[2] <- "MATTOTAL"
alumnosCole <- alumnosCole[!duplicated(alumnosCole$COD_MOD), ]

docAlum <- merge(docentesCole, alumnosCole, by = "COD_MOD")
docAlum$docAlum <- docAlum$MATTOTAL/docAlum$DOCTOTAL

docAlum <- left_join(docAlum, docentes[, c("COD_MOD", "CODGEO")], by = "COD_MOD")
docAlum <- docAlum[!duplicated(docAlum$COD_MOD), ]
docAlum$depart <- substr(docAlum$CODGEO, 1, 2)
docAlum$prov <- substr(docAlum$CODGEO, 3, 4)
docAlum <- docAlum[!docAlum$docAlum == Inf, ]

#merge departamento
ub <- readxl::read_xlsx("E:/Asesor�as de tesis/Econom�a/Eficiencia del gasto p�blico en la educaci�n/rptUbigeo.xlsx", sheet = 2)
docAlum <- merge(docAlum, ub, by = "depart", all.x = T)
docAlum$Departamento[docAlum$depart == "15" &
          docAlum$prov == "01"] <- "Provincia de Lima"
docAlum$Departamento[docAlum$depart == "15" &
                       !docAlum$prov == "01"] <- "Regi�n Lima"

docAlum2 <- aggregate(docAlum$docAlum, by= list(docAlum$Departamento), mean)
colnames(docAlum2) <- c("Departamento", "alumDoc")
docAlum2$year <- "2018"

saveRDS(docAlum2, "E:/Asesor�as de tesis/Econom�a/Eficiencia del gasto p�blico en la educaci�n/alumDoc18.Rdata")
