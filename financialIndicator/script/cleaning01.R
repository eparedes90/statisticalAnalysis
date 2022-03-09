
####
#EXRACTING DATA

rm(list = ls())


#ratios de liquidez
#
month <- c("Enero",
           "Febrero",
           "Marzo",
           "Abril",
           "Mayo",
           "Junio",
           "Julio",
           "Agosto",
           "Setiembre",
           "Octubre",
           "Noviembre",
           "Diciembre")

year <- c("2016",
          "2017",
          "2018",
          "2019",
          "2020",
          "2021")

month2 <- c("en",
            "fe",
            "ma",
            "ab",
            "my",
            "jn",
            "jl",
            "ag",
            "se",
            "oc",
            "no",
            "di")

# carpeta <- c("C-1242-",
#              "C-1230",
#              "C-1216",
#              "C-1301",
#              "C-1101",
#              "C-120201-",
#              "C-1233-",
#              "C-1205")

carpeta <- c("C-1205-")


#harvest the data
for (n in year){
  for (i in 1:12){
    for (c in carpeta){
      download.file(url = paste("https://intranet2.sbs.gob.pe/estadistica/financiera/", 
                                n,
                                "/",
                                month[i],
                                "/",
                                c,
                                month2[i],
                                n,
                                ".XLS",
                                sep = ""),
                    destfile = paste("raw/",
                                     c,
                                     "/",
                                     n,
                                     "_",
                                     i,
                                     ".xls",
                                     sep = ""),
                    mode = "wb",
                    quiet = TRUE)
    } 
  }
} 

