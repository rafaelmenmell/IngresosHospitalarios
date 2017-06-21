#datos de poblacion

poblacion <- read.csv2("2852sc.csv",skip = 8,header = FALSE,sep = ";")
colnames(poblacion) <- c("id","nombre",2016:2000)
poblacion[,ncol(poblacion)] <- NULL

#let's tidy up
poblacion <- poblacion %>% gather(key = year,value=poblacion,3:19)
poblacion$year <- as.numeric(poblacion$year)
