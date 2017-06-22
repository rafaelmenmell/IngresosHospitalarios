#rm(list=ls())
library(readr)
library(dplyr)
library(ggplot2)
library(meteologica)
library(seasonal)

LeeZip <- function(year,d=FALSE,p=NULL,u){
  #descomprime
  filezip <- sprintf("datos/datos_morbi%s.zip",substr(year,3,4))
  unzip(filezip)
  fileu <- unzip(filezip,list=TRUE)$Name
  #data <- read.fwf(fileu,widths = c(8,2,1,2,1,6,4,1,3,2,2,6,8,8),colClasses=rep("character",14))
  data <- read_fwf(fileu,fwf_widths(c(8,2,1,2,1,6,4,1,3,2,2,6,8,8)))
  colnames(data) <- c("numero","prov_hosp","sexo","prov_res","diag_in","fecha_alta","diag_ppal","motivo_alta","edad_anyos","edad_meses","edad_dias","estancia","elevacion","filler")
  #vamos a hacer unos cast para reducir el tamaño del data frame
  if(u==TRUE){
    data <- data[data$diag_in==2,]
  }
  data$numero <- as.integer(data$numero)
  data$prov_hosp <- as.integer(data$prov_hosp)
  data$sexo <- as.integer(data$sexo)
  data$prov_res <- as.integer(data$prov_res)
  data$fecha_alta <- ISOdate(year=as.integer(substr(data$fecha_alta,1,2))+2000,month = as.integer(substr(data$fecha_alta,3,4)),day = as.integer(substr(data$fecha_alta,5,6)))
  data$motivo_alta <- as.integer(data$motivo_alta)
  data$edad_anyos <- as.integer(data$edad_anyos)
  data$edad_meses <- as.integer(data$edad_meses)
  data$edad_dias <- as.integer(data$edad_dias)
  data$estancia <- as.integer(data$estancia)
  data$fecha_ingreso <- as.Date(data$fecha_alta-data$estancia*24*60*60)
  data$edad <- as.integer(round(data$edad_anyos+data$edad_meses/12+data$edad_dias/365))
  data$edad_anyos <- NULL
  data$edad_meses <- NULL
  data$edad_dias <- NULL
  data$filler <- NULL
  data$elevacion <- NULL
  data$numero <- NULL
  data$fecha_alta <- NULL
  if (!d){
    data$diag_ppal <- NULL
  }
  if(!is.null(p)){
    data <- data[data$prov_hosp==p,]
  }
  return(data)
}

LeeTodos <- function(y1=2005,y2=2015,diag=FALSE,provincia=NULL,urg=TRUE){
  ys <- y1:y2
  data.m <- list()
  n <- 1
  for (y in ys){
    print(y)
    data.m[[n]] <- LeeZip(y,d=diag,p=provincia,u=urg)
    n <- n+1
  }
  return(data.m)
}

GeneraSerie <- function(data=data.m,columnas=NULL){
  res <- list()
  for (i in 1:length(data)){
    if (is.null(columnas)){
      res[[i]] <- data[[i]] %>% dplyr::group_by(fecha_ingreso) %>% dplyr::summarise(ingresos=n())
    } else {
      res[[i]] <- data[[i]] %>% dplyr::group_by(fecha_ingreso,columnas) %>% dplyr::summarise(ingresos=n())
    }
  }
  res <- bind_rows(res)
}

# data <- LeeTodos(diag=TRUE,provincia = 28)
# 
# data <- bind_rows(data)
# 
# saveRDS(data,"data.rds")

#evolución de los ingresos respitarios pediatricos en madrid

# ejemplo <- data %>% dplyr::filter(prov_hosp==28 & edad<=12)
# 
# ejemplo.count <- ejemplo %>% dplyr::group_by(fecha_ingreso) %>% dplyr::summarise(count=n())
# plot(TimeLine(ejemplo.count,"fecha_ingreso","count"))
# 
# ejemplo$diag1 <- NA
# ejemplo$diag2 <- NA
# for (n in 1:nrow(ejemplo)){
#   print(n)
#   ejemplo[n,]$diag1 <- TraduceCodigoMasGeneral(ejemplo[n,]$diag_ppal)
#   ejemplo[n,]$diag2 <- TraduceCodigoGeneral(ejemplo[n,]$diag_ppal)
# }
# 
# saveRDS(ejemplo,"ejemplo.rds")
