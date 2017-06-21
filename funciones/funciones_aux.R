library(RCurl)
library(dplyr)
library(stringi)
CargaDiccionario <- function(){
  diccionario <- read.csv("diccionario_general.txt",header=FALSE,sep=";")
  colnames(diccionario) <- c("diag","inicio","fin")
  diccionario$V <- FALSE
  diccionario[grepl("V",diccionario$inicio),]$V <- TRUE
  diccionario$inicio <- gsub("V"," ",diccionario$inicio)
  diccionario$fin <- gsub("V"," ",diccionario$fin)
  diccionario$inicio <- as.numeric(diccionario$inicio)
  diccionario$fin <- as.numeric(diccionario$fin)
  diccionario$ID <- 1:nrow(diccionario)
  return(diccionario)
}

CargaDiciconarioMasGeneral <- function(){
  diccionario <- read.csv("diccionario_mas_general.txt",header=FALSE,sep=";")
  colnames(diccionario) <- c("diag","inicio","fin")
  diccionario$V <- FALSE
  diccionario$inicio <- as.numeric(diccionario$inicio)
  diccionario$fin <- as.numeric(diccionario$fin)
  diccionario$ID <- 1:nrow(diccionario)
  return(diccionario)
}

TraduceCodigoEspecifico <- function(codigo){
  if (nchar(codigo)==4){
    if (grepl("V",codigo)==FALSE){
      codigo <- paste(substr(codigo, 1, 3), ".", substr(codigo, 4, 4), sep = "")
    }
  }
  url <- sprintf("http://icd9cm.chrisendres.com/index.php?srchtype=diseases&srchtext=%s&Submit=Search&action=search",codigo)
  info <- readLines(url)
  info <- info[grepl(codigo,info)][2]
  info <- strsplit(info,codigo)[[1]][2]
  info <- gsub("</div>","",info)
  return(info)
}

diccionario <- CargaDiccionario()

TraduceCodigoGeneral <- function(codigo){
  #diccionario <- CargaDiccionario()
  if (grepl("V",codigo)){
    codigo <- substr(codigo,1,3)
    codigo <- gsub("V","",codigo)
    d2 <- diccionario[diccionario$V==TRUE,]
  } else {
    d2 <- diccionario[diccionario$V==FALSE,]
    if (nchar(codigo)==4){
      codigo <- substr(codigo,1,3)
    }
  }
  codigo <- as.numeric(codigo)
  diag <- d2 %>% filter(inicio<=codigo & fin>=codigo)
  if (nrow(diag)==0){
    dd <- NA
  } else {
    dd <- diag$ID
  }
  return(dd)
}

diccionario1 <- CargaDiciconarioMasGeneral()

TraduceCodigoMasGeneral <- function(codigo){
  #diccionario <- CargaDiccionario()
  if (grepl("V",codigo)==TRUE){
    dd <- TraduceCodigoGeneral(codigo)
  } else {
    if (nchar(codigo)==4){
      codigo <- substr(codigo,1,3)
    }
    codigo <- as.numeric(codigo)
    diag <- diccionario1 %>% filter(inicio<=codigo & fin>=codigo)
    dd <- diag$ID
  }
  
  return(dd)
}

TraduceCodigoMasGeneralFaster <- function(codigo){
  #diccionario <- CargaDiccionario()
  if (stri_sub(codigo,1,1)=="V"){
    dd <- TraduceCodigoGeneral(codigo)
  } else {
    if (nchar(codigo)==4){
      codigo <- substr(codigo,1,3)
    }
    codigo <- as.numeric(codigo)
    diag <- diccionario1 %>% filter(inicio<=codigo & fin>=codigo)
    dd <- diag$ID
  }
  
  return(dd)
}

