#Voy a probar a leer todo
# data.full <- LeeTodos(diag=TRUE)
# data.full <- bind_rows(data.full)
# saveRDS(data.full,"datafull.rds")

library(dplyr)
library(ggplot2)

data.full <- readRDS("datafull.rds")
source("funciones_aux.R")

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

data.full$V <- FALSE
#data.full %>% dplyr::filter(stri_detect_fixed(diag_ppal,"V")) %>% dplyr::mutate(V=TRUE)
data.full <- data.full %>% mutate_cond(condition = stri_detect_fixed(diag_ppal,"V")==TRUE,V=TRUE)
data.full <- data.full %>% dplyr::mutate(codigo=trunc(as.numeric(stri_replace_all_fixed(diag_ppal,"V",""))/10))

diccionario <- CargaDiccionario()
diccionario1 <- CargaDiciconarioMasGeneral()

#asigna diag1
data.full$diag1 <- NA
for (n in 1:nrow(diccionario1)){
  d <- diccionario1[n,]
  print(sprintf("Asignando ID de %s",d$diag))
  ini <- as.numeric(d$inicio)
  fin <- as.numeric(d$fin)
  dd <- d$ID
  if (!d$V){
    data.full <- data.full %>% mutate_cond(condition = (V==FALSE & codigo>=ini & codigo<=fin),diag1=dd)
  } else {
    data.full <- data.full %>% mutate_cond(condition = (V==TRUE & codigo>=ini & codigo<=fin),diag1=dd)
  }
}

#asigna diag2
data.full$diag2 <- NA
for (n in 1:nrow(diccionario)){
  d <- diccionario[n,]
  print(sprintf("Asignando ID de %s",d$diag))
  ini <- as.numeric(d$inicio)
  fin <- as.numeric(d$fin)
  dd <- d$ID
  if (!d$V){
    data.full <- data.full %>% mutate_cond(condition = (V==FALSE & codigo>=ini & codigo<=fin),diag2=dd)
  } else {
    data.full <- data.full %>% mutate_cond(condition = (V==TRUE & codigo>=ini & codigo<=fin),diag2=dd)
  }
}

saveRDS(data.full,file="datafullcondiag.RDS")

#por supuesto hay algo que llama la ataencion
#EFECTOS DE CUERPO EXTRAÑO QUE ENTRA A TRAVÉS DE ORIFICIO

funny <- data.full %>% filter (diag2 == 105)
funny$diag3 <- NA
for (n in 1:nrow(funny)){
  print(n)
  funny[n,]$diag3 <- TraduceCodigoEspecifico(funny[n,]$diag_ppal)
}

cosas <- unique(funny$diag3)

funny.funny <- funny %>% filter(diag3 %in% cosas[15:16])

funny.funny %>% dplyr::group_by(dsemana=weekdays(fecha_ingreso)) %>% dplyr::summarise(count=n())

#dias de estancia por diag1

data.full %>% dplyr::group_by(diag2) %>% dplyr::summarise(estancia=mean(estancia)) %>% arrange(-estancia)

#el codigo 305 son los abusos de sustancias
abusos <- data.full %>% filter(grepl(glob2rx("305*"),diag_ppal))
#un tl sencillo, po rmeses que queda mejor que por dia
abusos.tl <- abusos %>% dplyr::group_by(fecha=as.character(format(fecha_ingreso,"%Y-%m"))) %>% dplyr::summarise(total=n())
g2.1 <- ggplot(abusos.tl)+geom_bar(stat="identity",aes(x=fecha,y=total),alpha=0.4)+labs(x="fecha",y="Ingresos",title="Ingresos diarios por abusos de sustancias 2005-2014",subtitle="España",caption="INE. Encuesta de morbilidad hospitalaria")+theme(axis.text.x=element_blank())

#parece que están disminuyendo
abusos %>% filter(edad<18) %>% dplyr::group_by(y=year(fecha_ingreso)) %>% dplyr::summarise(total=n(),edad=mean(edad),estancia=mean(estancia))

abusos %>% filter(edad<18 & sexo==1) %>% dplyr::group_by(y=year(fecha_ingreso)) %>% dplyr::summarise(total=n(),edad=mean(edad),estancia=mean(estancia))

abusos %>% filter(edad<18 & sexo==2) %>% dplyr::group_by(y=year(fecha_ingreso)) %>% dplyr::summarise(total=n(),edad=mean(edad),estancia=mean(estancia))

#esguinces (cosa de chicos)
esg <- data.full %>% filter(V==FALSE & diag2==96)
esg.year <- esg %>% filter(year(fecha_ingreso)>2004) %>% group_by(y=year(fecha_ingreso),sexo) %>% dplyr::summarise(count=n())

g2.2 <- ggplot() + geom_bar(data=esg.year,stat="identity",aes(x=y,y=count,fill=factor(sexo)))+labs(x="Año",y="Ingresos",title="Ingresos anuales por esguinces 2005-2014",subtitle="España",caption="INE. Encuesta de morbilidad hospitalaria")+theme(axis.text.x=element_blank())

#por tipo
esg.tipo.sexo <- esg %>% dplyr::group_by(diag_ppal,sexo) %>% dplyr::summarise(count=n()) 
esg.tipo.sexo$desc <- NA
for (n in 1:nrow(esg.tipo.sexo)){
  print(n)
  esg.tipo.sexo[n,]$desc <- TraduceCodigoEspecifico(esg.tipo.sexo[n,]$diag_ppal)
}
esg.tipo.sexo %>% dplyr::filter(sexo==1) %>% top_n(10,count) %>% arrange(-count)
esg.tipo.sexo %>% dplyr::filter(sexo==2) %>% top_n(10,count) %>% arrange(-count)

#solo los señores
esg.señores <- esg %>% filter(sexo==1)
# el mas tipico por rangos de edades
esg.señores <- esg.señores %>% dplyr::mutate(rango=cut(edad,breaks=c(0,15,25,50,75,150),labels = c("menos de 15","15-25","25-50","50-75","mas de 75")))
esg.señores.tipo.edad <- esg.señores %>% dplyr::group_by(rango,diag_ppal) %>% dplyr::summarise(count=n())

esg.señores.tipo.edad <- esg.señores.tipo.edad %>% group_by(rango) %>% top_n(3,count)
esg.señores.tipo.edad$desc <- NA
for (n in 1:nrow(esg.señores.tipo.edad)){
  print(n)
  esg.señores.tipo.edad[n,]$desc <- TraduceCodigoEspecifico(esg.señores.tipo.edad[n,]$diag_ppal)
}
arrange(esg.señores.tipo.edad,rango,-count)

#Cuando se hacen los señores de 25-50 los esguinces
esg.señores %>% dplyr::filter(edad>=25 & edad<50) %>% dplyr::group_by(mes=month(fecha_ingreso)) %>% dplyr::summarise(total=n())

#complicaciones del puerperio
puer <- data.full %>% filter(diag2==79)
causas <- unique(puer$diag_ppal)
for (i in 1:length(causas)){
  print(TraduceCodigoEspecifico(causas[i]))
}
#el top10
top10 <- puer %>% dplyr::group_by(diag_ppal) %>% dplyr::summarise(total=n()) %>% dplyr::top_n(10,total) %>% arrange(-total)
top10$desc <- NA
for (n in 1:nrow(top10)){
  print(n)
  top10[n,]$desc <- TraduceCodigoEspecifico(top10[n,]$diag_ppal)
}

#hallazgos
hall <- data.full %>% filter(diag2==92)
causas <- unique(hall$diag_ppal)
for (i in 1:length(causas)){
  print(TraduceCodigoEspecifico(causas[i]))
}
#el top10
top10 <- hall %>% dplyr::group_by(diag_ppal) %>% dplyr::summarise(total=n()) %>% dplyr::top_n(10,total) %>% arrange(-total)
top10$desc <- NA
for (n in 1:nrow(top10)){
  print(n)
  top10[n,]$desc <- TraduceCodigoEspecifico(top10[n,]$diag_ppal)
}

#☻muerte subita
subit <- data.full %>% filter(diag2==89)
causas <- unique(subit$diag_ppal)
for (i in 1:length(causas)){
  print(TraduceCodigoEspecifico(causas[i]))
}
top10 <- subit %>% dplyr::group_by(diag_ppal) %>% dplyr::summarise(total=n()) %>% dplyr::top_n(10,total) %>% arrange(-total)
top10$desc <- NA
for (n in 1:nrow(top10)){
  print(n)
  top10[n,]$desc <- TraduceCodigoEspecifico(top10[n,]$diag_ppal)
}

#apendicitis
apen <- data.full %>% filter(diag2==63)
apen %>% dplyr::group_by(prov_hosp) %>% dplyr::summarise(total=n()) %>% arrange(-total)


#causas de muerte tras ingreso
muertes <- data.full %>% dplyr::filter(motivo_alta==3) %>% dplyr::group_by(diag_ppal) %>% dplyr::summarise(count=n()) %>% arrange(-count) %>% top_n(100,count)
muertes$desc <- NA
for (n in 1:nrow(muertes)){
  print(n)
  muertes[n,]$desc <-TraduceCodigoEspecifico(muertes[n,]$diag_ppal)
}
