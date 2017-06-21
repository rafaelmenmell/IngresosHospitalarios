#un poco de exploracion de los ingresos en madrid de <=12 años
library(ggplot2)
library(gridExtra)
library(zoo)
library(tidyr)

ejemplo <- readRDS("ejemplo.rds")

#el años 2004 no es bueno
ejemplo <- ejemplo[year(ejemplo$fecha_ingreso)!=2004,]

ejemplo.count.year.diag1 <- ejemplo %>% dplyr::group_by(y=year(fecha_ingreso),diag1) %>% dplyr::summarise(total=n()) %>% dplyr::top_n(10,total)

g <- vector(mode="list",length = 8)

g[[1]] <- ggplot(data=ejemplo.count.year.diag1)+geom_bar(stat="identity",aes(x=reorder(diag1,total),total,fill=diag1))+facet_wrap(~ y,nrow = 2,ncol=5,scales = "fixed")+theme(axis.text.x=element_blank(),axis.title.x=element_blank(),axis.ticks.x=element_blank())+scale_fill_discrete(guide=guide_legend(ncol=1))+ theme(legend.text=element_text(size=5),legend.title=element_blank())+labs(y="Ingresos",title="Diez razones principales de ingreso por año. 2005-2014",subtitle="Madrid. Pacientes menores de 13 años",caption="INE. Encuesta de morbilidad hospitalaria")

# se ve que Enfermedades del aparato respiratorio es la number1

respi <- ejemplo[ejemplo$diag1=="ENFERMEDADES DEL APARATO RESPIRATORIO",]
g[[2]] <- ggplot(data=respi)+geom_bar(aes(x=factor(year(fecha_ingreso))))+labs(x="Año",y="Ingresos",title="Ingresos anuales por enfermedades del aparato respiratorio 2005-2014",subtitle="Madrid. Pacientes menores de 13 años",caption="INE. Encuesta de morbilidad hospitalaria")

respi.tl <- respi %>% dplyr::group_by(fecha=fecha_ingreso) %>% dplyr::summarise(total=n())

g[[3]] <- ggplot(respi.tl)+geom_line(aes(x=fecha,y=total),alpha=0.4)+labs(x="fecha",y="Ingresos",title="Ingresos diarios por enfermedades del aparato respiratorio 2005-2014",subtitle="Madrid. Pacientes menores de 13 años",caption="INE. Encuesta de morbilidad hospitalaria")

#vamos a hacer una "climatologia"
respi.clim <- respi.tl %>% dplyr::group_by(dia=yday(fecha)) %>% dplyr::summarise(media=mean(total))
respi.clim$fecha <- seq(as.Date("2016-01-01"),as.Date("2016-12-31"),1)
#podemos hacerla mas suave
respi.clim$media.suave <- rollapply(rep(x = respi.clim$media,3),7,mean)[367:732]

g[[4]] <- ggplot(respi.clim)+geom_line(aes(x=as.Date(fecha,format="%m"),y=media.suave),alpha=0.4)+labs(x="",y="Ingresos",title="Ingresos medios diarios (media móvil 7 días) por enfermedades del aparato respiratorio 2005-2014",subtitle="Madrid. Pacientes menores de 13 años",caption="INE. Encuesta de morbilidad hospitalaria")+scale_x_date(date_labels = "%B",date_breaks ="1 month")

#los top de las enfermedades repiratorias
respi.count.year.diag2 <- respi %>% dplyr::group_by(y=year(fecha_ingreso),diag2) %>% dplyr::summarise(total=n()) %>% dplyr::top_n(10,total)

g[[5]] <- ggplot(data=respi.count.year.diag2)+geom_bar(stat="identity",aes(x=reorder(diag2,total),total,fill=diag2))+facet_wrap(~ y,nrow = 2,ncol=5,scales = "fixed")+theme(axis.text.x=element_blank(),axis.title.x=element_blank(),axis.ticks.x=element_blank())+scale_fill_discrete(guide=guide_legend(ncol=1))+ theme(legend.text=element_text(size=5),legend.title=element_blank())+labs(y="Ingresos",title="Diez razones principales de ingreso por enfermedad del aparato respiratorio por año. 2005-2014",subtitle="Madrid. Pacientes menores de 13 años",caption="INE. Encuesta de morbilidad hospitalaria")

#los top son las infecciones respiratorias y la gripe
respi2 <- respi[respi$diag2 %in% c("INFECCIONES RESPIRATORIAS AGUDAS","NEUMONÍA Y GRIPE"),]
g[[6]] <- ggplot(data=respi2)+geom_bar(aes(fill=diag2,x=factor(year(fecha_ingreso))))+labs(x="Año",y="Ingresos",title="Ingresos anuales por neumonía, gripe y otras infecciones del aparato respiratorio 2005-2014",subtitle="Madrid. Pacientes menores de 13 años",caption="INE. Encuesta de morbilidad hospitalaria")+scale_fill_discrete(guide=guide_legend(title=element_blank()))+theme(legend.text=element_text(size=5))

respi2.tl <- respi2 %>% dplyr::group_by(fecha=fecha_ingreso,diag=diag2) %>% dplyr::summarise(total=n())

g[[7]] <- ggplot(respi2.tl)+geom_line(aes(x=fecha,y=total,colour=diag))+labs(x="fecha",y="Ingresos",title="Ingresos diarios por neumonía, gripe y otras infecciones del aparato respiratorio 2005-2014",subtitle="Madrid. Pacientes menores de 13 años",caption="INE. Encuesta de morbilidad hospitalaria")+scale_colour_discrete(guide=guide_legend(title=element_blank()))+theme(legend.text=element_text(size=5))

#vamos a hacer una "climatologia"
respi2.clim <- respi2.tl %>% dplyr::group_by(dia=yday(fecha),diag) %>% dplyr::summarise(media=mean(total))
respi2.clim <- spread(respi2.clim,key = diag,value=media)
respi2.clim$fecha <- seq(as.Date("2016-01-01"),as.Date("2016-12-31"),1)
respi2.clim <- as.data.frame(respi2.clim)
#podemos hacerla mas suave
respi2.clim[,2] <- rollapply(rep(x = respi2.clim[,2],3),15,mean)[367:732]
respi2.clim[,3] <- rollapply(rep(x = respi2.clim[,3],3),15,mean)[367:732]

respi2.clim <- gather(respi2.clim,key=tipo,value = media,2:3)

g[[8]] <- ggplot(respi2.clim)+geom_line(aes(x=as.Date(fecha,format="%m"),y=media,colour=tipo))+labs(x="",y="Ingresos",title="Ingresos medios diarios (media móvil 7 días) por neumonía, gripe y otras infecciones del aparato respiratorio 2005-2014",subtitle="Madrid. Pacientes menores de 13 años",caption="INE. Encuesta de morbilidad hospitalaria")+scale_x_date(date_labels = "%B",date_breaks ="1 month")+scale_colour_discrete(guide=guide_legend(title=element_blank()))+theme(legend.text=element_text(size=5))

for (i in 1:length(g)){
  png(filename = sprintf("grafico%s.png",i),width = 1110,height = 550,res = 100)
  plot(g[[i]])
  dev.off()
}
