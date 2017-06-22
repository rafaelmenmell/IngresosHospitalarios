#para ejecutar en linux

ejemplo <- ingresos.urgentes.madrid.2010[1:100,]
library(doParallel)
cl <- makeCluster(8,outfile="")
clusterExport(cl,c("CargaDiccionario","CargaDiciconarioMasGeneral","TraduceCodigoEspecifico","TraduceCodigoGeneral","TraduceCodigoMasGeneral"))
ejemplo$diag1<-NA
foreach(i = 1:nrow(ejemplo)) %dopar%{
  cat(sprintf("\r%s",i))
  ejemplo[i,]$diag1<-TraduceCodigoMasGeneral(ejemplo[i,]$diag_ppal)
}
