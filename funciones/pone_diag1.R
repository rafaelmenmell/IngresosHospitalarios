#para ejecutar en linux

ejemplo <- data.full
library(doParallel)
cl <- makeCluster(8,outfile="")
clusterExport(cl,c("CargaDiccionario","CargaDiciconarioMasGeneral","TraduceCodigoEspecifico","TraduceCodigoGeneral","TraduceCodigoMasGeneral"))
ejemplo$diag<-NA
foreach(i = 1:nrow(ejemplo)) %dopar%{
  cat(sprintf("\r%s",i))
  ejemplo[i,]$diag1<-TraduceCodigoMasGeneral(ejemplo[n,]$diag_ppal)
}
