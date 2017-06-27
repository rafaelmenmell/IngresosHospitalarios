library(dplyr)
library(doSNOW)
library(foreach)
source("funciones/general.R")
source("funciones/funciones_aux.R")


ingresos1 <- LeeTodos(y1 = 2005,y2 = 2015,diag = TRUE,provincia = 28,urg = TRUE)
ingresos1 <- bind_rows(ingresos1)


cl <- makeCluster(3,outfile="")
registerDoSNOW(cl)

it <- nrow(ingresos1)

pb <- txtProgressBar(max = it, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

res = foreach(i = 1:it, 
              .combine = "rbind", 
              .packages = c("stringi","dplyr"),.options.snow = opts) %dopar% {
                cat(i)
                TraduceCodigoMasGeneralFaster(ingresos1[i,]$diag_ppal)
              }
stopCluster(cl)
ingresos1$diag1 <- res[,1]
