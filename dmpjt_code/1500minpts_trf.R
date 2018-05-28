################################################################# minpts = 400
rdata_trf_main <- readRDS("rdata_trf")
rdata_trf_main <- rdata_trf_main[, 1:2]



fdb2 <- dbscan::dbscan(rdata_trf_main, eps = radius, minPts = 1500)

rdata_trf_main$cluster <- fdb2$cluster
#
#ggplot(rdata_trf_main,  aes(x = lng, y = lat, color = cluster)) +  geom_point() 

## data according to clusters
#clustnum <- max(unique(fdb2$cluster))
totnum <- dim(rdata_trf_main)[1] - dim(rdata_trf_main[ rdata_trf_main$cluster == 0 , 1:2])[1]

for (i in 1:(max(unique(fdb2$cluster)))) {
  nam <- paste("c",i, sep="")
  assign(nam, as.matrix(rdata_trf_main[rdata_trf_main$cluster == i, 1:2]))
  
  namm <- paste("numc",i, sep="")
  assign(namm, dim(rdata_trf_main[rdata_trf_main$cluster == i, ])[1])
  
}
#########

lc <- list(c1, c2, c3, c4, c5, c6)

SSE <- matrix(0, 1, 6)



for (i in 1:(max(unique(fdb2$cluster)))) {
  tempD <- as.matrix(    proxy::dist(lc[[i]], lc[[i]])    )
  tempAvgD <- sum(tempD) / (dim(tempD)[1] ^ 2 - dim(tempD)[1])
  tempSSE <- (tempAvgD * (length(lc[[i]]) / 2 )  ) ^ 2
  SSE[1, i] <- tempSSE * ((length(lc[[i]]) / 2 ) / totnum)
}

TotSSE1500_trf <-  sum(SSE)
TotSSE1500_trf
