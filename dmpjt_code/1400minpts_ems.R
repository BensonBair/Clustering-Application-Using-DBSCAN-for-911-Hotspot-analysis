################################################################# minpts = 1400
rdata_ems_main <- readRDS("rdata_ems")
rdata_ems_main <- rdata_ems_main[, 1:2]



fdb2 <- dbscan::dbscan(rdata_ems_main, eps = radius, minPts = 1400)

rdata_ems_main$cluster <- fdb2$cluster
#
#ggplot(rdata_ems_main,  aes(x = lng, y = lat, color = cluster)) +  geom_point() 

## data according to clusters
#clustnum <- max(unique(fdb2$cluster))
totnum <- dim(rdata_ems_main)[1] - dim(rdata_ems_main[ rdata_ems_main$cluster == 0 , 1:2])[1]

for (i in 1:(max(unique(fdb2$cluster)))) {
  nam <- paste("c",i, sep="")
  assign(nam, as.matrix(rdata_ems_main[rdata_ems_main$cluster == i, 1:2]))
  
  namm <- paste("numc",i, sep="")
  assign(namm, dim(rdata_ems_main[rdata_ems_main$cluster == i, ])[1])
  
}
#########

lc <- list(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11)

SSE <- matrix(0, 1, 11)



for (i in 1:(max(unique(fdb2$cluster)))) {
  tempD <- as.matrix(    proxy::dist(lc[[i]], lc[[i]])    )
  tempAvgD <- sum(tempD) / (dim(tempD)[1] ^ 2 - dim(tempD)[1])
  tempSSE <- (tempAvgD * (length(lc[[i]]) / 2 )  ) ^ 2
  SSE[1, i] <- tempSSE * ((length(lc[[i]]) / 2 ) / totnum)
}

TotSSE1400_ems <-  sum(SSE)
TotSSE1400_ems
