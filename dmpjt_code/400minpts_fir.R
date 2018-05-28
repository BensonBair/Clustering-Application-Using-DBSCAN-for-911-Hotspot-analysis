setwd("C:/Users/user/Documents/dmpjt")

library(dplyr)
library(tidyr)
library(ggplot2)
library(hexbin)
library(factoextra)
library(dbscan)
library(proxy)

# determine eps (given a k-nearlest)
rdata_fir <- readRDS("rdata_fir")
rdata_fir_main <- rdata_fir[, 1:2]
dbscan::kNNdistplot(rdata_fir_main, k =  100)
abline(h = 0.02, lty = 2)

################################################################ minpts = ?
rdata_fir <- readRDS("rdata_fir")
rdata_fir_main <- rdata_fir[, 1:2]
radius <- sqrt(1261 / 69/ pi) / 111

fdb2 <- dbscan::dbscan(rdata_fir_main, eps = radius, minPts = 350)

fdb2

rdata_fir_main$cluster <- fdb2$cluster
rdata_fir_main[10:20,]


## data according to clusters
#clustnum <- max(unique(fdb2$cluster))
totnum <- dim(rdata_fir_main)[1] - dim(rdata_fir_main[ rdata_fir_main$cluster == 0 , 1:2])[1]

for (i in 1:(max(unique(fdb2$cluster)))) {
  nam <- paste("c",i, sep="")
  assign(nam, as.matrix(rdata_fir_main[rdata_fir_main$cluster == i, 1:2]))
  
  namm <- paste("numc",i, sep="")
  assign(namm, dim(rdata_fir_main[rdata_fir_main$cluster == i, ])[1])
  
}
######### create a list containing ? elements (of the number of clusters)
########## create SSE matrix with the value 0 (of nrow epualing the number of clusters)
lc <- list(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11)

SSE <- matrix(0, 1, 11)

######### create a fuction computing distance
mdist <- function(a,b){
  la <- nrow(a)
  lb <- nrow(b)
  ccm <- matrix(nrow=la,ncol=lb)
  for(i in 1:la)
    for(j in 1:lb)
      ccm[i,j] <- sqrt(sum((a[i,] - b[j,])^2))
  return(ccm)
}

######### compute the AVERAGE SSE with different minpts after weighting 

for (i in 1:(max(unique(fdb2$cluster)))) {
  tempD <- as.matrix(    proxy::dist(lc[[i]], lc[[i]])    )
  tempAvgD <- sum(tempD) / (dim(tempD)[1] ^ 2 - dim(tempD)[1])
  #tempSSE <- (tempAvgD * (length(lc[[i]]) / 2 )  ) ^ 2
  SSE[1, i] <- tempAvgD * ((length(lc[[i]]) / 2 ) / totnum)
}

TotSSE250_fir <-  sum(SSE)
TotSSE250_fir

######### scatter plot after clustering 
gg1 <- ggplot(rdata_fir_main,  aes(x = lng, y = lat, color = cluster)) 
gg1 +  geom_point(aes(colour = factor(cluster))) 


######### another discussion ------- matrix 
RegionMatrix <- matrix(0, nrow = 13, ncol = 17)

lngr <- ( max(rdata_fir_main$lng) -  min(rdata_fir_main$lng) ) / 16
latr <- ( max(rdata_fir_main$lat) -  min(rdata_fir_main$lat) ) / 12

xn <- seq(from = min(rdata_fir_main$lng), to = max(rdata_fir_main$lng), by = lngr)
yn <- seq(from = min(rdata_fir_main$lat), to = max(rdata_fir_main$lat), by = latr)

length(xn)
length(yn)

rdata_fir_main <- cbind(rdata_fir_main, findInterval(rdata_fir_main$lat, yn))
rdata_fir_main <- cbind(rdata_fir_main, findInterval(rdata_fir_main$lng, xn))

# FINAL counting
for (i in 1:13) {
  for (j in 1:17) {
    RegionMatrix[13 - i, j] =    sum(rdata_fir_main$`findInterval(rdata_fir_main$lng, xn)` == j & rdata_fir_main$`findInterval(rdata_fir_main$lat, yn)`==  i    )  
  }
}

View(RegionMatrix)

