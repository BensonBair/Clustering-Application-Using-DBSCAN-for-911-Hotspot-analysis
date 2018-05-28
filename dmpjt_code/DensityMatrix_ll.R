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








