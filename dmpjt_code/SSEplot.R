# SUMMARY SSE PLOT
fireMin <- c(300, 350, 450, 500) 
trafficMin <- c(900, 1200, 1500, 1800)
emsMin <- c(1400, 1500, 2000, 2800)

g <- ggplot(data=a, aes(x=trafficMin, y=SSE)) 
g + 
  geom_line() +
  geom_point() +
  ggtitle("DBSCAN SSE of Traffic in Different MinPts")


fireSSE <- readRDS("fireSSE")
emsSSE <- readRDS("emsSSE")
trafficSSE <- readRDS("trafficSSE")



#a <- as.data.frame(cbind(cbind(fireMin), cbind(SSE)))
t <- data.frame(c(TotSSE900_trf, TotSSE1200_trf, TotSSE1500_trf, TotSSE1800_trf) )
colnames(t) <- c("SSE")
a <- as.data.frame(cbind(cbind(trafficMin), cbind(t)))
####

gg <- ggplot(data=b, aes(x=fireMin, y=SSE)) 
gg + 
  geom_line() +
  geom_point() +
  ggtitle("DBSCAN SSE of Fire in Different MinPts")


tt <- data.frame(c(TotSSE300_fir, TotSSE350_fir, TotSSE450_fir, TotSSE500_fir) )
colnames(tt) <- c("SSE")
b <- as.data.frame(cbind(cbind(fireMin), cbind(tt)))

#####


ggg <- ggplot(data=c, aes(x=emsMin, y=SSE)) 
ggg + 
  geom_line() +
  geom_point() +
  ggtitle("DBSCAN SSE of EMS in Different MinPts")


SSE <- c()
SSE[1] <- trafficSSE[[1]]
SSE[2] <- trafficSSE[[2]]
SSE[3] <- trafficSSE[[3]]

ttt <- data.frame(c(TotSSE1400_ems, TotSSE1500_ems, TotSSE2000_ems, TotSSE2800_ems) )
colnames(ttt) <- c("SSE")
c <- as.data.frame(cbind(cbind(emsMin), cbind(ttt)))

