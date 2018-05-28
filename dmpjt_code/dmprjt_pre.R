###### set working directory #####
setwd("C:/Users/user/Documents/dmpjt")

rdata <- read.csv("911_PA.csv")

View(rdata)

class(rdata)

str(rdata)

dim(rdata)

summary(rdata)

#####
library(dplyr)
library(tidyr)
library(ggplot2)
library(hexbin)

rdata <- read.csv("911_PA.csv")
rdata <- rdata[, c(-3, -8, -9)]
rdata$timeStamp <- as.character(rdata$timeStamp)
rdata$timeStamp <- strptime(rdata$timeStamp, format = "%m/%d/%Y %H:%M")
                                                      #"%Y-%m-%d %H:%M:%S"
rdata$timeStamp <- gsub(" ", ",", rdata$timeStamp)
rdata <- separate(rdata, col = timeStamp, into = c("date", "time"), sep = ",")

rdata <- separate(rdata, col = title, into = c("category", "subcat"), sep = ": ")

rdata$subcat <- gsub("-", "", rdata$subcat)

# rm outliers
rdata <- rdata[!rdata$lat %in% boxplot.stats(rdata$lat, coef = 2)$out, ]
rdata <- rdata[!rdata$lng %in% boxplot.stats(rdata$lng, coef = 2)$out, ]

rdata_ems <- rdata[rdata$category == c("EMS"),]
rdata_trf <- rdata[rdata$category == c("Traffic"),]
rdata_fir <- rdata[rdata$category == c("Fire"),]

df <- rdata_fir[sample(nrow(rdata_fir), 1000),]

# plotting
ggplot(rdata_fir, aes(x = lng,y = lat)) + geom_point(alpha = 0.06)


a <- xyplot(lat ~ lng, data = rdata_ems)
par(new = TRUE)
xyplot(lat ~ lng, data = rdata_fir, col = "red")
par(new = TRUE)
xyplot(lat ~ lng, data = rdata_trf, col = "yellow", add = TRUE)

# scatter plot
qplot(lng, lat, data = rdata, col = category) # + geom_point(alpha = 0.01)

bin_fir <- hexbin(rdata_fir$lng, rdata_fir$lat, xbins = 60)
plot(bin_fir, main = "Call for Fire")

bin_ems <- hexbin(rdata_ems$lng, rdata_ems$lat, xbins = 60)
plot(bin_ems, main = "Call for EMS")

bin_trf <- hexbin(rdata_trf$lng, rdata_trf$lat, xbins = 60)
plot(bin_trf, main = "Call for Traffic")

# another way
g <- ggplot(rdata, aes(x = lng,y = lat))
g + geom_point( size = 0.1, alpha = 0.06) + facet_grid(. ~ category)

ggplot(rdata_fir, aes(x = lng,y = lat), main = "Call for Fire") + geom_point(alpha = 0.01)
ggplot(rdata_trf, aes(x = lng,y = lat), main = "Call for Traffic") + geom_point(alpha = 0.05)
ggplot(rdata_ems, aes(x = lng,y = lat), main = "Call for EMS") + geom_point(alpha = 0.008)

########### temp ends #
