# library
library(TSstudio)
library(zoo)
library(maptools)
library(sp)
library(geosptdb)

# import data
peta <- readShapeSpatial("C:/Peta/PULAUSUMATERA.shp")
mydata <- read.csv("C:/newdata.csv", header = T)
koordinat <- read.csv("C:/koordinat.csv", header = T)
mydata$time <- as.Date(mydata$time,'%d/%m/%Y')
mydata$year <- as.integer(format(mydata$time,'%Y'))
mydata$month <- as.integer(format(mydata$time,'%m'))

# Compute total renewable energy sources
totalpermonth_year <- aggregate(cbind(bengkulu, sumsel, sumut, sumbar, riau,
                                 aceh, lampung, kepri, babel, jambi) ~ month*year, data = mydata, FUN = sum)
# Time Series Plot
for( i in c("bengkulu", "sumsel", "sumut", "sumbar", "riau",
            "aceh", "lampung", "kepri", "babel", "jambi")) {
  plot(ts(totalpermonth_year[,i], start = c(1980, 2), frequency = 12), 
       col = "red", ylab = "Renewable Energy", main = paste("Monthly Total Renewable Energy Sources in",i,"1980-2019")) 
}

totalpermonth <- aggregate(cbind(bengkulu, sumsel, sumut, sumbar, riau,
                                 aceh, lampung, kepri, babel, jambi) ~ month, data = mydata, FUN = sum)

median(totalpermonth$jambi)
min(totalpermonth$jambi)
max(totalpermonth$jambi)
summary(mydata)
# BarPlot
for( i in c("bengkulu", "sumsel", "sumut", "sumbar", "riau",
            "aceh", "lampung", "kepri", "babel", "jambi")) {
  barplot(totalpermonth[,i],
          main = paste("Monthly Renewable Energy Sources in",i),
          xlab = "Month",
          ylab = "Renewable Energy",
          ylim = c(0,6000),
          names.arg = c("Jan","Feb","March","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
          col = "darkred")
}

totalperyear <- aggregate(cbind(bengkulu, sumsel, sumut, sumbar, riau,
                                 aceh, lampung, kepri, babel, jambi) ~ year, data = mydata, FUN = sum)
summary(totalperyear)
dens <- density(totalperyear$bengkulu)
hist(totalperyear$bengkulu, freq=FALSE, col="steelblue")
polygon(dens, border="red")
# Regression Plot
for( i in c("bengkulu", "sumsel", "sumut", "sumbar", "riau",
            "aceh", "lampung", "kepri", "babel", "jambi")) {
  plot(ts(totalperyear[,i], start = c(1980)), type = "o",
       col = "red", ylab = "Renewable Energy", main = paste("Yearly Total Renewable Energy Sources in",i))
  fit=lm(totalperyear[,i]~totalperyear[,"year"])
  abline(fit)
  text(x = 2010,y = 1150,labels = substitute(paste("y = ",beta0,beta1," Year",sep=' '),
                                         list(beta0=round(fit$coefficients[1],2),beta1=round(fit$coefficients[2],2))))
}

ts_plot(zoo(cbind(bengkulu = totalperyear$bengkulu, sumsel = totalperyear$sumsel,
                  sumut = totalperyear$sumut, sumbar = totalperyear$sumbar,
                  riau = totalperyear$riau, aceh = totalperyear$aceh,
                  lampung = totalperyear$lampung, kepri = totalperyear$kepri,
                  babel = totalperyear$babel, jambi = totalperyear$jambi), order.by = totalperyear$year), title = "Total Renewable Energy Sources")

value <- colMeans(totalperyear)[-1]
std <- apply(totalperyear, 2, sd)[-1]
bar_plot <- data.frame(cbind(value,std))
ggplot(bar_plot) +
  geom_bar( aes(x=names(bar), y=value), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=names(bar), ymin=value-std, ymax=value+std), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  ggtitle("Average Renewable Energy Sources in Every Area") + 
  labs(y="Renewable Energy", x = "Lokasi") + ylim(0, 1800)

# define groups for mapping
firstdata <- as.data.frame(t(totalperyear[,-1]))
names(firstdata) <- c(paste("Year",1980:2019, sep = ""))

dsp <- SpatialPoints(koordinat[,2:3], proj4string=CRS("+proj=longlat +datum=WGS84"))
dsp <- SpatialPointsDataFrame(dsp, firstdata)

pols <- list("sp.polygons", peta)
spplot(dsp, c("Year2000","Year2005","Year2010","Year2019","Year1980","Year1985","Year1990","Year1995"), pch=20, cex=1.5,
       sp.layout = pols, key.space="right", colorkey = T) 

# Interpolation Case: a grid of points Sumatra (All years)
finaldata <- NULL
for(i in 2:ncol(totalperyear)) {
  temp <- cbind(koordinat[(i-1),],totalperyear[,c(1,i)])
  colnames(temp) = c("Location","x","y","t","Series") # x = Longitude, y = Latitude, t = Years
  finaldata <- rbind(finaldata,temp)
}
points <- spsample(peta, n=10000, type="regular")
coordinates(finaldata) <- ~ x + y
GridsT <- vector(mode = "list", length = 40)

for(i in 1:40){ 
  GridsT[[i]] <- data.frame(points@coords,1979+i)
  names(GridsT[[i]]) <- c("x","y","t")
}

idw.sumatra <- data.frame(matrix(NA, ncol = 42, nrow=nrow(GridsT[[1]])))
pb <- txtProgressBar(min = 0, max = 40, char = "=", style = 3)
for(i in 1:40){ 
  coordinates(GridsT[[i]]) <- c("x","y")
  idw.sumatra[,i+2] <- idwST(Series ~ 1, data = finaldata, newdata=GridsT[[i]], n.neigh=10, C=1, 
                             factor.p=2, progress=FALSE)[,4]                
  setTxtProgressBar(pb, i)
}
close(pb)

idw.sumatra[,1:2] <- GridsT[[1]]@coords
nam <- paste("Year",1980:2019,sep="")
names(idw.sumatra) <- c("Longitude","Latitude",nam)
coordinates(idw.sumatra) <- c("Longitude","Latitude")
gridded(idw.sumatra) <- TRUE

# show prediction map
spplot(idw.sumatra,c("Year2005","Year2010","Year2015","Year2017","Year2019","Year1980","Year1985","Year1990","Year1995","Year2000"),colorkey = T,key.space="right",
       pch=20,cex=1.5,cex.lab=0.3, cex.title=0.3,auto.key = F, 
       main = "KAPASITAS ENERGI TERBARUKAN DI PULAU SUMATERA IDW MAP 1980-2019")

# RMSE value
RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}
actual <- rep(NA, 40)
pred <- rep(NA, 40)
for (k in 1:40) {
  actual[k] <- mean(finaldata[finaldata$t==(1979+k),]$Series) 
  pred[k] <- mean(as.data.frame(idw.sumatra)[,k])
}
RMSE(actual,pred)


