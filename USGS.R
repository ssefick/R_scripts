USGS <- function(input="discharge", days=7){
require(chron)
require(gsubfn)
require(ggplot2)
require(plyr)

a <- "http://waterdata.usgs.gov/nwis/uv?format=rdb&period="
#this is where you put the gauge numbers if you would like to 
#change them for your purposes
b <- "&site_no=021973269,02102908,02133500,02341800,02342500,02203000,02196690"
z <- paste(a, days, b, sep="")
L <- readLines(z)

#look for the data with USGS in front of it (this take advantage of the agency column)
L.USGS <- grep("^USGS", L, value = TRUE)
DF <- read.table(textConnection(L.USGS), fill = TRUE)  ##** need(?) closeAllConnections()
closeAllConnections()
colnames(DF) <- c("agency", "gauge", "date", "time", "time_zone", "gauge_height", "discharge", "precipitation")

pat <- "^# +USGS +([0-9]+) +(.*)"
L.DD <- grep(pat, L, value = TRUE)

library(gsubfn)
DD <- strapply(L.DD, pat, c, simplify = rbind)
DDdf <- data.frame(gauge = as.numeric(DD[,1]), gauge_name = DD[,2])
both <- merge(DF, DDdf, by = "gauge", all.x = TRUE)

dts <- as.character(both[,"date"])
tms <- as.character(both[,"time"])
date_time <- as.chron(paste(dts, tms), "%Y-%m-%d %H:%M")
DF <- data.frame(Date=as.POSIXct(date_time), both)

#change precip to numeric
DF[,"precipitation"] <- as.numeric(as.character(DF[,"precipitation"]))

###precip.1 <- subset(DF, precipitation!="NA")
### replaced with following line:
precip.1 <- subset(DF, !is.na(precipitation))
DF.precip <- ddply(precip.1, "gauge_name", transform, precipitation = cumsum(precipitation))


### qplot calls wrapped in print(); if/else logic fixed (I hope)
if(input=="data"){

 return(DF)

}
else{

 if(input=="precipitation"){

   print(
     qplot(Date, precipitation, data=DF.precip, geom="line", ylab="Precipitation") 
     + facet_wrap(~gauge_name, scales="free_y"))

 }
 else{
 
   print(
     qplot(Date, discharge, data=DF, geom="line", ylab="Discharge")
     + facet_wrap(~gauge_name, scales="free_y")
     + coord_trans(y="log10"))

### the ylab should probably be 'discharge'???
 }
}
}


