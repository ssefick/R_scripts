#input depth velocity data - make sure that it is in
#the correct format

cms_calc <- function(data){
require(zoo)
require(chron)
require(doBy)
Q <- function(a){
#discharge function
a[is.na(a[,"Velocity_1_if_d.0.6"]),"Velocity_1_if_d.0.6"] <- 0
a[is.na(a[,"Velocity_2_if_d.0.6"]),"Velocity_2_if_d.0.6"] <- 0

vel.1 <- a[,"Velocity_1_if_d.0.6"]
vel.2 <- a[,"Velocity_2_if_d.0.6"]

vel_2 <- sum(vel.2, na.rm=TRUE)
velocity <- (vel.1+vel.2)/2

#get the average depth for each "cell" 
depths <- a[,"depth_m"]
length_depths <- length(depths)-1
mean_depth_vect <- vector(mode="numeric", length=length_depths)
for(k in 1:length_depths){
mean_depth_vect[k] <- ((depths[k]+depths[k+1])/2) 
}

velocity <- velocity[1:length(mean_depth_vect)]
cms <- mean_depth_vect*velocity
#a$depth_vect <- mean_depth_vect
cms_sum <- sum(cms)
return(cms_sum)
}

require(doBy)
data <- data[,-c(19,20)]
z <- splitBy(~serial_num+Creek+Date, data=data)
b <- lapply(z, Q)
tmp <- data.frame(unlist(b))
tmp.2  <- data.frame(do.call(rbind,strsplit(row.names(tmp), split="\\|")))

colnames(tmp.2) <- c("serial_num", "Creek", "Date")
cms <- data.frame(tmp.2, cms=tmp[,1])
tmp.ser <- as.numeric(as.character(cms[,"serial_num"]))
cms[,1] <- tmp.ser - 100000000
return(cms)
}



