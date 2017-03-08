###################################################
##################quantile functions###############

#######################################################
####na.rm in the quantile function added mar 30 2011###
####to make sure that ragged time series can be used###

q75 <- function(x){ifelse(x>quantile(x, 0.75, na.rm=TRUE), x, NA)}
q90 <- function(x){ifelse(x>quantile(x, 0.90, na.rm=TRUE), x, NA)}
q95 <- function(x){ifelse(x>quantile(x, 0.95, na.rm=TRUE), x, NA)}
q25 <- function(x){ifelse(x<quantile(x, 0.25, na.rm=TRUE), x, NA)}
q10 <- function(x){ifelse(x<quantile(x, 0.10, na.rm=TRUE), x, NA)}
q5 <- function(x){ifelse(x<quantile(x, 0.05, na.rm=TRUE), x, NA)}
###################################################



#x <- read.zoo.csv("winter_2010-2011.csv")
#x <- x[,grep("level", colnames(x))]


x. <- na.approx(x)


######uncomment to functionize######
#recession_constants <- function(x){
####################################


y <- x.

y.2 <- y

coredata(y.2) <- apply(coredata(y), MARGIN=2, q75); q75.1 <- y.2

#coredata(y.2) <- apply(coredata(y), MARGIN=2, q90); q90.1 <- y.2

#coredata(y.2) <- apply(coredata(y), MARGIN=2, q95); q95.1 <- y.2

#overplot
#plot(merge(x,y.2), screens=c(1:14,1:14), type=c(rep("l", 14),rep("p",14)), col=c(rep("black", 14), rep("red",14)), pch=20, ylim=c(0,150))


#######################################################################
#rollapply(y.2, FUN=function(x){max(x, na.rm=TRUE)}, width=96)

#system("mkdir recession_constants_rawdata")

#directory <- getwd()

#folder <- "/recession_constants_rawdata/"

#folder. <- paste(directory,folder,sep="")

#for(i in 1:length(colnames(y))){
#write.zoo.csv(y[!is.na(y.2[,i]),i], paste(folder., colnames(y)[i], ".csv", sep=""))

#}
####################################################################

storm_events_above_thresh <- vector("list")

for(i in 1:length(colnames(y.2))){

name. <- colnames(y.2)[i]

z <- y.2[,i]

g <- cumsum(c(TRUE, diff(is.na(z)) != 0))
g[is.na(z)] <- 0
zoo_broken <- do.call("merge", split(z, g)[-1])

test <- apply(zoo_broken, MARGIN=2,FUN=function(a)sum(!is.na(a))>16 )

storm_events_above_thresh[[name.]] <- zoo_broken[,grep("TRUE", test)]

}

output.k <- vector("list")

output.k2data <- vector("list")

for(j in 1:length(storm_events_above_thresh)){

print("j"); print(j)

name. <- names(storm_events_above_thresh)[j]

a <- storm_events_above_thresh[[j]]

output.int <- vector("list", length(colnames(a)))
names.int <- vector("list", length(colnames(a)))

for(k in 1:length(colnames(a))){

print("k"); print(k)

max. <- which.max(a[,k])

maximum <- a[max.,k]

ifelse(max.+16>length(a[,k]), NA, Four_Hours_Forward <- a[max.+16,k]) 

ifelse(max.+16>length(a[,k]), k4hr <- NA, k4hr <- (1/4)*log((coredata(maximum)/coredata(Four_Hours_Forward)))) 

output.int[[k]] <- k4hr

names.int[[k]] <- colnames(a)[k] 

}


output.k2data[[name.]] <- data.frame(name=do.call(rbind, names.int),k=do.call(rbind, output.int), site=name.)
output.k[[name.]] <- do.call(rbind, output.int)



}

#out <- lapply(output.k, FUN=function(x){data.frame(mean=mean(x, na.rm=TRUE), sd=sd(x, na.rm=TRUE), CV=CV(x, na.rm=TRUE), length=length(!is.na(x)))})


#out.final <- do.call(rbind, out)

#out.final[,"start"] <- as.character(start(y))
#out.final[,"end"] <- as.character(end(y))

#return(out.final)

#####################################################
###########for data screening########################
#####################################################

data.frame.k <- do.call(rbind, output.k2data)
#return(data.frame.k)




####################
#end of workin code#
#                  #
#                  #
#                  #
#                  #
####################
















#####################################################
#####################################################


##################################################################
#PLOT OUT THE GRAPHS FOR VISUAL INSPECTION INDIVIDUAL HYDROGRAPHS#
##################################################################

#system("rm -rf recession_constants_graphs")

#system("mkdir recession_constants_graphs")

#for(i in 1:length(storm_events_above_thresh)){

#folder <- "/recession_constants_graphs/"

#dir. <- system("pwd", intern=TRUE)

#name. <- names(storm_events_above_thresh)[i]

#name.pdf <- paste(dir.,folder,name.,".pdf", sep="")

#pdf(name.pdf)

#for(k in 1:length(colnames(storm_events_above_thresh[[name.]]))){

#main. <- colnames(storm_events_above_thresh[[name.]])[k]

#a <- storm_events_above_thresh[[name.]][!is.na(storm_events_above_thresh[[name.]][,k]),k]

#plot(a, main=main., ylab="level in cm") 

#max. <- which.max(a)

#points(a[max.], col="red", pch=20)

#points(a[max.+16], col="red", pch=20)

#}

#dev.off()
#}  

#################################################################
#################################################################
#################################################################

#uncomment to functionize#
#}
##########################



#y <- recession_constants(x)
























































































