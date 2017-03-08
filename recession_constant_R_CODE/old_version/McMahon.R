################read data and get level only###
x <- read.zoo.csv("level_reduced.csv")
x <- x[,grep("level", colnames(x))]

#############change to meters##################

#coredata(x) <- apply(coredata(x), MARGIN=2, fun=cm2m) 

###############################################

###############colnames x######################
# [1] "ftbn_hbms_level" "ftbn_kmms_level" "ftbn_k13__level" "ftbr_jpms_level"
# [5] "ftbr_bmcm_level" "ftbr_wpms_level" "ftbr_jcms_level" "ftbr_fbms_level"
# [9] "ftbr_fcms_level" "srs__mc_6_level" "srs__pbms_level" "srs__mb_6_level"
# [13] "srs__mqhw_level" "srs__mq_8_level"
###############################################


##################hourly data##################
b <- hourly_zoo(x)





McMahon_et_al._2003 <- function(zoo_hourly_series){
#GIVE THIS FUNCTION Hourly Stage data in cm#
y <- zoo_hourly_series
#plot(y)
###############################################
################x2-x1##########################
s <- diff(y)

###############################################


##################data frame output from apply#
dapply <- function(...){

x <- apply(...)

as.data.frame(t(x))

}

################################################


#############rise flashiness####################
periodr.3.048 <- function(x){sum(x>=3.048)}
periodr.9.144 <- function(x){sum(x>=9.144)}
periodr.15.24 <- function(x){sum(x>=15.24)}
periodr.21.336 <- function(x){sum(x>=21.336)}
periodr.27.432 <- function(x){sum(x>=27.432)}

x.1 <- dapply(s, MARGIN=2, periodr.3.048)

rownames(x.1) <- "3.048"

x.2 <- dapply(s, MARGIN=2, periodr.9.144)

rownames(x.2) <- "9.144"

x.3 <- dapply(s, MARGIN=2, periodr.15.24)

rownames(x.3) <- "15.24"

x.4 <- dapply(s, MARGIN=2, periodr.21.336)

rownames(x.4) <- "21.336"

x.5 <- dapply(s, MARGIN=2, periodr.27.432)

rownames(x.5) <- "27.432"

rise <- rbind(x.1,x.2,x.3,x.4,x.5)

name_rise <- paste(rownames(rise), "cm", sep="_")

rise <- data.frame(rise, limb="rising", rise_fall=name_rise)

#################################################

###############fall flashiness###################
periodf.3.048 <- function(x){sum(x<=-3.048)}
periodf.9.144 <- function(x){sum(x<=-9.144)}
periodf.15.24 <- function(x){sum(x<=-15.24)}
periodf.21.336 <- function(x){sum(x<=-21.336)}
periodf.27.432 <- function(x){sum(x<=-27.432)}

x.1 <- dapply(s, MARGIN=2, periodf.3.048)

rownames(x.1) <- "3.048"

x.2 <- dapply(s, MARGIN=2, periodf.9.144)

rownames(x.2) <- "9.144"

x.3 <- dapply(s, MARGIN=2, periodf.15.24)

rownames(x.3) <- "15.24"

x.4 <- dapply(s, MARGIN=2, periodf.21.336)

rownames(x.4) <- "21.336"

x.5 <- dapply(s, MARGIN=2, periodf.27.432)

rownames(x.5) <- "27.432"

fall <- rbind(x.1,x.2,x.3,x.4,x.5)

name_fall <- paste(rownames(fall), "cm", sep="_")

fall <- data.frame(fall, limb="falling", rise_fall=name_fall)


###################################################

frequency <- rbind(rise, fall)

###################################################
##################quantile functions###############
q75 <- function(x){ifelse(x>quantile(x, 0.75), x, NA)}
q90 <- function(x){ifelse(x>quantile(x, 0.90), x, NA)}
q95 <- function(x){ifelse(x>quantile(x, 0.95), x, NA)}
q25 <- function(x){ifelse(x<quantile(x, 0.25), x, NA)}
q10 <- function(x){ifelse(x<quantile(x, 0.10), x, NA)}
q5 <- function(x){ifelse(x<quantile(x, 0.05), x, NA)}
###################################################

###dummy dataset####
y.2 <- y

####################

 
coredata(y.2) <- apply(coredata(y), MARGIN=2, q75)
q75.1 <- y.2

coredata(y.2) <- apply(coredata(y), MARGIN=2, q90)
q90.1 <- y.2

coredata(y.2) <- apply(coredata(y), MARGIN=2, q95)
q95.1 <- y.2

coredata(y.2) <- apply(coredata(y), MARGIN=2, q25)
q25.1 <- y.2

coredata(y.2) <- apply(coredata(y), MARGIN=2, q10)
q10.1 <- y.2

coredata(y.2) <- apply(coredata(y), MARGIN=2, q5)
q5.1 <- y.2

intermediate <- list(q75=q75.1, q90=q90.1, q95=q95.1, q25=q25.1, q10=q10.1, q5=q5.1)

names_of_list <- names(intermediate)

output <- vector("list")


for(i in 1:length(names_of_list)){

name. <- names_of_list[i]

working <- intermediate[[name.]]

working <- as.data.frame(working)

#for(k in 1:length(colnames(working))){

#name.col <- colnames(working)[k]

#list_indexes_lengths <- rle(!is.na(working[,k]))

#lengths_contiguous <- list_indexes_lengths$length[grep("TRUE", list_indexes_lengths$values)]

#MX <- max(lengths_contiguous)
#MD <- median(lengths_contiguous)

#inter.data <- data.frame(MX,MD)

#colnames(inter.data) <- c(paste(name.col, "max", sep="_"), paste(name.col, "median", sep="_"))

#output[[k]] <- inter.data

#} 

work <- function(working){
list_indexes_lengths <- rle(!is.na(working))

lengths_contiguous <- list_indexes_lengths$length[grep("TRUE", list_indexes_lengths$values)]

MAX <- max(lengths_contiguous)
MED. <- median(lengths_contiguous)

inter.data <- data.frame(MAX,MED.)

#colnames(inter.data) <- c(paste(name.col, "max", sep="_"), paste(name.col, "median", sep="_"))
}


output1 <- apply(working, MARGIN=2, work)

output2 <- data.frame(do.call(rbind, output1), name.)

output3 <- data.frame(site=rownames(output2), output2)

output[[i]] <- output3

}

duration <- do.call(rbind, output)

duration[,"name."] <- as.character(duration[,"name."])
duration[,"site"] <- as.character(duration[,"site"])

rownames(duration) <- NULL

rownames(frequency) <- NULL

CV. <- data.frame(CV(y))

CV. <- data.frame(site=as.character(rownames(CV.)), CV=CV.[,1])

frequency. <- melt.data.frame(frequency)

frequency. <- cast(frequency., variable~limb+rise_fall)

frequency.[,"variable"] <- as.character(frequency.[,"variable"])

colnames(frequency.)[1] <- "site"

duration. <- melt.data.frame(duration)

duration. <- cast(duration.,site~variable+name.)

final_output <- list(duration=sort_df(duration., vars="site"), frequency=sort_df(frequency., vars="site"), CV=sort_df(CV., vars="site"))

final_output. <- merge(final_output$duration, final_output$frequency, by="site")

final_output. <- merge(final_output., final_output$CV)



}


##################apply the McMahon Stuff#######################
hydrology <- McMahon_et_al._2003(b)
################################################################

################################################################
#################fixing up the data (maybe include?)############
################################################################
base.creek <- data.frame(do.call(rbind,strsplit(hydrology[,"site"], split="_level")))

base <- substr(base.creek[,1],1,4)

creek <- substr(base.creek[,1],6,9)

hydrology[,"site"] <- NULL
###############################################################

########################end product############################
hydrology <- data.frame(base=base, creek=creek, hydrology)
###############################################################



 


