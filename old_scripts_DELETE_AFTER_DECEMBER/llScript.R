

###################################################level logger read in#############################################
read.ll <- function(...)
	{ 
if(!require(chron))
   stop("Package 'chron' required but not installed.")
if(!require(zoo))
   stop("Package 'zoo' required but not installed.")
list.of.files <- list.files()
fnames <- list.of.files[grep(".txt", list.of.files, fixed=TRUE)]
length.files <- length(fnames)
df <- vector(mode = "list", length = length.files)
list.of.names <- strsplit(fnames, split="_")
		for(i in seq(along = fnames))
		{
		df[[i]] <- read.table(fnames[i], skip=45, as.is=TRUE)
		#lengths of the dataframes because the last two lines of the .lev file are garbage#
		length.1 <- length(df[[i]][,1]) 
		length.2 <- length(df[[i]][,1])-1
		#remove the garbage
		df[[i]] <- df[[i]][-c(length.1, length.2),]
		#make chron class for datetime
		df[[i]] <- data.frame(chron(as.character(df[[i]][,1]), as.character(df[[i]][,2]), format=c(dates="Y/m/d", times="H:M:S")), as.numeric(df[[i]][,3]), as.numeric(df[[i]][,4]))
#change names of columns
#get string for serial num		
ll_name <- list.of.names[[i]][1]

colnames(df[[i]]) <- c(paste(ll_name, "datetime", sep="_"), paste(ll_name, "level", sep="_"), paste(ll_name, "temp", sep="_"))

df[[i]] <- melt.data.frame(df[[i]], id.vars=paste(ll_name, "datetime", sep="_"))

colnames(df[[i]])[1] <- "date_time"

#make zoo object
#num <- df[[i]][,2:3]

#apply(num, MARGIN=2, as.numeric)

#df[[i]] <- zoo(num, df[[i]][,paste(ll_name,"datetime", sep="_")])
}			

df <- do.call(rbind, df)

#make zoo

tozoo <- function(x) zoo(x$value, x$date_time) 
df <- do.call(merge, lapply(split(df, df$variable), tozoo))


return(df)
}

#####################################################################################################################




