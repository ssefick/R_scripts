cms_with_level <- function(level_data, cms_data){
	require(zoo)
	require(chron)
	require(reshape)	
	level <- level_data
	cms <- cms_data
	date_time <- paste(as.Date(index(level)), index(level) - floor(index(level)))
	level.df <- data.frame(date_time, coredata(level))
	level.melt <- melt.data.frame(level.df, id.vars="date_time")
	level.melt$serial_num <- substr(level.melt[,"variable"], 3, 9)
	type <- data.frame(do.call(rbind, strsplit(as.character(level.melt[,"variable"]), split="_")))
	level.melt$type <- type[,2]
}
