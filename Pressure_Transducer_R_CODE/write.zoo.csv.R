write.zoo.csv <- function(zoo, file="", index.name="Index", row.names=FALSE, col.names=NULL, ...){

	require(zoo)
	require(chron)

	index(zoo) <- format(index(zoo), enclosed = c("", "")) 
	write.zoo(zoo, file=file, sep=",")
}
