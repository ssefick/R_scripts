write.zoo.csv <- function(zoo, file="", index.name="Index", row.names=FALSE, col.names=NULL, ...){
	index(zoo) <- format(index(zoo), enclosed = c("", "")) 
	write.zoo(zoo, file=file, sep=",")
}
