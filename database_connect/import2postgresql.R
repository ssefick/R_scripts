import2postgresql <- function(null.var=NULL)
{

require(RPostgreSQL)

m <- PostgreSQL()
con <- dbConnect(m, user="ssefick", password="monobrodobro", dbname="SERDP_Physical")

workdir <- getwd()
for (x in dir(workdir,pattern='.csv$')){
	d <- read.csv(paste(workdir,'/',x,sep=''), sep=",",	header=TRUE)
  colnames(d) <- tolower(colnames(d))
  table_name <- gsub(".csv", "", x)   
	dbRemoveTable(con, tolower(table_name), d, overwrite=TRUE)	
	dbWriteTable(con, tolower(table_name), d, overwrite=TRUE)

}
} 
