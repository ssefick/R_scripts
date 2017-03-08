read.zoo.csv <- function(data){

require(chron)
require(zoo)

x <- read.csv(data)

index <- as.chron(as.character(x[,1]), format="%y/%m/%d %H:%M:%S", out.format=c(dates="Y/m/d", times="H:M:S"))


y <- x[,-1]

z <- zoo(y, index)

return(z)

}
