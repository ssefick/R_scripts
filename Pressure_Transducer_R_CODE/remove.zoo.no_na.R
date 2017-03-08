remove.zoo.no_na <- function(data.in)
{

require(zoo)

x <- data.in

columns.tot <- apply(coredata(x), MARGIN=2, function(x)sum(is.na(x)))

reduced <- x[,columns.tot==0]

return(reduced)
}
