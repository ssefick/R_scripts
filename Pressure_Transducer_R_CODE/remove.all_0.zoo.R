remove.0.zoo <- function(data.in)
{

x <- data.in

function(x){sum(x==0)}

columns.tot <- apply(coredata(x), MARGIN=2, function(x)sum(x==0, na.rm=TRUE))

#index.length <- length(coredata(index(x)))

reduced <- x[,-grep("TRUE", columns.tot!=0)]

return(reduced)
}
