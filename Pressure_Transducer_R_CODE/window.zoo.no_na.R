window.zoo.no_na <- function(data.in, day1, hour1="00:00:00", day2, hour2="00:00:00")
{

require(StreamMetabolism)

x <- window.chron(data.in, day1, hour1, day2, hour2)

columns.tot <- apply(coredata(x), MARGIN=2, function(x)sum(is.na(x)))

index.length <- length(coredata(index(x)))

grep("TRUE", columns.tot==index.length)

reduced <- x[,-grep("TRUE", columns.tot==index.length)]

return(reduced)
}
