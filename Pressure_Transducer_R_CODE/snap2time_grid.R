snap2min <- function(zoo, min="00:15:00"){

require(chron)

min15 <- times(min)
a <- aggregate(zoo, trunc(time(zoo), min15), function(x) mean(x, na.rm=TRUE))
}


