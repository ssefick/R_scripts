hourly_zoo <- function(x){aggregate(x, trunc(index(x), "hour"), mean)}
