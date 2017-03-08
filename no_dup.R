no_dup <- function(x){

y <- only_numeric(x)[,apply(only_numeric(x), 2, function(x)length(unique(x)))>1]

z <- cbind(not_numeric(x), y)

return(z)

}
