only_numeric_dataframe <- function(x){
require(plyr)

y <- x[,do.call(cbind,colwise(is.numeric)(x))]

return(y)

}
