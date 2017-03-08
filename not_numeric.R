not_numeric <- function(x){
require(plyr)

y <- x[,grep("FALSE", do.call(cbind, colwise(is.numeric)(x)))]

return(y)

}
