last_reading_zoo <- function(x){

out <- vector(mode="list",length=length(colnames(x)))

for(i in 1:length(colnames(x))){
out[[i]] <- tail(x[!is.na(x[,i]),i], n=1)

names(out)[i] <- colnames(x)[i]

}

out <- do.call(cbind, out)

return(out)

}
