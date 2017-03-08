zplot_subset <- function(x, columns, type="p"){

if(is.numeric(columns)){
plot(x[,columns], ylab=colnames(x)[columns], type=type, pch=".")
}else if(is.character(columns)){
plot(x[,columns], ylab=columns, type=type, pch=".")
} else{
print("something is up")
}
}


