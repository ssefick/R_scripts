make_row_delete_column <- function(x, column){

rownames(x) <- x[,grep(column, colnames(x))]

x <- x[,-grep(column, colnames(x))]

return(x)

}
