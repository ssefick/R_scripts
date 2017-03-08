remove_columns_any_na <- function(x)
{
  
  out <- x[,-grep("TRUE", apply(x, 2, function(x)sum(is.na(x)))>0)]
  return(out)
}