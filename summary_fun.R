#summary

summary_fun <- function(x, ...)
{
  
  min.x <- min(x, ...)
  max.x <- max(x, ...)
  median.x <- median(x, ...)
  mean.x <- mean(x, ...)
  
  out <- data.frame(min=min.x, max=max.x, median=median.x, mean=mean.x)
  
  return(out)
  
}