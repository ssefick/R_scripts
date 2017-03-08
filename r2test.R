r2test <- function(df, cutoff=0.8, use="pairwise.complete.obs") {


only_numeric <- function(x){
require(plyr)

y <- x[,do.call(cbind,colwise(is.numeric)(x))]

return(y)

}


not_numeric <- function(x){
require(plyr)

y <- x[,grep("FALSE", do.call(cbind, colwise(is.numeric)(x)))]

return(y)

}

  #since this only removes columns then you can strip off
  #and recombine the labels later
  labels <- not_numeric(df)
  d <- only_numeric(df)  

  

  if (cutoff > 1 || cutoff <= 0) {
    stop(" 0 <= cutoff < 1")
  }
  if (!is.matrix(d) && !is.data.frame(d)) {
    stop("Must supply a data.frame or matrix")
  }

  #make r-sq into r
  r2cut = sqrt(cutoff);
  cormat <- cor(d, use=use);
  
  #greater than r
  bad.idx <- which(abs(cormat)>r2cut,arr.ind=TRUE);
  
  #bottom diagnal
  bad.idx <- matrix(bad.idx[bad.idx[,1] > bad.idx[,2]], ncol=2);
  
  #randomly remove columns...  
  drop.idx <- ifelse(runif(nrow(bad.idx)) > .5, bad.idx[,1], bad.idx [,2]);

  #  
  if (length(drop.idx) == 0){
  sub <- 1:ncol(d)
  } else {
  sub <- (1:ncol(d))[-unique(drop.idx)]
  }

  out <- d[,sub]

out. <- cbind(labels, out)

return(out.)

} 
