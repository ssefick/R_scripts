pca.boot <- function (x, permutations=1000, ...) 
{ 
  pcnull <- princomp(x, ... ) 
  res <- pcnull$loadings[,1] 
  bsresults = matrix( rep.int(NA, permutations*NROW(res)) , 
nrow=permutations, ncol=NROW(res) ) 
  N <- nrow(x) 
  for (i in 1:permutations) { 
      pc <- princomp(x[sample(N, replace=TRUE), ], ... ) 
      pred <- predict(pc, newdata = x) 
      r <-  cor(pcnull$scores, pred) 
      k <- apply(abs(r), 2, which.max) 
      reve <- sign(diag(r[k,])) 
      sol <- pc$loadings[ ,k] 
      sol <- sweep(sol, 2, reve, "*") 
      bsresults[i,] <- t(sol[,1]) 
  } 
  out <- apply( bsresults, 2, quantile, c(0.05, 0.95) ) 

out <- as.data.frame(out)

colnames(out) <- colnames(x)

not_contain_0 <- names(apply(out, 2, function(x)sum(sign(x)))[apply(out, 2, function(x)sum(sign(x)))!=0])

out.list <- list(out, not_contain_0)

return(out.list)

} 
