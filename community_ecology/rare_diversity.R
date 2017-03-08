rare_diversity <- function(L, n.boot=1000){
require(vegan)

if(any(apply(L,2,is.integer) == 0)) stop("L contains non-integer values\n possible solution community_round2int")

out <- vector(mode="list", length=n.boot)

min.n <- min(apply(L, 1, sum))

for(i in 1:n.boot){

out[[i]] <- diversity(rrarefy(L, sample=min.n), index = "shannon", MARGIN = 1, base = exp(1))

}

out <- do.call(rbind, out)

out <- list(mean=apply(out,2,mean), sd=apply(out,2,sd))

return(out)
}
