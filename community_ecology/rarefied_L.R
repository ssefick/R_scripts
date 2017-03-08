
rarefied_L <- function(X, n.sample, n.runs){
require("vegan")
out.i <- vector("list", length=n.runs)
for(i in c(1:n.runs)){ out.i[[i]] <-rrarefy(floor(X), n.sample) }
out <- Reduce('+', out.i)/n.runs
return(out)

}