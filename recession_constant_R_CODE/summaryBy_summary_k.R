#x <- read.csv("k_final.csv")

sumfun <- function(x, ...){
  c(mean=mean(x, ...), median=median(x, ...), sd=sd(x, ...), CV=CV(x, ...), max=max(x, ...), min=min(x, ...),n=length(x))
}

out <- summaryBy(k~site, data=x, FUN=sumfun)

