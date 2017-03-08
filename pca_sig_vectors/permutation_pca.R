#####################################
#Load libraries
#####################################
library(BiodiversityR)
library(vegan)
#####################################
# Function sigpca2
# This code tests for the significance of loadings as described in these papers:
# (Jackson, Ecology 1993, 74: 2204-2214; Peres-Neto and al. 2003. Ecology 84:2347-2363)
# Becuase this has not been implemented into a package, we need to enter the
# function manually.
# Select (or copy/paste) all of this function and send it to R at once.
# After that, we can use the sigpca() function as any other function (see below)
#####################################

sigpca2<-function (x, permutations=1000, ...)
{
   pcnull <- princomp(x, ...)
   res <- pcnull$loadings
   out <- matrix(0, nrow=nrow(res), ncol=ncol(res))
   N <- nrow(x)
   for (i in 1:permutations) {
       pc <- princomp(x[sample(N, replace=TRUE), ], ...)
       pred <- predict(pc, newdata = x)
       r <-  cor(pcnull$scores, pred)
       k <- apply(abs(r), 2, which.max)
       reve <- sign(diag(r[k,]))
       sol <- pc$loadings[ ,k]
       sol <- sweep(sol, 2, reve, "*")
       out <- out + ifelse(res > 0, sol <=  0, sol >= 0)
   }
   out/permutations
}
######################################
# End of function
######################################
