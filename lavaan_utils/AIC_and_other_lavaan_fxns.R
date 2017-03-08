#All From:
#http://jarrettbyrnes.info/ubc_sem/lavaan_materials/


#changelog: 1/3/12 - changed npar calculated to difference between baseline and object df to accomodate fixed.x mishegas

library(AICcmodavg)

AICc.lavaan<-function(object, second.ord=TRUE, c.hat = 1, return.K = FALSE){
	object <- as.list(fitMeasures(object))
  npar<-object$baseline.df - object$df
	if(return.K==T) return(object$npar)
	if(second.ord==F && c.hat>1) return(-2*object$logl/c.hat+2*npar)
	if(second.ord==F) return(object$aic)
    if(c.hat>1) return( -2*object$logl/c.hat+2*npar + 2*( npar*(object$npar+1))/(object$ntotal-npar-1))
    object$aic + 2*( npar*(npar+1))/(object$ntotal-npar-1)
}
    
aictab.lavaan<-function(cand.set, modnames, sort = TRUE, c.hat = 1, second.ord = TRUE, nobs = NULL){
	if(is.null(modnames)) modnames<-1:length(cand.set)
	# check.resp <- lapply(X = cand.set, FUN = function(b) formula(b)[2])
   # if (length(unique(check.resp)) > 1) 
   #     stop("You must use the same response variable for all models\n")
    Results <- NULL
    Results <- data.frame(Modnames = modnames)
    Results$K <- unlist(lapply(X = cand.set, FUN = AICc.lavaan, 
        return.K = TRUE, c.hat = c.hat,second.ord = second.ord))
    Results$AICc <- unlist(lapply(X = cand.set, FUN = AICc.lavaan, 
        return.K = FALSE, c.hat = c.hat,second.ord = second.ord))
    Results$Delta_AICc <- Results$AICc - min(Results$AICc)
    Results$ModelLik <- exp(-0.5 * Results$Delta_AICc)
    Results$AICcWt <- Results$ModelLik/sum(Results$ModelLik)
    if (length(unique(Results$AICc)) != length(cand.set)) 
        warning("\nCheck model structure carefully as some models may be redundant\n")
    if (second.ord == TRUE && c.hat == 1) {
        Results$LL <- unlist(lapply(X = cand.set, FUN = function(i) logLik(i)[1]))
    }
    if (second.ord == TRUE && c.hat > 1) {
        colnames(Results) <- c("Modnames", "K", "QAICc", "Delta QAICc", 
            "ModelLik", "QAICcWt")
        LL <- unlist(lapply(X = cand.set, FUN = function(i) logLik(i)[1]))
        Results$Quasi.LL <- LL/c.hat
        Results$c_hat <- c.hat
    }
    if (second.ord == FALSE && c.hat == 1) {
        colnames(Results) <- c("Modnames", "K", "AIC", "Delta AIC", 
            "ModelLik", "AICWt")
        Results$LL <- unlist(lapply(X = cand.set, FUN = function(i) logLik(i)[1]))
    }
    if (second.ord == FALSE && c.hat > 1) {
        colnames(Results) <- c("Modnames", "K", "QAIC", "Delta QAIC", 
            "ModelLik", "QAICWt")
        LL <- unlist(lapply(X = cand.set, FUN = function(i) logLik(i)[1]))
        Results$Quasi.LL <- LL/c.hat
        Results$c_hat <- c.hat
    }
    if (sort) {
        Results <- Results[rev(order(Results[, 6])), ]
        Results$Cum.Wt <- cumsum(Results[, 6])
    }
    else {
        Results$Cum.Wt <- NULL
    }
    class(Results) <- c("aictab", "data.frame")
    return(Results)
	
}


### The industrialization and Political Democracy Example 
### Bollen (1989), page 332
#model <- ' 
#  # latent variable definitions
#     ind60 =~ x1 + x2 + x3
#     dem60 =~ y1 + y2 + y3 + y4
#     dem65 =~ y5 + equal("dem60=~y2")*y6 
#                 + equal("dem60=~y3")*y7 
#                 + equal("dem60=~y4")*y8
#
#  # regressions
#    dem60 ~ ind60
#    dem65 ~ ind60 + dem60
#
#  # residual correlations
#    y1 ~~ y5
#    y2 ~~ y4 + y6
#    y3 ~~ y7
#    y4 ~~ y8
#    y6 ~~ y8
#'
#
#fit <- sem(model, data=PoliticalDemocracy)
#summary(fit, fit.measures=TRUE)
#
#
###no residual correlations
#modelNoRes <- ' 
#  # latent variable definitions
#     ind60 =~ x1 + x2 + x3
#     dem60 =~ y1 + y2 + y3 + y4
#     dem65 =~ y5 + equal("dem60=~y2")*y6 
#                 + equal("dem60=~y3")*y7 
#                 + equal("dem60=~y4")*y8
#
#  # regressions
#    dem60 ~ ind60
#    dem65 ~ ind60 + dem60
#
#'
#fitNoRes <- sem(modelNoRes, data=PoliticalDemocracy)
#
#aictab.lavaan(list(fitNoRes, fit), c("No Residual Covariances", "Residual Covariances"))


#d-seperation
###
##Implements Shipley's D-Separation method for SEMs with no latent variables, correlated errorrs, or cycles
##
## JEB
##
## Last Modified 8/4/2011
## Changelog
## 8/4/11 - now independent of ggm!  Also, added ability to handle multiple groups

dsepTest<-function(object, showall=F, adjust="none"){
	  
  cmats<-inspect(object, "coef")
  if(length(lavaanNames(object, type="lv") > 0)) stop("This SEM contains latent variables!")
  if(sum(cmats$theta[lower.tri(cmats$theta)]) != 0) stop("This SEM contains correlated errors.  Method not yet implemented, but see Shipley 2003.")

  dag<-DGlavaan(object)

  #check for being recursive
  if (!isAcyclic(dag)) stop("The SEM contains directed cycles!")
  
  #calculate Shipley's C (of Fisher's C)
  covMat<-object@Sample@cov
  n<-object@Sample@nobs
  
  #there may be one or more groups, so, generalized method to deal with this
  rlist<-lapply(1:object@Sample@ngroups, function(i) {

  	dimnames(covMat[[i]])<-dimnames(dag)
  	n<-object@Sample@nobs[[i]]
  	return(shipleyC(dag, covMat[[i]], n, showall=showall, adjust=adjust) )
  })

  if(object@Sample@ngroups==1) return(rlist[[1]]) #if only 1 group, just return the shipleyC list

  #otherwise, add group names
  names(rlist)<-object@Sample@group.label
  
  return(rlist)

}


####
# gets the directed graph from a lavaan object
DGlavaan<-function(object){
  cmats<-inspect(object, "coef")
    
  dg<-t(cmats$beta)
  dg<-(dg !=0)+0 #translate to 1s and 0s
  
  return(dg)
}

####
# determines if a graph is acyclic or not
####
isAcyclic<-function(amat){
	cycles<- (amat-t(amat)==1) + (amat==0)
	return(length(which(cycles==0)) == 0)
	}

###
#based on shipley.test from ggm library 1.0.4, but, this one also lets one see the dsep table itself
#if showall=T
#hrm...maybe add a pval for a normal distribution?
####

shipleyC<-function (amat, S, n, showall=F, adjust="none") 
{
    pval <- function(r, q, n) {
        df = n - 2 - q
        tval <- r * sqrt(df)/sqrt(1 - r * r)
        2 * pt(-abs(tval), df)
    }
    l <- basiSet(amat)
    k <- length(l)
    p <- rep(0, k)
    for (i in 1:k) {
        r <- pcor(l[[i]], S)
        q <- length(l[[i]]) - 2
        p[i] <- pval(r, q, n)
    }
    ctest <- -2 * sum(log(p))
    df <- 2 * k
    pv <- 1 - pchisq(ctest, df)
    ret<-list(ctest = ctest, df = df, pvalue = pv)

    if(!showall) return(ret)
    
    #to see tests for individual elements of the basis set - this is the new material - JEB
    ret$dsep<-sapply(l, function(x) {
    	vars<-c(paste(x[1], x[2], sep=","), "")
    	if(length(x)>2) vars[2] <- paste(x[3:length(x)], collapse=",")
    	return(vars)
    	})

   ret$dsep<-data.frame(Pair=ret$dsep[1,], Conditioning=ret$dsep[2,], "P(t)"=p.adjust(p, adjust))
    ret
}

####
##function to calculate partial correlations
##taken from ggm 1.0.4 to avoid having a dependency
####

pcor<-function (u, S) 
{
    k <- solve(S[u, u])
    -k[1, 2]/sqrt(k[1, 1] * k[2, 2])
}

####
##gets the basis set from a DAG
####

basiSet<-function(amat){
  if (!isAcyclic(amat)) stop("This graph contains cycles!")
	
  set<-which(amat+t(amat)==0 & lower.tri(amat), arr.ind=T) #get the vertices

  #get their parents and smoosh them together along with the vertices...and look up their names! 
  apply(set, 1, function(arow) {
  	colnames(amat)[c(arow, unique(c(which(amat[,arow[1]]==1), which(amat[,arow[2]]==1))))]
  })	
}


# # set.seed(2001)

# adf<-rbind(data.frame(x=rnorm(100, sd=20)), data.frame(x=rnorm(100, sd=20)))

# adf<-within(adf,{
	# grp = c(rep("A", 100), rep("B", 100))
	# y1<-rnorm(100, 20*x, sd=30)
	# y2<-rnorm(100, 10*x, sd=40)
	# y3 <- rnorm(100, 40*x+20*y2, sd=40)
	# })

# mod<-"y1 ~ x\ny2~x\ny3 ~ x+y2"

# asem<-sem(mod, data=adf)
# dsepTest(asem, showall=T)


# asem<-sem(mod, data=adf, group="grp")
# dsepTest(asem, showall=T)

#residual correlation lavaan
residualsCor<-function(fit){
  sampCov<-inspect(fit, "sampstat")$cov
  fitCov<-fitted(fit)$cov
  residCor<-cov2cor(sampCov)-cov2cor(fitCov)
  residCor
}
