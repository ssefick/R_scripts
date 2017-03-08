bollen.stine.boot.p <- function(fit, n.boot=10L){
# get the test statistic for the original sample
T.orig <- fitMeasures(fit, "chisq")
 
# bootstrap to get bootstrap test statistics
# we only generate 10 bootstrap sample in this example; in practice
# you may wish to use a much higher number
T.boot <- bootstrapLavaan(fit, R=n.boot, type="bollen.stine", FUN=fitMeasures, fit.measures="chisq")
 
# compute a bootstrap based p-value
pvalue.boot <- length(which(T.boot > T.orig))/length(T.boot)

out <- data.frame(chisq=mean(T.boot) ,p=pvalue.boot) 

return(out)
}
