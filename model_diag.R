model_diag <- function(Qobs, Qsim, k=NULL, name.){
out.nash <- NSeff(Qobs, Qsim)
out.MBe <- MBe(Qobs, Qsim)
out.det <- R_sq(Qobs, Qsim)

n <- length(Qobs)

#actual error
e <- Qsim-Qobs

#mean residual (Bias)
E_e <- (1/n)*sum(e)

#P vs. O

OP <- lm(Qsim~Qobs)

#standard deviation of the residuals (Precision)
S_e <- sqrt((1/(n-1))*(sum((e-E_e)^2)))

#root mean square error (accuarcy and Precision)
RMSE <- sqrt((E_e^2)+(S_e^2))

#relative error
r <- (Qsim-Qobs)/Qobs

#mre <- sum(r)*(100/n)
#mre25 <-  sum((abs(Qsim-Qobs)/Qobs)<=0.25)/n
#mre50 <-  sum((abs(Qsim-Qobs)/Qobs)<=0.5)/n


#mse

#MSE <- sum((Qsim-Qobs)/Qobs)*(100/n)

#%error

per_error <- 100*(e-1)

#####################################################
#AIC#################################################
#####################################################

AIC_Lopez <- function(Qobs, Qsim, k){

#after formula in 
#Lopez, R., J. Barragan, M.A. Colomer.  2007.  Flow resistance equations without explicit 
#estimation of the resistance coefficient for coarse-grained rivers.  Journal of Hydrology.
#338: 113-121

n <- length(Qsim)

AIC <- n*log(sum(((Qobs-Qsim)^2)/(n-k-1)))+2*(k+1)

return(AIC)

}

#####################################################
#####################################################

#####################################################
#BIC#################################################
#####################################################


BIC_Lopez <- function(Qobs, Qsim, k){

#after formula in 
#Lopez, R., J. Barragan, M.A. Colomer.  2007.  Flow resistance equations without explicit 
#estimation of the resistance coefficient for coarse-grained rivers.  Journal of Hydrology.
#338: 113-121

n <- length(Qsim)

BIC <- n*log(sum(((Qobs-Qsim)^2)/(n-k-1)))+k*log(n)

return(BIC)

}

#####################################################
#####################################################

AIC_L <- AIC_Lopez(Qobs, Qsim, k=k)

BIC_L <- BIC_Lopez(Qobs, Qsim, k=k)


full_model <- data.frame(NSeff=out.nash, MBe=out.MBe, R_sq=out.det, mean_resid=E_e, OP_intercept=coef(OP)[1] , OP_slope=coef(OP)[2] ,sd_resid=S_e, RMSE=RMSE, AIC=AIC_L, BIC=BIC_L ,n=length(Qsim))

row.names(full_model) <- name.

return(full_model)
}
