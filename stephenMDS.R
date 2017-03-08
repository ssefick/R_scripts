#metaMDS for dimensions 1:10
stephenMDS <- function(x, n=10, ...){

require(vegan)

out <- vector(length=n, mode="list")

for(i in 1:n){

out[[i]] <- metaMDS(x, k=i, ...)

}

return(out)

}


#stressplot
stephensstress <- function(stephenMDS){

require(vegan)

stress <- vector()

dimension <- vector()

for(i in 1:length(stephenMDS)){

stress[i] <- stephenMDS[[i]]$stress

dimension[i] <- stephenMDS[[i]]$ndim


}

out.plot <- data.frame(dimension=dimension, stress=stress)

plot(out.plot)

}


#accessor function
getstephenMDS <- function(stephenMDS, dimension){stephenMDS[[dimension]]} 
