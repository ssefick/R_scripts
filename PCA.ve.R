PCA.ve<- function(fit){
# to extract variance from PCA model created using prcomp()
CP <- data.frame(colnames(fit[[2]]), fit[[1]]^2/(sum(fit[[1]]^2)))
names(CP) <- c("PCs", "Variance_explained")
print(CP)
plot(c(1:length(fit[[1]])), CP$Variance_explained, ylab= "Variance_explained", xlab="Principle Components", type="b", main="Scree Plot for PCA")
}
