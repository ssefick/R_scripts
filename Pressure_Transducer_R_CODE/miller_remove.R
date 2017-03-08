miller_remove <- function(data){

a <- grep("1043415", colnames(data))
a.1 <- grep("1042374", colnames(data))
a.2 <- grep("1042367", colnames(data))
a.3 <- grep("1043426", colnames(data))

remove <- c(a,a.1,a.2,a.3)

data.2 <- data[,-remove]

return(data.2)

}
