serdp_name_split <- function(x){
x <- as.character(x[,1])
base  <- substr(as.character(x),1,4)
creek <- substr(as.character(x),5,8)

out <- data.frame(base=base, creek=creek)

return(out)

}
