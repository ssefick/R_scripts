laptop_database_connect_package <- function(...){

scripts2source <- dir("/home/ssefick/R_scripts/database_connect_laptop", full.names=TRUE)
R_scripts <- grep(".*\\.R$", scripts2source)
scripts2source <- scripts2source[R_scripts]

for(i in 1:length(scripts2source)){
  source(scripts2source[i])
}

}
