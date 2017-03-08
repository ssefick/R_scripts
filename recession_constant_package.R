recession_constant_package <- function(...){

scripts2source <- dir("/home/ssefick/R_scripts/recession_constant_R_CODE", full.names=TRUE)
R_scripts <- grep(".*\\.R$", scripts2source)
scripts2source <- scripts2source[R_scripts]

for(i in 1:length(scripts2source)){
  source(scripts2source[i])
}

}
