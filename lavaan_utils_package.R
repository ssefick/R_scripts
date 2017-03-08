lavaan_utils <- function(...){

scripts2source <- dir("/home/ssefick/R_scripts/lavaan_utils", full.names=TRUE)
R_scripts <- grep(".*\\.R$", scripts2source)
scripts2source <- scripts2source[R_scripts]

for(i in 1:length(scripts2source)){
  source(scripts2source[i])
}

}
