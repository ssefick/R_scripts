import_all_bases2postgresql <- function(...){

#file stuff...
user <- system("echo $USER", intern=TRUE)
home <- "/home"
where_csv <- "Documents/Analyses/SERDP/base_data_master/SERDP_csv"
base_dir <- paste(home, user, where_csv,sep="/")
#####

#fgor
setwd(paste(base_dir, "fgor/", sep="/"))
import2postgresql()

#ftbn
setwd(paste(base_dir, "ftbn/", sep="/"))
import2postgresql()

#ftbr
setwd(paste(base_dir, "ftbr/", sep="/"))
import2postgresql()

#manf
setwd(paste(base_dir, "manf/", sep="/"))
import2postgresql()

#sgam
setwd(paste(base_dir, "sgam/", sep="/"))
import2postgresql()

#shsf
setwd(paste(base_dir, "shsf/", sep="/"))
import2postgresql()

#snwr
setwd(paste(base_dir, "snwr/", sep="/"))
import2postgresql()

#srs_
setwd(paste(base_dir, "srs_/", sep="/"))
import2postgresql()

#tnc_
setwd(paste(base_dir, "tnc_/", sep="/"))
import2postgresql()

}


