baro_comp <- function(level_logger, base="NULL"){

#barometric compensation
#fgor     srs_ 1042707     no                    OK srs_ (30ish miles)       
#ftbn     ftbn 1042293     yes                   --  
#ftbr     ftbr 1042302     yes                   --
#manf     manf 1042720     yes                   --
#sgam     ftbr 1042302     no                    OK ftbr (20ish miles)
#shsf     DHEC             no                    DHEC manf (40ish miles)
#snwr     DHEC             no                    DHEC manf (40ish miles)
#srs_     srs_ 1042707     yes                   --
#tnc_     ftbn 1042293     no                    OK ftbn (20ish miles)


#####################################################
####barologgers hard coded- remove for generality####
#####################################################
#Fort Benning
if(base=="ftbn"){
baro_logger <- "01042293"
print("ftbn")
}else{

#The Nature Conservancy Lands Easements @ ftbn
if(base=="tnc_"){
baro_logger <- "01042293"
print("tnc_")
}else{ 	

#Savannah River Site
if(base=="srs_"){
baro_logger <- "01042707"
print("srs_")
}else{

#Fort Gordon
if(base=="fgor"){
baro_logger <- "01042707"
print("fgor")
}else{ 	

#Mancester State Forest
if(base=="manf"){
baro_logger <- "01042720"
print("manf")
}else{ 	

#Sandhills State Forest
if(base=="shsf"){
baro_logger <- "DHEC"
print("shsf")
}else{ 	

#Sand Hills Wildlife Refuge
if(base=="snwr"){
baro_logger <- "DHEC"
print("snwr")
}else{ 	

#Southern Gamelands
if(base=="sgam"){
baro_logger <- "01042302"
print("sgam")
}else{ 	

#Fort Bragg
if(base=="ftbr"){
baro_logger <- "01042302"
print("ftbr")
}else{
print("error base of unkown origin selected")
}
}
}
}
}
}
}
}
}

####################################################
####################################################
####################################################

  a <- level_logger
  baro_logger.1 <- paste(baro_logger, "level", sep="_")	

###############################################################
#########check to make sure the barologger column exists#######
###############################################################
#doesn't work SAS 2013-01-31
#if(match(baro_logger.1, colnames(a), nomatch=0)==0)stop("Barometric Pressure Data Not in Dataframe")

#works
if(length(grep(baro_logger.1, colnames(a)))!=1)stop("Barometric Pressure Data Not in Dataframe")




###############################################################
###############################################################
###############################################################

  index_baro <- grep(baro_logger.1, colnames(a))		
  indexes. <- grep("level", colnames(a))	
  #uncomment if you want to include barometric pressure in final output	
  #indexes. <- indexes.[-grep("TRUE", !is.na(match(indexes., index_baro)))]
	a[,indexes.] <- a[,indexes.]-a[,index_baro]
return(a)
#12cm/100m and 9.5m @ sealevel  so 9.5-(altitude*0.0012)=zero_point
}
