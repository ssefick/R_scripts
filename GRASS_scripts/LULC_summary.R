LULC_summary <- function(basin_map, LULC_data){
require(spgrass6)

#print messages
print("make sure mask is set to basins")
print("make sure that region is set")

#paste together charactervector for reporting
input <- paste(basin_map, LULC_data, sep=",")

#projection units from GRASS
unitsgrab <- execGRASS("g.proj", flags=c("j"), intern=TRUE)

units <- unitsgrab[grep("to_meter", unitsgrab)]

#test in srs
#"srs_basins_final@srs__10m,srs__LULC_2006@LULC_2006"

#execute GRASS command
x <- execGRASS("r.stats", flags=c("a"), input=input, intern=TRUE)

#massage data into right format
out <- as.data.frame(do.call(rbind, strsplit(x, " ")))

#give colnames
colnames(out) <- c("basin", "LULC_2006_cat", "area")

#combine with projection units
out <- data.frame(out, units=units)


#test if units are m
if(sum(out[,"units"]!="+to_meter=1")==0){

out[,"units"] <- "m"

return(out)

}else{

stop("units not m!!!!")

}
}

