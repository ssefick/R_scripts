LULC_summary_no_m_check <- function(basin_map, LULC_data){
require(spgrass6)

#print messages
print("make sure mask is set to basins")
print("make sure that region is set")

#paste together charactervector for reporting
input <- paste(basin_map, LULC_data, sep=",")

#projection units from GRASS
#unitsgrab <- execGRASS("g.proj", flags=c("j"), intern=TRUE)

#units <- unitsgrab[grep("to_meter", unitsgrab)]

#test in srs
#"srs_basins_final@srs__10m,srs__LULC_2006@LULC_2006"

#execute GRASS command
x <- execGRASS("r.stats", flags=c("p"), input=input, intern=TRUE)

#massage data into right format
out <- as.data.frame(do.call(rbind, strsplit(x, " ")))

#give colnames
colnames(out) <- c("basin", "LULC_2006_cat", "per")

#combine with projection units
#out <- data.frame(out, units=units)


#not_test_for_m

#out[,"units"] <- "m"

return(out)

}

