sugar2add <- function(sugar = 1.044, PPG=1.046, gal = 4, raise_to = 1.065, units="g"){

  Convert_package()
  
  #Light Liquid Malt Extract (LME)
  #1.036
  #Light Dry Malt Extract (DME)
  #1.044
  #Corn Sugar (dexrose)
  #1.046
  #Molasses
  #1.036
  #Sugar (sucrose, cane, etc.)
  #1.046
  #2-row Pale Malt
  #1.027
  #Wheat Malt
  #1.029
  #Wheat, Torrefied
  #1.027
  
  
percent <- (as.numeric(substr(raise_to, start=4, stop=5))-as.numeric(substr(sugar, start=4, stop=5)))/as.numeric(substr(PPG, start=4, stop=5))

pounds_of_sugar <- percent*gal

if(units=="lbs"){
return(data.frame(sugar=round(pounds_of_sugar, digits=2), units="lbs"))
}

if(units=="g"){
  return(data.frame(sugar=round(lbs2g(pounds_of_sugar), digits=2), units="g"))
}

}
