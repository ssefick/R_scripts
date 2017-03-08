altitude2STP <- function(alt_m, P_inHg){

#altitude has to be in meters
#altitude in meters is 0.3048*altitude in feet

#altimeter setting must be converted to inHg
#1mmHg is 0.039370079197408404inHg


STP <- P_inHg*((288-(0.0065*alt_m))/288)^5.2561 

return(STP)

}
