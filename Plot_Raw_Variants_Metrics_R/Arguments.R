#!/usr/bin/env Rscript

## Collect arguments
argv <- commandArgs(trailingOnly=TRUE)
 
## Default setting when no arguments passed
if(length(argv) < 1) {
  argv <- c("--help")
}
 
## Help section
if("--help" %in% argv){
    cat("The R Script
Arguments:
--arg1=someValue   - numeric, blah blah
--arg2=someValue   - character, blah blah
--arg3=someValue   - logical, blah blah
--help              - print this text
 
Example:
./test.R --arg1=1 --arg2=output.txt --arg3=TRUE \n\n")
 
  q(save="no")
}
 
## Parse arguments (we expect the form --arg=value)
parseArgs <- function(x) strsplit(sub("^--", "", x), "=")
argvDF <- as.data.frame(do.call("rbind", parseArgs(argv)))
argvL <- as.list(as.character(argvDF$V2))
names(argvL) <- argvDF$V1
 
## Arg1 default
if(is.null(argv$arg1)) {
  ## do something
}
 
## Arg2 default
if(is.null(args$arv2)) {
  ## do something
}
 
## Arg3 default
if(is.null(argv$arg3)) {
  ## do something
}
