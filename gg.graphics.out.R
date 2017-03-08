gg.graphics.out <- function(ggplot.object, name="test", type="pdf", ...){

#can wrap ggplot.object in list() and print out multiple graphs to one file...


require(ggplot2)


#pdf
if(type=="pdf"){

out <- paste(name, ".", type, sep="")

pdf(out, ...)

print(ggplot.object)

dev.off()
}

#svg
if(type=="svg"){

out <- paste(name, ".", type, sep="")

svg(out, ...)

print(ggplot.object)

dev.off()
}

}
