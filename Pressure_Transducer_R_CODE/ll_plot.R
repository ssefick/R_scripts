ll_plot <- function(object, type="p", pch="."){

#this is just a wrapper function to plot.zoo
#with more better defaults

require(zoo)

	plot.zoo(object, type=type, pch=pch)

}
