# Function to make lower scatterplot matrix with correlation coefficients and N
# modified code from the following Nabble post:
# http://r.789695.n4.nabble.com/pairs-no-axis-labels-values-for-upper-panel-td3234437.html
# requires "ltm" package

scat.plot <- function(dat, ...){

require(ltm)

panel.cor <- function(a, b, digits=2,  ...)
     {
		 library(ltm)

        usr <- par("usr"); on.exit(par(usr))
         par(usr = c(0, 1, 0, 1))
         x<-cbind(a,b)
         x<-na.omit(x)
         n <- nrow(x)
         corx <- cor(x,method="pearson")[1, 2]
		 cort <- rcor.test(x)[[2]][,3]
		 txt2 <-  paste("p =",format(c(cort, 0.123456789), digits=digits)[1])
         txt1 <- paste("r =",format(c(corx, 0.123456789), digits=digits)[1])
         txt3<-paste("N =",round(n,0))
         txt <- paste(txt1,"\n",txt2, "\n",txt3, sep="")
         text(0.5, 0.5, txt,cex=.8)
     }


pairs(dat, lower.panel=panel.cor, label.pos=0.5)
}
