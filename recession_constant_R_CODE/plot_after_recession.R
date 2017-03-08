system("rm -rf recession_constants_graphs")

system("mkdir recession_constants_graphs")

for(i in 1:length(storm_events_above_thresh)){

folder <- "/recession_constants_graphs/"

dir. <- system("pwd", intern=TRUE)

name. <- names(storm_events_above_thresh)[i]

name.pdf <- paste(dir.,folder,name.,".pdf", sep="")

pdf(name.pdf)

for(k in 1:length(colnames(storm_events_above_thresh[[name.]]))){

main. <- paste(name., colnames(storm_events_above_thresh[[name.]])[k], sep="_")

a <- storm_events_above_thresh[[name.]][!is.na(storm_events_above_thresh[[name.]][,k]),k]

plot(a, main=main., ylab="level in cm") 

max. <- which.max(a)

points(a[max.], col="red", pch=20)

points(a[max.+16], col="red", pch=20)

}

dev.off()
} 
