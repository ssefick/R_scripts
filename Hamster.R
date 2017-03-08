hamster_summary <- function(...){
require(RSQLite)
require(chron)
require(doBy)
require(ggplot2)
require(gridExtra)

setwd("/home/ssefick/.local/share/hamster-applet")

con <- dbConnect(RSQLite::SQLite(), "hamster.db")

activities <- dbReadTable(con, "activities")

facts <- dbReadTable(con, "facts")

######bring in tags###############################
tags <- dbReadTable(con, "tags")

fact_tags <- dbReadTable(con, "fact_tags")

fact_tag_merge <- merge(fact_tags, tags, by.x="tag_id", by.y="id", all=TRUE)

facts <- merge(facts, fact_tag_merge, by.x="id", by.y="fact_id", all=TRUE)
##################################################

y <- merge(activities, facts, by.x="id", by.y="activity_id", all=TRUE)

colnames(y)[c(1,8)] <- c("id.activities","id.facts")

y <- y[!is.na(y$start_time),]

dates.start <- do.call(rbind,strsplit(y$start_time, split=" "))[,1]
times.start <- do.call(rbind,strsplit(y$start_time, split=" "))[,2]


y$start.time <- chron(dates.=dates.start, times.=times.start, format=c(dates="y-m-d", times="h:m:s"))

dates.end <- do.call(rbind,strsplit(y$end_time, split=" "))[,1]
times.end <- do.call(rbind,strsplit(y$end_time, split=" "))[,2]


y$end.time <-chron(dates.=dates.end, times.=times.end, format=c(dates="y-m-d", times="h:m:s"))

y$time_spent <- y$end.time-y$start.time

y$time_spent <- 24*y$time_spent

date2week <- as.Date(y$start.time)

y$year <- format(date2week, format="%G")

y$week <- format(date2week, format="%W")

y$day <- format(date2week, format="%d")

#change colnames of merge
colnames(y)[grep("name.x", colnames(y))] <- "name"

colnames(y)[grep("name.y", colnames(y))] <- "tags"
###

out.names <- summaryBy(time_spent~name+year+week, FUN=sum, data=y)

names <- qplot(week,time_spent.sum, data=out.names, geom="bar")+facet_wrap(~name)

#print(names)

out.tags <- summaryBy(time_spent~name+tags+year+week, FUN=sum, data=y)

out.tags <- na.omit(out.tags)

tags <- qplot(week,time_spent.sum, data=out.tags, geom="bar")+facet_wrap(~tags)

#summarys
#remove mq_8 because it is not a complete sample
out.whole.samples <- out.tags[out.tags[,"tags"]!="mq_8",]
sum.by.tag <- summaryBy(time_spent.sum~tags, data=out.whole.samples, FUN=sum)

b <- sum.by.tag[,"time_spent.sum.sum"]

boxplot.tags <- qplot(rep("samples", length(b)), b, xlab="", ylab="time in hours")+geom_boxplot()

#table of summary stats



#mean by day
sum.day <- summaryBy(time_spent~name+year+week+day, FUN=sum, data=y)

mean.day <- rbind(mean=mean(sum.day[sum.day[,"name"]=="bugs","time_spent.sum"]), median=median(sum.day[sum.day[,"name"]=="bugs","time_spent.sum"]),sd=sd(sum.day[sum.day[,"name"]=="bugs","time_spent.sum"]),CV=CV(sum.day[sum.day[,"name"]=="bugs","time_spent.sum"]),n=length(sum.day[sum.day[,"name"]=="bugs","time_spent.sum"]))

colnames(mean.day) <- "stats (hrs/d)"

#####working code#######
#stats per week
sum.week <- summaryBy(time_spent.sum~week, FUN=sum, data=out.tags)
mean.week <- rbind(mean=mean(sum.week$time_spent.sum.sum),median=median(sum.week$time_spent.sum.sum),sd=sd(sum.week$time_spent.sum.sum),CV=CV(sum.week$time_spent.sum.sum),n=length(sum.week$time_spent.sum.sum))
colnames(mean.week) <- "stats (hrs/week)"
########################

##streams per week#########################

str_per_week <- length(unique(out.tags[,"tags"]))/length(unique(out.tags[,"week"]))

names(str_per_week) <- "streams/week"
###########################################

#stats per stream
table.stats <- rbind(mean=mean(sum.by.tag$time_spent.sum.sum), median=median(sum.by.tag$time_spent.sum.sum), sd=sd(sum.by.tag$time_spent.sum.sum), CV=CV(sum.by.tag$time_spent.sum.sum), n=length(sum.by.tag$time_spent.sum.sum))

colnames(table.stats) <- "stats (hrs/stream)"

table.stats <- cbind(table.stats, mean.day, mean.week)

#print(tags)

grid.arrange(names, tags, boxplot.tags, tableGrob(round(table.stats, digits=2)), ncol=2, nrow=2)

}
