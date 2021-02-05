setwd('C:\\Users\\Skyler Richmond\\Desktop\\Evolution\\Tasks\\Task_02')
Data<- read.csv('http://jonsmitchell.com/data/beren.csv', stringsAsFactors=F)
write.csv(Data, 'rawdata.csv', quote=F)
length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
head(Data)
Data[1,]
Data[1:3,]
Data[1:3, 4]
Data[1:5, 1:3]
Feeds<- which(Data[,9] =='bottle')
berenMilk <-Data[Feeds,]
head(berenMilk)
Feeds<- which(Data[,'event'] == 'bottle')
Feeds<- which(Data$event == 'bottle')
dayID <-apply(Data, 1, function(x) paste(x[1:3], collapse='-'))
dateID <-sapply(dayID, as.Date, format= "%Y-%m-%d", origin ="2019-04-18")
Data$age<- dateID - dateID[which(Data$event == 'birth')]
head(Data)
beren2<- Data
beren3<-beren2[order(beren2$age),]
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)
bye()
Question 01: The weight and nap time are not recorded like the amount of milk that Beren is given in a bottle so the hypotheses for those could not be tested, whereas the milk one can.  
setwd(beren3)
Feeds<- which(beren3$event =="bottle")
avgMilk<- mean(beren3$value[Feeds])
Units are ounces. 
Value column was used to show the values that were similar to the values for "bottle".
The brackets are used to show enclosed data in the vector called Feeds it is important to enclose that data to use it when finding the mean. 
avgFeed<- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
varFeed<-tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed<- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds<-tapply(beren3$value[Feeds], beren3$age[Feeds], length)
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds, beren3$age[Feeds])
beren <- read.csv("http://jonsmitchell.com/data/beren.csv
", stringsAsFactors=F)
dayID <- apply(beren, 1, function(x) paste(x[1:3], collapse="-"))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
beren$age <- dateID - dateID[which(beren$event == "birth")]
beren2 <- beren[order(beren$age),]
beren2$value <- as.numeric(beren2$value)
beren3<-beren2
Feeds<- which(beren3$event =="bottle")
avgMilk<- mean(beren3$value[Feeds])
avgFeed<- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
varFeed<-tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed<- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds<-tapply(beren3$value[Feeds], beren3$age[Feeds], length)
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor<- cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)
berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
boxplot( beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab="who gave the bottle", ylab= " amount of milk consumed (oz)" )
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01 )
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
pdf("r02b-totalMilkByDay.pdf", height =4, width=4)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()
Question 02: The graph is hard to interrpret because the viewing window has not been adjusted so that the data can fit and be viewed better instead of all smshed together inn one place. A simple change of the minimums and maximums of the graphs axes can fix the issue. 
source("http://jonsmitchell.com/code/plotFxn02b.R")
unique(beren3$event)
Self Quiz: The graph that came from the code on eCampus is easier to understand as the information is placed with a different time scale on the x axis changing the viewing window to make it easier. The on eon ecampus has the saame data essentially but it being put into 

beren4<- which(Data[,9] == 'naps')
Naps<-Data[beren4,]
head(Naps)
beren4<- which(Data[,'event']=='naps')
StartTimeID<- apply(beren,1, function(x) paste(x[5:6], collapse="-"))
StopTimeID<- apply(beren,1, function(x) paste(x[7:8], collapse="-"))
beren$naps<-StopTimeID - StartTimeID[which(Data$event=='nap')]
beren4<- beren3[Naps,]
startHour<-(beren4$start_hour)
startMin<- (beren4$start_minute)
stopHour<-(beren4$end_hour)
stopMin<-(beren4$end_minute)
startHour
startMin
stopHour
stopMin
beren4$sleepTime<- ((stopHour - startHour)*60)+(stopMin-startMin)
beren4
totalNap<-tapply(beren4$sleepTime, beren4$age, sum)
totalNap
par(las=1,mar=c(5,5,1,1),mgp=c(2,0.5,0),tck=-0.01)
plot(as.numeric(names(totalNap)),totalNap,type="b",pch=16,xlab="age in days",ylab="Nap time in minutes")
cor.test(beren4$start_hour,beren4$sleepTime)
There is a negative correlation between the two. 