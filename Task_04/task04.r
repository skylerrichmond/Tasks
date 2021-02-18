source("http://jonsmitchell.com/code/fxn05.R")
Pop1<- simPop(Popsize=50, nGenerations=100, initial_p=0.5, h=1, s=0)
plot(1:nrow(Pop1), Pop1[,1], ylim=c(0, 1), type= "l", xlab="generation", ylab="allele freq.", lwd=2)
lines(1:nrow(Pop1), Pop1[,2], lwd=2, col='red')
legend("topleft", legend=c("a", "b"), col=c("black", "red"), lwd=2, bty="n")
plotFit( nruns=10, n=50, ngens=100, init_p=0.5, h=1, s=0)
dev.off()
par(mar=c(1, 1, 1, 1))
par("mar")
graphics.off()
Expectation<-c(10, 10, 10, 10)
Observed<-c(15, 15, 5, 5)
Chisq<- sum(((Expectation- Observed)^2)/Expectation)
barplot(rbind(Expectation, Observed), beside=T, main=bquote(chi^2 ~ "=" ~.(Chisq)), legend.text=c("expected", "observed"))
Observed<-c(5, 0, 0, 35)
Chisq<- sum(((Expectation- Observed)^2)/Expectation)
barplot(rbind(Expectation, Observed), beside=T, main=bquote(chi^2 ~ "=" ~.(Chisq)), legend.text=c("expected", "observed"))
Observed<-c(2, 3, 10, 30)
Chisq<- sum(((Expectation- Observed)^2)/Expectation)
barplot(rbind(Expectation, Observed), beside=T, main=bquote(chi^2 ~ "=" ~.(Chisq)), legend.text=c("expected", "observed"))
The X^2 value is 10 and the same for when all of the observations are in one category. When it comes to evenness of the bars because the closer it is to the X^2 values the bars will be more even. 
results<-read.csv("http://jonsmitchell.com/data/biol112labresults.csv", stringsAsFactors=F)
counts<-results[,c("yellow", "red", "green", "blue", "black", "tan")]
backgrounds<-c("White", "Red", "Yellow", "Green", "Blue", "Black")
backgroundsCol<-c("white", "#d53e4f", "#fee08b", "#abdda4", "#3288bd", "black")
calcChi(counts[1,])
Chisqs <-apply(counts, 1, calcChi)
plotChis(counts)
When the values is high the bars are more uneven with one bar really high compared to when it is low and the bars are even. The plot.Chis() fucntion shows that the smaller the chi-square value the closer the measurment predicted is to what is actually observed, so when the value is high the predicted and observed are farther apart and the phenotypes for those are distributed unevenly. 
The average chi-squared is 60.99 or 61 that should be interpreted as uneven distribution of phenotypes and differences in the predicted versus observed values with such a high value. 
Avg<-mean(Chisqs)
The average is much higher than the critical value from the packet. 
The value does differ by background that inputs different values for selection. 
backgroundAvgs<-tapply(Chisqs, results[,3], mean)
propSig<-length(which(Chisqs>11.70))/length(Chisqs)
percSig<-round(100 * propSig)
head(percSig)
This number does suprise me because it is more than I thought. 
I think due to the fact it is such a high value there may be more at play than just natural selection. 
par(las=1, mar=c(4, 4, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01, cex.axis=1)
hist(Chisqs, main="", xlab="chi-squared values", ylab="frequency")
par(las=1, mar=c(4, 4, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01, cex.axis=1)
plot(1, 1, xlim=c(0, 400), ylim=c(1, 8.5), xlab="", ylab="", type="n", yaxt="n")
axis(2, at =1:length(backgrounds), labels=backgrounds)
mtext(side=1, expression(chi^2), cex=1.75, line=2.5)
counter<-1 for (i in backgrounds) {
Data <- Chisqs[which(results[,3]==i)]
addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])
counter<-counter+1
}
counter<-1 for (i in seq) {}(i in backgrounds) {Data <- Chisqs[which(results[,3]==i)] addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])counter<-counter+1}
counter<-1(i in backgrounds) {Data <- Chisqs[which(results[,3]==i)] addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])counter<-counter+1}
counter<-1 
for (i in backgrounds){
Data <- Chisqs[which(results[,3]==i)]
addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])
counter<-counter +1
}
counter<-1 for (i in backgrounds) {Data <- Chisqs\[which(results\[,3\]==i)\] addHist(Y=counter, Dat=Data, Color=backgroundCol\[counter\]) counter<-counter+1}
counter<-1 
for (i in backgrounds){
Data <- Chisqs[which(results[,3]==i)]
addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])
counter<-counter +1
}

par (las=1, mar=c (4, 4, 1, 1), mgp=c (2, 0.5, 0), tck = -0.01, cex.axis = 1)
hist (Chisqs, main="", xlab="chi-squared values", ylab="frequency")
par (las=1, mar=c (4, 4, 1, 1), mgp=c (2, 0.5, 0), tck = -0.01, cex.axis = 1)
plot (1, 1, xlim=c(0, 400), ylim=c(1, 8.5), xlab="", ylab="", type="n", yaxt="n")
axis (2, at = 1:length(backgrounds), labels = backgrounds)
mtext (side=1, expression(chi^2), cex=1.75, line=2.5)
backgroundCol<- c("white", "#d53e4f", "#fee08b", "#abdda4", "#3288bd", "black")
counter <- 1
for (i in backgrounds) {
Data <- Chisqs[which(results[,3] == i)]
addHist (Y=counter, Dat=Data, Color=backgroundCol[counter])
counter <- counter + 1
}
abline( v=11.70, lty=2, lwd=2, col='black')
There were not many menainngul differences but the backgrounds coresponding with the color of the toothpick so it could blend in or camoflauge were least significant.
Simulation<-simDraws(10000)
addHist(Y=7, Dat=Simulation, Color="lightgray")
mtext(side=2, at=7, line=0, "simulated")
abline(v=11.70, lty=2, lwd=2)
Fit<-c(1, 1, 1, 1, 1, 1)
names(Fit)<- 1:6
Simulation2<-simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation2, Color=rgb(0, 0, 0, 0.25))
Fit<-c(00.1, 1, 1, 1, 1, 1)
names(Fit)<- 1:6
Simulation3<-simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation3, Color=rgb(0, 0, 0, 0.25))
Fit <- c(0.5, 0.6, 0.7, 1, 1, 1)
names(Fit)<- 1:6
Simulation4<-simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation4, Color=rgb(0, 0, 0, 0.25))
Fit <- c(0.1, 0.2, 0.3, 0.4, 0.5, 1)
names(Fit)<- 1:6
Simulation5<-simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation5, Color=rgb(0, 0, 0, 0.25))
Fit<-c(0.1, 0.1, 0.1, 0.1, 0.1, 1)
names(Fit)<- 1:6
Simulation6<-simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation6, Color=rgb(0, 0, 0, 0.25))
mtext(side=2, at=8, line=0, "sel. sim.")
Simulation7<-c(Simulation2, Simulation3, Simulation4, Simulation5, Simulation6)
addHist(Y=8, Dat=Simulation7, Color=rgb(0, 0, 1, 0.25))
The mixture is very similar to the student data. The student groups do not show strong selection. I would say natural slection is in play with certain colors being picked as well as fitness on what colors end up not being picked.   
The evolutionary processes at work with the human version are natural selection, fitness, and variation of genes as certain toothpicks are picked. 
The evolutionary process at work with the computer simulation is natural selection.
The graph shows the strength of the processes are varied with some strong and some weak. 
The student numbers compare to the ciritcal value are more close to the natural procces of life than the simulated numbers that do not consider as many forces. 
The chi square values increase with mutation.
Extra Credit- 
data(simDraws)
simDraws<-fucntion(nruns, ncols=6, ndtart=20, nrounds=3, mu=0,twoway=TRUE, w=NULL) {
Chiout<-c()
for (j in 1:runs){
if (is.null(w))	{
Draws <- sample(Pop, 20, replace = T)
}
else if (!is.null(w))	{
if (length(setdiff(unique(Pop), names(w))) == 0)	{
Draws <- sample(Pop, 20, replace=T, prob=w[Pop])
}
else if (length(setdiff(unique(Pop), names(w))) != 0)	{
cat("fitness values", setdiff(unique(Pop), names(w)))
}
}
Pop <- sort(c(Draws,Draws,Draws))
}	
Summary<- c()
for (k in 1:ncols)	{
Summary[k] <- length(which(Pop == k))
}
Chiout[j] <- sum(((Summary - nstart)^2) / nstart)
}
return(Chiout)
}
Simulation8 <- simDraws(1e4, mu=2)
addHist(Y=8, Dat=Simulation8, Color=rgb(0,0,0,0.25))
Simulation9 <- simDraws(1e4, mu=300)
addHist(Y=8, Dat=Simulation9, Color=rgb(0,0,0,0.25))
Pop2 <- simPop(Popsize = 50, nGenerations = 100, initial_p = 0.5, h = 1, s = 0, mu=0, twoway = TRUE, w = NULL) 
plot(1:nrow(Pop2), Pop2[,1], ylim=c(0,1), type = 'l', xlab='generation', ylab='allele freq.', lwd=2 )
lines(1:nrow(Pop2), Pop2[, 2], lwd=2, col='red')
legend('topleft', legend = c('a', 'b'), col = c('black', 'red'), lwd = 2, bty='n')
Pop3 <- simPop(Popsize = 50, nGenerations = 100, initial_p = 0.5, h = 1, s = 0, mu=0.001, twoway = TRUE, w = NULL) 
plot(1:nrow(Pop3), Pop3[,1], ylim=c(0,1), type = 'l', xlab='generation', ylab='allele freq.', lwd=2 )
lines(1:nrow(Pop3), Pop3[, 2], lwd=2, col='red')
Mutation makes the chi-squared value go up and drift occur more. Selection increases drift also therefore increasing the chi-squared value. 