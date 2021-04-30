setwd('C:\\Users\\Skyler Richmond\\Desktop\\Evolution\\Tasks\\Task_10')
install.packages("diversitree")
library("diversitree")
transition_0to1<- 0.1
transition_1to0<-0.1
speciation_0<- 0.2
excitntion_0<-0.1
speciation_1<-0.2
excitntion_1<-0.1
maxN<-1e3
maxT<- 50
Pars<-c(speciation_0, speciation_1, excitntion_0, excitntion_1, transition_0to1, transition_1to0)
simTree<- tree.bisse(Pars, max.taxa = maxN, max.t= maxT)
str(simTree)
?tree.bisse
stateTable<-table(simTree$tip.state)
stateTable / sum(stateTable)
Frequencies <-c('State0', 'State1')
Colors<-c('red','blue')
Data<- matrix(c(0.3, 0.32, 0.33, 0.35, 0.43, 0.56, 0.568, 0.57, 0.642, 0.647, 0.68, 0.69), nrow=2, ncol=6, byrow=TRUE )
Question 1: For state 1 to be high with a lower diversification, the speciation rate would be lower and an inverse relationship between the frequency of state 1 and the net diversification would have to be present. 
Difference <-c(0.01, 0.02, 0.03, 0.05, 0.1, 0.15)
Frequencyof1<- c(0.3, 0.32, 0.33, 0.35, 0.43, 0.568)
Frequencyof0<-c(0.43, 0.57, 0.642, 0.647, 0.68, 0.69)
pdf('Question1.pdf', height=6, width=6)
barplot(Data, names.arg=Difference, main='Change in Frequency of the States based on Variation in the R Values', xlab='Difference in Rate of Diversification', ylab= 'Frequency', beside=TRUE, col=c('orange', 'blue'))
legend('top', Frequencies, fill= 'orange', 'blue')
dev.off()
Question 2: The state 1 frequecy was never zero but close when the net diversification rate of state 0 was increased. 
Frequencies <-c('State0', 'State1')
Data<- matrix(c(0.015, 0.016, 0.022, 0.023, 0.037, 0.045, 0.057, 0.078, 0.1, 0.14, 0.18, 0.2, 0.23, 0.26, 0.34, 0.37, 0.4, 0.43, 0.46, 0.52, 0.55, 0.58, 0.6, 0.63, 0.66, 0.7, 0.74,  0.77, 0.8, 0.83, 0.85, 0.92, 0.95, 0.97))
Data
Difference<-c(0.05, 0.06, 0, 0, 0, 0.1, 0.1, 0.1, 0.2, 0.2, 0.3, 0.33, 0.43, 0.33, 0.33, 0.45, 0.45)
pdf('Question2.pdf', height=8, width=8)
barplot(Data, names.arg=Difference, main='Close to Zero State 1 for Transition Rate of Nonzero', xlab='Difference in Diversification Rate', ylab='Frequencies', col=c('pink','purple'))
legend('topright', Frequencies, fill='pink', 'purple')
dev.off()
Question 3: There was a some but not much variance in frequency as demonstrated by the trials I ran. 
Data
head(Data)
Frequency1Trial1<-Data[,2]
Freqeuncy1Trial2<-Data[,5]
Frequency1Trial3<--Data[,8]
Variance1<-var(Frequency1Trial1)
Variance2<-var(Freqeuncy1Trial2)
Variance3<-var(Frequency1Trial3)
Variance1
Variance2
Variance3
TotalVarianceMatrix<-c(Variance1, Variance2, Variance3)
TotalVarianceMatrix
Trial<-c(1, 2, 3)
pdf('Question3.pdf', height=8, width=8)
barplot(TotalVarianceMatrix, names.arg=Trial, ylim=c(0, 0.5), xlab='Trial Number', ylab='Variance in Differnt Frequencies', col='yellow')
dev.off()
Question 4:
Data
Frequencyof0<-Data[,2]
Frequencyof0
NetDivR0<-Data[,1]
pdf("Trend1.pdf", height=8, width=8)
plot(NetDivR0, Frequencyof0, main='Change in Frequency of State 0 Based on Net Diversification Rate', xlab='Net Diversification Rate of State 0', ylab='Frequency of State 0')
abline(lm(Frequencyof0~NetDivR0), col='green', lty='dashed')
dev.off()
Frequencyof1<-Data[,7]
NetDivR1<-Data[,5]
pdf("Trend2.pdf", height=8, width=8)
plot(NetDivR1, Frequencyof1, main='Change in Frequency of State 1 Based on Net Diversification Rate', xlab='Net Diversification Rate of State 1', ylab='Frequency of State 1')
abline(lm(Frequencyof1~NetDivR1), col='yellow', lty='dashed')
dev.off()