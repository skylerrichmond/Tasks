setwd('C:\\Users\\Skyler Richmond\\Desktop\\Evolution\\Tasks\\Task_09\\')
library('phytools')
Question 1-3-
trees <- list()
births <- c()
Fractions <- c()
for(i in 1:100) {
	births[i] <- runif(1)
	Fractions[i] <- runif(1)
	trees[[i]] <- pbtree(b = births[i], d = (births[i] * Fractions[i]), n = 100,
nsim = 1)
}
trees
trees[[i]]
plot(trees[[i]])
install.packages('geiger')
library('geiger')
Question 4- There appears to be a positive correlation between diversification and the number of tips with a correlation value of 0.23. 
install.packages('TreeTools')
library('TreeTools')
tips <- sapply(trees, NTip)
logtips <- log(tips)
diversification <- sapply(trees, bd.ms)
plot(diversification, logtips, xlab='net diversification', ylab='log of total number of tips')
abline(lm(diversification~logtips), col='red')
tips
cor(diversification, logtips)
Question 5- There seems to be a direct inverse relationship between the branch length and speciation rate that were inversely proportional to one another. 
speciation <- sapply(trees, bd.km)
#for (t in 1:length(trees)) {
i <- 1
numtips <- c()
avgBL <- c()
for ( i in 1:length(trees)) {
# choose tree
y <- trees[[i]]
# find number of tips
numtips[i] <- Ntip(y)
# find average branch length
avgBL[i] <- mean(y$edge.length)
}
plot(speciation, avgBL, xlab='speciation rate', ylab='average branch length')
Question 6- The correlation value is -0.41.
cor(speciation, avgBL)
Question 7-
which.max(tips)
bigTree <- trees[[66]]
plot(bigTree)
rates <- c()
traits <- list()
for (i in 1:100) {
rates[i] <- runif(1)
traits[[i]] <- fastBM(tree = bigTree, sig2 = rates[i])
}
Question 8- The correlation value is -0.06. 
avgtrait <- sapply(traits, mean)
avgtrait
avgrate <- sapply(rates, mean)
avgrate
correlation <- cor(avgtrait, avgrate)
print(correlation)
plot(avgrate, avgtrait)
abline(lm(avgrate~avgtrait), col='purple')
Question 9- There appears to be a positive correlation between the variables with a correlation value of 0.84. 
vartraits<-sapply(traits, var)
cor(vartraits, rates)
Quesiton 10- This is not significant because the correlation value is close to 0 at 0.08.
trait1<- traits[1]
trait1
trait2<-traits[2]
trait2
traitmat<-cbind(traits[[1]], traits[[2]])
traitmat
var(traitmat)
cor(traitmat[,1], traitmat[,2])
plot(traitmat[,1], traitmat[,2])
abline(lm(traitmat[,1]~traitmat[,2]), col='blue')
plot(avgrate, avgtrait)
abline(lm(avgrate~avgtrait), col='orange')