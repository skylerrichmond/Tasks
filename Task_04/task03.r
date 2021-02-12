trueMean1<-5 
trueSD1<- 5
trueMean2<- 4
trueSD2 <-5
population2 <- rnorm(1e6, trueMean2, trueSD2)
Size <- 50
Sample1 <- sample(population1, Size)
Sample2<- sample(population2, Size)
head(Sample1)
head(Sample2)
population2
population1
The samples are different from one another which makes sense considering the populations are different from one another. 
boxplot(Sample1, Sample2)
source("http://jonsmitchell.com/code/simFxn04.R")
MatGrandma<- makeFounder("grandma_mom")
MatGrandpa <- makeFounder("grandpa_mom")
PatGrandma <- makeFounder("grandma_da")
PatGrandpa <- makeFounder("grandpa_da")
head(MatGrandpa)
head(MatGrandma)
head(PatGrandpa)
head(PatGrandma)
Alan<- makeBaby(PatGrandma, PatGrandpa)
head(Alan)
Brenda<- makeBaby(MatGrandma, MatGrandpa)
head(Brenda)
Focus<- makeBaby(Brenda, Alan)
50%
ToMom<- length( grep("mom", Focus) )/ length( Focus )
25% The numbers do not match my prediction. 
head(ToMom)
ToMomMom<- length( grep("grandma_mom", Focus) )/ length( Focus )
ToMomDad<- length( grep("grandpa_mom", Focus) )/ length( Focus )
head(ToMomMom)
head(ToMomDad)
ToDadMom<- length( grep("grandma_da", Focus) )/ length( Focus )
ToDadDad<- length( grep("grandpa_da", Focus) )/ length( Focus )
head(ToDadMom)
head(ToDadDad)
No, Focus is more related to the maternal grandfather than the maternal grandmother. Focus is also more related to the paternal grandmother. I expected that the genes would not be equal because genetically that basically never happens as some alleles are stronger than others and with crossing over and other genetic changes you never really know what you will get genetically. The average relatedness to all four grandparents was 0.34. 
Sibling_01<- makeBaby(Brenda, Alan)
I expect Focus to share around 80% of DNA with the sibling since they have the same parents but genetic mix ups always occur when pairing allels. The two actually share 94% of their DNA. 
ToSib<- length( intersect( Focus, Sibling_01) ) / length( Focus )
head(ToSib)
Focus shares around 23-60=5% of the DNA with all the siblings because yet again they have the same parents and gentic crossing will occur in the alles that come together. 
ManySiblings<- replicate( 1e3, length( intersect( Focus, makeBaby(Brenda, Alan)))/length( Focus ))
head(ManySiblings)
quantile(ManySiblings)
mean(ManySiblings)
The mean shows Focus shares 50.3% of genes with the siblings. 
plot(density(ManySiblings), main= "", xlab="proportion shared genes")
You see a range of vlues in analyses because with crossing over, pairing, and recombination of DNA there is never a set value but instead more of a range dependent on the factors that are present during reproduction.
HWE<- function(p) {
aa <-p^2
ab<-2 * p * (1-p)
bb<- (1-p)^2
return(c(aa=aa, ab=ab, bb=bb))
}
HWE(0.5)
plot(1,1,type="n", xlim=c(0,1), ylim=c(0,1), xlab="freq. allele a", ylab="geno. freq")
p <- seq(from=0, to =1, by =0.1)
GenoFreq<- t(sapply(p, HWE))
lines(p,GenoFreq[,"aa"], lwd=2, col="red")
I can read and understand this plot. if the frequency of allele a increases it follows the expectations we had, so the graphed rate will be as we predicted. The less the allele is present the less it fits the predictions we made into the graph. Time is shown on this plot as well as geographic space. 
lines(p,GenoFreq[,"ab"], lwd=2, col="purple")
lines(p,GenoFreq[,"bb"], lwd=2, col="blue")
legend("top", legend=c("aa", "ab", "bb"), col=c("red", "purple", "blue"), lty=1, lwd=2, bty="n")
Pop<- simPop(500)
points(Pop[, "freqa"], Pop[,"Genotypes.aa"]/500, pch=21, bg="red")
Yes, the points are on the line of expectation for Hardy-Weinberg.
Pop<-simPop(50)
points(Pop[, "freqa"], Pop[,"Genotypes.aa"]/50, pch=22, bg="red")
The points are more spread out but still remain on the expectation line but a smaller population has more room for variables. The frequency of a increased due to the small size of the opulation as well as a lesser chance of b. 
install.packages("learnPopGen")
library(learnPopGen)
learnPopGen()
x<-genetic.drift(Ne=200, nrep=5, pause=0.01)
x<-genetic.drift(Ne=20, nrep=5, pause=0.01)
PopSizes<-5:50
Samples<- rep(PopSizes, 5)
tExt<- sapply(Samples, function(x) nrow(simPop(x,500)))
Line<- lm(tExt~ Samples)
summary(Line)
Line$coef
plot(Samples, tExt)
abline(Line)
Line2<- lm(tExt ~ Samples +0)
summary(Line2)
When the poopulation increases the points tray further and further from the line we created meaning that allele is more present and not as likely to be edited out due to natural selection via extinction. 
Extra Credit-
install.packages("robustbase")
install.packages("tidyverse")
install.packages("sandwich")
install.packages("lmtest")
install.packages("modelr")
install.packages("broom")
bptest(Line)
bptest(Line2)
coeftest(Line, vcov= vcovHC(Line))
Line3<-lmrob(tExt~Samples)
summary(Line3)
plot(Samples, tExt)
abline(Line)
abLine(Line3)
pdf("r03exc-RobustHeteroskedasticity.pdf", height-5, width=5)
plot(Samples, tExt)
abLine(Line, col="blue")
abLine(Line3, col="purple")
legend(x=20, y=400, legend=c('Robust', 'Linear'), col=c('blue', 'purple'), lwd=1.2, cex=0.8)
dev.off()
The slope is lower for the robust line than the one we had before. 