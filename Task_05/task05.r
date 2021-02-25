install.packages("learnPopGen")
install.packages("coala")
install.packages("phytools")
library("learnPopGen")
library("coala")
library("phytools")
coalescent.plot()
?coalescent.plot
1. It begins with 10  alleles that cna be modified using the n= function from the coalescent.plot(). 
pdf('r.05-Question1.pdf', height=5, width=5)
2. An allele goes to fixation after 10 generations. 
pdf('r.05-Question2.pdf', height=5, width=5)
coalescent.plot(n=20, ngen=20, colors=NULL)
dev.off()
3. The average number of offspring was 2.
pdf('r.05-Question3.pdf', height=5, width=5)
coalescent.plot( n=10, ngen=1, colors=NULL)
coalescent.plot(n=10, ngen=5, colors=NULL)
dev.off()
4. Fitness plays a role becuase if the allele is in an unfit individual that cannot mate a lot it will go extinct. 
5. Yes. 
model<- coal_model(sample_size =5, loci_number=10, loci_length=500, ploidy =2) +
feat_mutation(10) +
feat_recombination(10) +
sumstat_trees() +
sumstat_nucleotide_div()
stats<-simulate(model,nsim=1)
Diversity<-stats$pi
The numbers are not all the same due to the change in alleles that comes with mutation, recombination, and crossover. 
Nloci<-length(stats$trees)
t1<- read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()
6. The number of tips differs because there are two alleles for each individual. 
Age1<- max(nodeHeights(t1))
t2<- read.tree(text=stats$trees[[2]][1])
plot(t2)
axisPhylo()
The most recent common ancestor is not the same as the 1st SNP. 
7. No. 
par(mfrow=c(1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()
compare.chronograms(t1,t2)
t1_1<- read.tree(text=stats$trees[[1]][1])
t1_2<-read.tree(text=stats$trees[[1]][2])
compare.chronograms(t1_1, t1_2)
for(locus in 1:Nloci {
	ntrees<-length(stats$trees[[locus]])
	for(n in 1:ntrees) {
		outPhy<- read.tree(text=stats$trees[[locus]][n])
	}
	else {
		outPhy<- ape:::c.phylo(outPhy, read.tree(text=stats$trees[[locus]][n]))
		}
	}
}
for (locus in 1:Nloci) {
ntrees <- length (stats$trees [[locus]])
for (n in 1:ntrees) {
if (locus == 1 && n == 1) {
outPhy <- read.tree (text=stats$trees [[locus]][n])
}
else {
outPhy <- ape:::c.phylo(outPhy, read.tree (text=stats$trees [[locus]][n]))
}
}
}
par (mfrow=c(1,1))
phytools::densityTree(outPhy)
par(mfrow=c(1,1))
densityTree(outPhy)
model <- coal_model(sample_size=5, loci_number=10, loci_length=500, ploidy=2) +
	feat_mutation(10) +
	feat_recombination(40) +
	sumstat_trees() +
	sumstat_nucleotide_div()
By changing the recombination frequency, I would expect more mutations and therefore a differnece in distribution which was in fact present. 
model3<- coal_model(10, 50) +
feat_mutation(par_prior("theta", sample.int(100, 1))) +
sumstat_nucleotide_div()
stats<- simulate(model3, nsim=40)
mean_pi<-sapply(stats, function(x) mean(x$pi))
theta<- sapply(stats, function(x) x$pars[["theta"]])
pdf('r05-Model3Graph.pdf', height=5, width=5)
plot(mean_pi)
plot(theta)
plot(mean_pi, theta, xlab='Diversity', ylab='Mutation Rate', pch=16, cex=1.3, col='black', main='Diversity Due to Varied Mutation Rate')
abline(lm(mean_pi ~ theta), col='red')
dev.off()
help(feat_selection)
install.packages('phyclust')
library('phyclust')
activate_ms(priority = 400)
activate_msms(jar = NULL, java = NULL, priority = 400, download = TRUE)
model4 <- coal_model(c(13, 18)) +
feat_selection(
	strength_AA = 0,
	strength_Aa = 0, 
	strength_aa = 1,
	strength_A = NULL,
	population = 1,
	time = '1',
	start = TRUE,
	start_frequency = 5e-04,
	Ne = 10000,
	position = 0.5,
	force_keep = TRUE,
	locus_group = 'all') +
feat_selection(
	strength_AA = 0,
	strength_Aa = 1, 
	strength_aa = 0,
	strength_A = NULL,
	population = 2,
	time = '1',
	start = TRUE,
	start_frequency = 5e-04,
	Ne = 10000,
	position = 0.5,
	force_keep = TRUE,
	locus_group = 'all') +
feat_size_change(0.5,
	population = 1, 
	time = '1', 
	locus_group = 'all') +
feat_size_change(0.1, 
	population = 2, 
	time = '2', 
	locus_group = 'all') +
feat_migration(1.2, 
	pop_to = 1, 
	pop_from = 2, 
	symmetric = FALSE, time = '3', locus_group = 'all') +
feat_mutation(array(c(2,5)))
sumstat_nucleotide_div() +
sumstat_trees() 
activate_ms(priority = 100)
activate_msms(priority = 500, download = TRUE)

model4 <- coal_model(c(13, 18), loci_number = 10, loci_length = 1000, ploidy = 2) +
	feat_selection(10, time = 1, population = 1) +
	feat_selection(25, time = 1, population = 2) +
	feat_size_change(0.5,
		population = 1, 
		time = '1', 
		locus_group = 'all') +
	feat_size_change(0.1, 
		population = 2, 
		time = '2', 
		locus_group = 'all') +
	feat_migration(1.2, 
		pop_to = 1, 
		pop_from = 2, 
		symmetric = FALSE, time = '3', locus_group = 'all') +
	feat_mutation(10) +
	sumstat_trees() +
	sumstat_nucleotide_div()
check_model(model4)
list_simulators()
activate_scrm(priority = 400)
stat2 <- simulate(model4) 
stat2 <- simulate(model4, nsim = 40)
Diversity2 <- stat2$pi
Diversity2 