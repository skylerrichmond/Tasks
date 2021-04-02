getwd()
setwd('C:\\Users\\Skyler Richmond\\Desktop\\Evolution\\Tasks\\Task_08')
library('phytools')
tree <-read.tree('https://jonsmithcell.com/data/anoliis.tre')
plot(tree, type="fan")
tree$tip.label
Question 1: There are 82 tips on the tree and branch lengths are present. 
data <- read.csv("https://jonsmitchell.com/data/svl.csv" , stringsAsFactors=F , row.names=1)
data
data[,1]
Question 2: Data is a list of species of lizards with their snout-vent lengths with 100 dimensions.  
svl <- setNames(data$svl, rownames(data))
svl
Ancestors <- fastAnc(tree, svl, vars=TRUE, CI=TRUE)
Ancestors
Question 3: The values are stored in the list called ace and the CI95 element is a confidence interval.
Question 4: The assumptions are that the contrast state is at the root when computed each time and that thr tree is rerooted at each internal node. 
par(mar=c(0.1,0.1,0.1,0.1))
plot(tree , type="fan", lwd=2, show.tip.label=F)
tiplabels(pch=16, cex=0.25*svl[tree$tip.label])
nodelabels(pch=16, cex=0.25*Ancestors$ace)
obj <- contMap(tree, svl, plot=F)
plot(obj, type="fan" , legend=0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7, 0.9))
fossilData <-data.frame(svl=log(c(25.4, 23.2, 17.7, 19.7, 24, 31)), tip1=c("Anolis_aliniger", "Anolis_aliniger", "Anolis_occultus"), tip2=c("Anolis_chlorocyanus", "Anolis_coelestinus", "Anolis_hendersoni", "Anolis_cybotes", "Anolis_angusticeps", "Anolis_angusticeps"))
fossilData
Question 5: Loop runs the action for each item in the list. 
fossilNodes <- c()
nodeN <- c()
{
	for(i in 1:nrow(fossilData))
	i<-1
	if(i==1) {
		print(Ancestors)
		}
		}
Node <- fastMRCA(tree, fossilData[i, "tip1"], fossilData[i, "tip2"])
Node
fossilNodes[i] <- fossilData[i, "svl"]
fossilNodes[i]
nodeN [i] <- Node
names(fossilNodes) <- nodeN
Ancestors_withFossils <- fastAnc(tree , svl , anc.states=fossilNodes, C1=TRUE, var=TRUE)
Ancestors_withFossils
Ancestors_woFossils<- fastAnc(tree, svl,CI=TRUE, var=TRUE)
Ancestors_woFossils
plot(Ancestors_withFossils$ace, Ancestors_woFossils$ace, xlab='fossils', ylab='no fossils')
Question 7: Fossils generally increase estimated ancestral sizes. 
Question 8-10: EB had the best fit and the function fastAnc makes its assumptions using BM making the assumptions correct. 
install.packages('geiger')
library('geiger')
?fitContinuous
fitContinuous(tree, svl, model='EB')
fitContinuous(tree, svl, model='OU')
fitContinuous(tree, svl, model='BM')
?fastAnc