setwd('C:\\Users\\Skyler Richmond\\Desktop\\Evolution\\Tasks\\Task_11')
x<- rnorm(100, mean=5, sd=2)
x
var(x)
mean(x)
y<-(x *5) + 2 + (rnorm(100, 0, 0.1))
y
pdf("1.pdf", height=4, width=4)
plot(x,y)
abline(lm(y~x), col='purple')
coef(lm(y~x))
dev.off()
The interecept is at 4.999  and the y interecept is 2.053 because numbers are rarely exact and are generally rounded and the little numbers added in threw these numbers off some.
z<-c()
x<- rnorm(100, mean=5, sd=2)
for(i in 1:100) {
 	z[i]<-runif(1)
 	y<- (x* z[i]) + 2 +(rnorm(100, 0:0.1))
 	l<-coef(lm(z[1:100]~y))
 }
pdf("2.pdf", height=4, width=4)
plot(z[1:100], y)
abline(lm(y~z[1:100]))
dev.off()
pdf("3.pdf", height=4, width=4)
plot(c(z, -0.03))
dev.off()
install.packages("meme")
library('meme')
dir()
u<-'https://imgur.com/t/frodo/XdKFw0t'
memelol<- meme(u, lower='Looking at coding I made a month ago', col='white', size='1.5')
dev.off()