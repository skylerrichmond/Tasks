library(swirl)
swirl()
Skyler Richmond
1
5+7
x <- 5+7.
x
y <- x-3.
y
z <- c(1.1, 9, 3.14)
?c
z
c(z, 555, z)
z*2+1000
my_sqrt <- sqrt(z-1)
2
library(swirl)
swirl()
skip()
my_div <- z/my_sqrt
2
c(1, 2, 3, 4) + c(0, 10, 100)
my_div
skyler.richmond.sr@gmail.com
0
getwd()
ls()
x<- 9
dir()
list.files()
args(list.files)
old.dir<- getwd()
dir.create("testdir")
setwd("testdir")
file.create("mytest.R")
file.exists("mytest.R")
file.info("mytest.R")
file.rename("mytest.R""to""mytest2.R")
bye
swirl()
swirl()
file.rename("mytest.R","mytest2.R")
file.copy("mytest2.R","mytest3.R")
file.path("mytest3.R")
file.path('folder1','folder2')
?dir.create
dir.create("testdir2")
file.path("testdir3")
setwd(old.dir)
1
scr0013@mix.wvu.edu
Task 01 b
2
1
3
1:20
pi:10
15:1
?':'
seq(1, 20)
my_seq<- seq(5, 10, length=30)
my_seq<-
length(my_seq)
1:length(my_seq)
seq(along.with = my_seq)
seq_along(my_seq)
rep(0, times=40)
rep(c(0, 1,2), times=10)
rep(c(0, 1, 2), each=10)
1
2
swirl()
Skyler Richmond
1
5
x<- c(44, NA, 5, NA)
x*3
y<- rnorm(1000)

my_data<- sample(c(y,z), 100)
my_na<- is.na(my_data)
my_data == NA
sum(my_na)
my_data
0/0
Inf-Inf
1
1
6
x
x[1:10]
2
1
4
x[is.na(x)]
y<- x[!is.na(x)]
y
2
y[y>0]
x[x>0]
x[!is.na(x) & x>0]
x[c(3, 5, 7)]
x[0]
x[3000]
x[c(-2, -10)]
x[-c(2, 10)]
vect<- c(foo=11, bar=2, norf=NA)
vect
names(vect)
vect2<- c(11, 2, NA)
names(vect2)<- c("foo", "bar", "norf")
identical(vect,vect2)
3
3
vect["bar"]
vect[c("foo", "bar")]
1
1
7
my_vector<- 1:20
my_vector
dim(my_vector)
length(my_vector)
dim(my_vector)
dim(my_vector)<- c(4, 5)
attributes(my_vector)
my_vector
class(my_vector)
my_matrix<- my_vector
?matrix()
?matrix
my_matrix2<- matrix(<-dim(4, 5)<-c(1:20))
info()
my_matrix2<- matrix(dim(4, 5)<-c(1:20))
my_matrix2<- matrix(dim(4, 5)<-c(1:20))
my_matrix2<- matrix(dim(4X5))
bye()
swirl()
Skyler Richmond
1
my_matrix2<- matrix(1:20, nrow=4, ncol=5)
identical(my_matrix, my_matrix)
cbind(patients, my_matrix)
my_data<- data.frame(patients, my_matrix)
my_data
class(my_data)
cnames<- c("patient", "age", "weight", "bp", "rating","test")
colnames(my_data)<- cnames
my_data
1
1
8
TRUE==TRUE
(FALSE==TRUE)==FALSE
6==7
6<7
10<=10
1
!5==7
3
3
FALSE & FALSE
TRUE& c(TRUE, FALSE, FALSE)
TRUE && c(TRUE, FALSE, FALSE)
TRUE| c(TRUE, FALSE, FALSE)
TRUE|| c(TRUE, FALSE, FALSE)
5>8 || 6!= 8 && 4>3.9
2
4
3
isTRUE(6>4)
3
identical('twins', 'twins')
4
xor(5==6, !FALSE)
2

ints<- sample(10)
ints
ints>5
which(ints>7)
1
any(ints<0)
all(ints>0)
2
1
1
9
Sys.Date()
mean(c(2, 4, 5))
}
submit()
boring_function('My first function!')
boring_function
{sum(my_vector)/length(my_vector)}
submit()
my_mean(c(4, 5, 10))
by=2
%%
submit()
{num/ by=2}{%%}
1
{num/ by=2 %%}
?
info()
?remainder
?increment
(num, by=2) {num/by %%}
remainder <-(num/by=2) {num%%by}
submit()
remainder <-(num/by=2){num%%by}
remainder <-(num%%by=2) {num%%by}
remainder<-function(num,divisor=2) {num%%divisor}
submit()
remainder(5)
remainder(11,5)
remainder(divisor=11, num=5)
remainder(4, div=2)
args(remainder)
evaluate<- function(func,dat) {func,dat}
submit()
{dat,func}
evaluate(function(func,dat)) {func,dat}
evaluate <- function(func,dat) func, dat
evaluate <- function(func,dat) {func(dat)}
evaluate(standard deviation, c(1.4, 3.6, 7.9, 8.8))
evaluate(sd,c(1.4, 3.6, 7.9, 8.8))
evaluate(function(x){x+1}, 6)
evaluate(function(x),{8},c(8, 4, 0))
evaluate(function('x'){"8"})
evaluate(function(x){"8"}, "x")
evaluate(function(x){"8"}, 'x')
evaluate(function(x){c("8")}, c(8,4,0)
evaluate(function(x){"8"})
?vector
?index.vector
evaluate(function(x){c("8", "4", "0")})
swirl()
Skyler Richmond
1
evaluate(function(x){x[1]}, c("8", "4", "0"))
evaluate(function(x){x[1]}, c(8, 4, 0))
evaluate(function(x){x[3]}, c(8, 4, 0))
evaluate(function(x){x[length(c)]}, c(8, 4, 0))
evaluate(function(x){x[last 1]}, c(8, 4, 0))
evaluate(function(x){x[length(vec)]}, c(8, 4, 0))
evaluate(function(x){x[length(x)]}, c(8, 4, 0))
?paste
paste("Programming", "is", "fun!")
telegram <- function(...) {paste("START", "STOP"}
telegram <- function(...) {paste("START", ..., "STOP"}
telegram <- function(...) {paste(..., "START"}
telegram <- function(...) {paste("START", ..., "STOP", sep =" ")}
submit()
telegram(c("Hello", "everyone"))
mad_lib <- function (...){args <- list (...) place <- args [["place"]] adjective <- args[["adjective"]] noun <- args[["noun"]]
submit()
mad_libs(place= "Brazil", adjective= "pretty", noun= "girl")
"%p%" <- function(left, right){paste(left, right, sep=" ")}
"I" %p% "love" %p% "R!"
2
1
15
data(cars)
?cars
head(cars)
plot(cars)
?plot
plot(x=cars$speed, y=cars$dist)
plot(x=cars$dist, y=cars$speed)
plot(x=cars$speed, y=cars$dist, xlab= "Speed")
plot(x=cars$speed, y=cars$dist, ylab= "Stopping Distance")
plot(cars, main= "My Plot")
plot(cars, sub= "My Plot Subtitle")
plot(cars, col=2)
plot(cars, xlim=c(10, 15))
plot(cars, pch=2)
data(mtcars)
?boxplot
boxplot(formula=mpg~cyl, data=mtcars)
hist(mtcars$mpg)
2
0
