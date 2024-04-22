?mean
help.search('weighted mean')
help(package = 'iml')
data(iris)
str(iris)
class(iris)
install.packages('iml')
library('iml')
tidyr::gather(X)
data("iris")
load("C:/Users/Mohsen/Documents/GitHub/Interpretable2/R/interpretable-ml-book-master/interpretable-ml-book-master/data/bike.RData")
getwd()
setwd("C://Users/Mohsen/Documents/GitHub/Interpretable2/R/interpretable-ml-book-master/interpretable-ml-book-master/manuscript")
c(2, 4, 6)
2:6
rep(1:2, times = 3)
rep(1:2, each = 3)
sort(bike$cnt)
table(bike$season)
rev(c(1, 2, 3))
unique(bike$season)
bike$season[1]
bike$season[-1]
bike$season[2:4]
bike$season[-(2:4)]
bike$season[c(1, 5)]
bike$season[bike$season == 'WINTER']
bike$temp[bike$temp > 20]
bike$holiday[bike$holiday %in% 'HOLIDAY']
bike['season']
cor(bike$temp, bike$cnt)
median(bike$temp)            
quantile(bike$temp)
rank(bike$season)
var(bike$cnt)
sd(bike$cnt)
ls()
rm(iris)
rm(list = ls())
m <- matrix(c(1:9), nrow = 3, ncol = 3)
m <- matrix(1:9, nrow = 3, ncol = 3)
m[2, ]
t(m)
m %*% m
solve(matrix((rep(1, times = 9)), nrow = 3, ncol = 3), c(6, 15, 24))
l <- list(x = 1:5, y = c('a', 'b'))
l[[2]]      
l[1]
l$x
l['y']
df <- data.frame(x = 1:3, y = c('a', 'b', 'c')) #A special case of a list where all elements are the same length.
df$x
df[[2]]
df[, 2]
View(df)
nrow(df)
ncol(df)
dim(df)
cbind() # Bind columns.
rbind() # Bind rows
factor(bike$season)
cut(bike$temp, breaks = 4) 
bike.features.of.interest = c('season','holiday', 'workingday', 'weathersit', 'temp', 'hum', 'windspeed', 'days_since_2011')
df <- bike[c(bike.features.of.interest)]
df <-  cbind(df, bike['cnt'])
linear_Model = summary(lm(cnt~., data = df ))$coefficients
glm_Model = summary(glm(cnt~., data = df ))$coefficients
t.test(bike$temp, bike$cnt)
t.test(c(1, 2, 3), c(1.1, 2, 3))
pairwise.t.test(c(1, 2, 3), c(1.1, 2, 3))
pairwise.t.test(c(1, 2, 3), c(10, 20, 30))
prop.test()
aov()
library(ggplot2)
plot(rnorm(10, 0, 1))
hist(rnorm(1000, 0, 1))
dnorm(0, 0, 1) #Density function PDF N(mu, sigma2) PDF=(1/(sigma*sqrt(2*pi))*exp((-1/2)*((z-mu)/pi)^2)

z = -5
for (i in 1:100){
  z[i] <-  -5 + 0.1 * i
  x[i]= dnorm(z[i],0,1)#x[i] <- (1/(sigma*sqrt(2*pi))) * exp((-1/2)*((z[i]-mu)/sigma)**2) #  x <-  (1/(sigma*sqrt(2*pi))*exp((-1/2)*((z-mu)/pi)^2)
}
plot(z,x)

pnorm(0, 0, 1) #CDF

CDF = 0
x = -5
for (i in 1:100){
  x[i] <-  -5 + 0.1 * i
  CDF[i] = pnorm(x[i],0,1)
}
plot(x, CDF)

qnorm(0.5, 0, 1) #Quantile

Q = 0
x = -5
for (i in 1:100){
  x[i] <-  0 + 0.01 * i
  Q[i] = qnorm(x[i], 0, 1)
}
plot(x, Q)


plot(rpois(10, 1))
hist(rpois(10000, 1))

PMF = 0
for (i in 0:5) {
  PMF[i + 1] = dpois(i, 1)
}
plot(PMF)
sum(PMF)

dpois(2, 1) #Density function PMF
ppois(1,1) #CDF

CMF = 0
for (i in 0:5) {
  CMF[i + 1] = ppois(i, 1)
}
plot(CMF)

qpois(0.99, 1) #Quantile

plot(rbinom(100, 10, 0.5))
hist(rbinom(1000, 10, 0.5))
dbinom(5, 10, 0.5)

pbinom(5, 10, 0.5)
qbinom(0.5, 10, 0.5)

plot(runif(100, 0, 1))
hist(runif(1000, 0, 1))
dunif(0, 0, 1) # a < x < b -> 1 / (b - a) otherwise 0
punif(8.1, 8, 10) # (x - a) / (b - a)
for (i in seq(0, 100, by = 1)) {
  x[i] <- punif(i/100, 0, 1)
}
plot(x)
qunif(0.05, 8, 10) # qunif(punif(8.1, 8, 10), 8, 10) = 8.1
