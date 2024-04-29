library("partykit")
## Loading required package: grid
## Loading required package: libcoin
## Loading required package: mvtnorm
set.seed(42)
n = 100
dat_sim = data.frame(feature_x1 = rep(c(3,3,4,4), times = n), feature_x2 = rep(c(1,2,2,2), times = n), y = rep(c(1, 2, 3, 4), times = n))
dat_sim = dat_sim[sample(1:nrow(dat_sim), size = 0.9 * nrow(dat_sim)), ]
dat_sim$y = dat_sim$y + rnorm(nrow(dat_sim), sd = 0.2)
ct = ctree(y ~ feature_x1 + feature_x2, dat_sim)
plot(ct, inner_panel = node_inner(ct, pval = FALSE, id = FALSE), 
     terminal_panel = node_boxplot(ct, id = FALSE))

library(rpart)
load("C:/Users/Mohsen/Documents/GitHub/Interpretable2/R/interpretable-ml-book-master/interpretable-ml-book-master/data/bike.RData")
bike.features.of.interest = c('season','holiday', 'workingday', 'weathersit', 'temp', 'hum', 'windspeed', 'days_since_2011')
X = bike[bike.features.of.interest]
y = bike[,'cnt']
dat = cbind(X, y)
# increases readability of tree
library(partykit)
x = rpart(y ~ ., data = na.omit(dat), method = 'anova', control = rpart.control(cp = 0, maxdepth = 2))
xp = as.party(x)
plot(xp, digits = 0, id = FALSE, terminal_panel = node_boxplot(xp, id = FALSE),
     inner_panel = node_inner(xp, id = FALSE, pval = FALSE)
)

imp = round(100 * x$variable.importance / sum(x$variable.importance),0)
imp.df = data.frame(feature = names(imp), importance = imp)
imp.df$feature = factor(imp.df$feature, levels = as.character(imp.df$feature)[order(imp.df$importance)])
library(ggplot2)
ggplot(imp.df) + geom_point(aes(x = importance, y = feature)) + 
  scale_y_discrete("")
