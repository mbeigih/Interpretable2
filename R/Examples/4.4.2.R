library(rpart)
load("C:/Users/Mohsen/Documents/GitHub/Interpretable2/R/interpretable-ml-book-master/interpretable-ml-book-master/data/bike.RData")
bike.features.of.interest = c('season','holiday', 'workingday', 'weathersit', 'temp', 'hum', 'windspeed', 'days_since_2011')

X = bike[bike.features.of.interest]
y = bike[,'cnt']
dat = cbind(X, y)
# increases readability of tree
x = rpart(y ~ ., data = na.omit(dat), method = 'anova', control = rpart.control(cp = 0, maxdepth = 2))
library(partykit)
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
