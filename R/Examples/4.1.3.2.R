load("C:/Users/Mohsen/Documents/GitHub/Interpretable2/R/interpretable-ml-book-master/interpretable-ml-book-master/data/bike.RData")
bike.features.of.interest = c('season','holiday', 'workingday', 'weathersit', 'temp', 'hum', 'windspeed', 'days_since_2011')

X = bike[bike.features.of.interest]
y = bike[,'cnt']
dat = cbind(X, y)

mod = lm(y ~ ., data = dat, x = TRUE)



X = data.frame(predict(mod, type = 'terms'))

X = tidyr::gather(X)
library("ggplot2")

ggplot(X) +
  geom_hline(yintercept=0, linetype=4) +
  geom_boxplot(aes(x=key, y=value, group=key)) +
  coord_flip() +
  scale_y_continuous('Feature effect') 

#effect_plot(mod, dat) + scale_x_discrete("")
