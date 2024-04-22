load("C:/Users/Mohsen/Documents/GitHub/Interpretable2/R/interpretable-ml-book-master/interpretable-ml-book-master/data/bike.RData")
bike.features.of.interest = c('season','holiday', 'workingday', 'weathersit', 'temp', 'hum', 'windspeed', 'days_since_2011')

X = bike[bike.features.of.interest]
y = bike[,'cnt']
dat = cbind(X, y)

mod = lm(y ~ ., data = dat, x = TRUE)
lm_summary = summary(mod)$coefficients

