load("C:/Users/Mohsen/Documents/GitHub/Interpretable2/R/interpretable-ml-book-master/interpretable-ml-book-master/data/bike.RData")
bike.features.of.interest = c('season','holiday', 'workingday', 'weathersit', 'temp', 'hum', 'windspeed', 'days_since_2011')

X = bike[bike.features.of.interest]
y = bike[,'cnt']
dat = cbind(X, y)

mod = lm(y ~ ., data = dat)#, x = TRUE)

library('ggplot2')
alpha = 0.05
lm_summary = summary(mod)$coefficients
df = data.frame(Features = rownames(lm_summary),
                Estimate = lm_summary[,'Estimate'],
                std_error = lm_summary[,'Std. Error'])
df$lower = df$Estimate - qnorm(alpha/2) * df$std_error
df$upper = df$Estimate + qnorm(alpha/2) * df$std_error
ggplot(df) +
  geom_vline(xintercept=0, linetype=4) +
  geom_point(aes(x=Estimate, y=Features)) +
  geom_segment(aes(y=Features, yend=Features, x=lower, xend=upper), arrow = arrow(angle=90, ends='both', length = unit(0.1, 'cm')))+
  scale_x_continuous('Weight estimate')+
  scale_y_discrete("")

