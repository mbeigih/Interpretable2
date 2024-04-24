load("C:/Users/Mohsen/Documents/GitHub/Interpretable2/R/interpretable-ml-book-master/interpretable-ml-book-master/data/bike.RData")
bike.features.of.interest = c('season','holiday', 'workingday', 'weathersit', 'temp', 'hum', 'windspeed', 'days_since_2011')
X = bike[bike.features.of.interest]
y = bike[,'cnt']
dat = cbind(X, y)
mod = lm(y ~ ., data = dat, x = TRUE)
lm_summary = summary(mod)$coefficients

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

feature_names=NULL
X = data.frame(predict(mod, type = 'terms'))
df = lapply(dat, function(feature){
  if(class(feature) == 'factor'){
    factor(levels(feature)[1], levels = levels(feature))
  } else {
    0
  }
})
# predict with type='terms' centers the results, so we have to add the mean again
reference_X = predict(mod, newdata=data.frame(df), type='terms')
X_star = data.frame(t(apply(X, 1, function(x){ x - reference_X[1,names(X)]})))
X=X_star
if(!missing(feature_names)){
  rownames(X) = feature_names
}
X = tidyr::gather(X)
require("ggplot2")
ggplot(X) +
  geom_hline(yintercept=0, linetype=4) +
  geom_boxplot(aes(x=key, y=value, group=key)) +
  coord_flip() +
  scale_y_continuous('Feature effect')

library("glmnet")
library(foreach)
X = bike[bike.features.of.interest]
X.d = model.matrix(y ~ . -1, data = X)
l.mod = glmnet(X.d, y)
plot(l.mod,  xvar = "lambda", ylab="Weights")

extract.glmnet.effects = function(betas, best.index) {
  data.frame(beta = betas[, best.index])
}  
n.features = apply(l.mod$beta, 2, function(x){sum(x!=0)})
library(kableExtra)
kable(extract.glmnet.effects(l.mod$beta, max(which(n.features == 2))), col.names = "Weight", digits = 2)
kable(extract.glmnet.effects(l.mod$beta, max(which(n.features == 5))), col.names = "Weight", digits = 2)
