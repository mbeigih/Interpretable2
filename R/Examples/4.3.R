theme_blank = theme(axis.line=element_blank(),axis.text.x=element_blank(),
                    axis.text.y=element_blank(),axis.ticks=element_blank(),
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank(),legend.position="none",
                    panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                    panel.grid.minor=element_blank(),plot.background=element_blank())
## For the GLM
n = 10000
df = data.frame(x = c(rnorm(n), rexp(n, rate = 0.5)), dist = rep(c("Gaussian", "Definitely Not Gaussian"), each = n))
df$dist  = relevel(factor(df$dist), "Gaussian")
p.glm = ggplot(df) + geom_density(aes(x = x)) + facet_grid(. ~ dist, scales = "free") + theme_blank
# For the interaction
df = data.frame(x1 = seq(from = -3, to = 3, length.out = n), x2 = sample(c(1,2), size = n, replace = TRUE))
df$y = 3 + 5 * df$x1 + (2  - 8 * df$x1 ) * (df$x2 == 2)
df$interaction = "Interaction"
df2 = df
df2$y = 3  + 5 * df$x1 + 0.5 * (- 8 * df$x1 ) + 2 * (df$x2 == 2)
df2$interaction = "No Interaction"
df = rbind(df, df2)
df$interaction  = relevel(factor(df$interaction), "No Interaction")
df$x2 = factor(df$x2)
p.interaction = ggplot(df) + geom_line(aes(x = x1, y = y, group = x2, lty = x2)) + facet_grid(. ~ interaction) + theme_blank
# For the gam
df = data.frame(x  = seq(from = 0, to = 10, length.out = 200))
df$y = 5 + 2 * df$x
df$type = "Linear"
df2 = df
df2$y = 3 + 2 * df$x + 3 * sin(df$x)
df2$type = "Nonlinear"
df = rbind(df, df2)
p.gam = ggplot(df) + geom_line(aes(x = x, y = y)) + facet_grid(. ~ type) + theme_blank
gridExtra::grid.arrange(p.glm, p.interaction, p.gam)

# simulate data where the normal linear model fails.
n = 200
df = data.frame(stress  = runif(n = n, min = 1, max = 10), 
                sleep = runif(n = n, min = 1, max = 10), 
                work = sample(c("YES", "NO"), size = n, replace = TRUE))
lambda = exp(1* df$stress/10 - 2 * (df$sleep - 5)/10  - 1 * (df$work == "NO"))
df$y = rpois(lambda = lambda, n = n)
tab = data.frame(table(df$y))
ggplot(tab) + 
  geom_col(aes(x = Var1, y = Freq), width = 0.3) +
  scale_x_discrete("Number of coffees on a given day") + 
  scale_y_continuous("Number of days")

x = data.frame(work = c("Y", "N", "N", "Y"), temp = c(25, 12, 30, 5))
knitr::kable(x)

mod = lm(1:4 ~ ., data = x)
model.tab = data.frame(model.matrix(mod))
colnames(model.tab)[1] = "Intercept"
knitr::kable(model.tab)

mod = lm(1:4 ~ work * temp, data = x)
model.tab = data.frame(model.matrix(mod))
colnames(model.tab)[1] = "Intercept"
knitr::kable(model.tab)

x = data.frame(work = c("Y", "N", "N", "Y"), wthr = c("2", "0", "1", "2"))
knitr::kable(x)

mod = lm(1:4 ~ work * wthr, data = x)
model.tab = data.frame(model.matrix(mod))
colnames(model.tab)[1] = c("Intercept")
knitr::kable(model.tab)

load("C:/Users/Mohsen/Documents/GitHub/Interpretable2/R/interpretable-ml-book-master/interpretable-ml-book-master/data/bike.RData")
bike.features.of.interest = c('season','holiday', 'workingday', 'weathersit', 'temp', 'hum', 'windspeed', 'days_since_2011')
X = bike[bike.features.of.interest]
y = bike[,'cnt']
dat = cbind(X, y)
mod = lm(y ~ . + temp * workingday, data = dat, x = TRUE)
lm_summary = summary(mod)$coefficients
lm_summary_print = lm_summary
rownames(lm_summary_print) = pretty_rownames(rownames(lm_summary_print))
# var name becomes to long otherwise
rownames(lm_summary_print)[rownames(lm_summary_print) == "weathersitRAIN/SNOW/STORM"] = "weathersitRAIN/..."
library(kableExtra)
kable(cbind(lm_summary_print[,c('Estimate', 'Std. Error')], confint(mod)), digits = 1, col.names = c('Weight', 'Std. Error', "2.5%","97.5%"))

interactions::interact_plot(mod, pred = "temp", modx = "workingday")

mod.simpel = lm(cnt ~ temp, data = bike)
bike.plt = bike
bike.plt$pred.lm = predict(mod.simpel)
bike.plt$log.temp = log(bike$temp + 10)
mod.simpel = lm(cnt ~ log.temp, data = bike.plt)
bike.plt$pred.sqrt = predict(mod.simpel)
bike.plt$cat.temp = cut(bike$temp, breaks = seq(from = min(bike$temp), to = max(bike$temp), length.out = 10), include.lowest = TRUE)
mod.simpel = lm(cnt ~ cat.temp, data = bike.plt)
bike.plt$pred.cat = predict(mod.simpel)
library(mgcv)
## Loading required package: nlme
## This is mgcv 1.8-28. For overview type 'help("mgcv-package")'.
mod.gam = gam(cnt ~ s(temp), data = bike)
bike.plt$pred.gam = predict(mod.gam)
bike.plt = data.table::melt(bike.plt[c("pred.lm", "pred.sqrt", "pred.cat", "pred.gam")])
## No id variables; using all as measure variables
## Warning: attributes are not identical across measure variables; they will
## be dropped
bike.plt$temp = rep(bike$temp, times = 4)
bike.plt$cnt = rep(bike$cnt, times = 4)
model.type = c(pred.lm = "Linear model", 
               pred.sqrt = "Linear model with log(temp + 10)", 
               pred.cat = "Linear model with categorized temp", 
               pred.gam = "GAM")
library(ggplot2)
ggplot(bike.plt) + 
  geom_point(aes(x = temp, y = cnt), size = 1 , alpha = 0.3)  + 
  geom_line(aes(x = temp, y = value), size = 1.2, color = "blue") + 
  facet_wrap("variable", labeller = labeller(variable = model.type)) + 
  scale_x_continuous("Temperature (temp)") + 
  scale_y_continuous("(Predicted) Number of rented bikes")

# fit GAM again with less splines
mod.gam = gam(cnt ~ s(temp, k = 5), data = bike)
kable(head(model.matrix(mod.gam)), digits = 2)

mm = model.matrix(mod.gam)
mm2 = data.table::melt(mm)
mm2 = mm2[mm2$Var2 != "(Intercept)",]
ggplot(mm2) + geom_line(aes(x = rep(bike$temp, times = 4), y = value)) + facet_wrap("Var2") + 
  scale_x_continuous("Temperature") + 
  scale_y_continuous("Value of spline feature")
