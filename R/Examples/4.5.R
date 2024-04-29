value = factor(c("high", "high", "high", "medium", "medium", "medium", "medium", "low", "low", "low"), levels = c("low", "medium", "high"))
df = data.frame(
  location = c("good", "good", "good", "bad", "good", "good", "bad", "bad", "bad", "bad"),
  size = c("small", "big", "big", "medium", "medium", "small", "medium", "small", "medium", "small"), 
  pets = c("yes", "no", "no", "no", "only cats", "only cats", "yes", "yes", "yes", "no"),
  value = value
)
value.f = factor(paste("value=", value, sep = ""), levels = c("value=low", "value=medium", "value=high"))
library(kableExtra)
kable(df)

kable(table(paste0("location=", df[,"location"]), value.f))

kable(table(paste0("size=", df[,"size"]), value.f))

kable(table(paste0("pets=", df[,"pets"]), value.f))

library("OneR")
load("C:/Users/Mohsen/Documents/GitHub/Interpretable2/R/interpretable-ml-book-master/interpretable-ml-book-master/data/cervical.RData")
rule = OneR::OneR(Biopsy ~ ., data = cervical)
## Warning in OneR.data.frame(x = data, ties.method = ties.method, verbose =
## verbose, : data contains unused factor levels
rule.to.table = function(rule){
  dt = data.frame(x = names(rule$rules), prediction = unlist(rule$rules))
  colnames(dt) = c(rule$feature, "prediction")
  dt
}
library(kableExtra)
kable(rule.to.table(rule), row.names = FALSE)

tt = table(paste0("Age=", bin(cervical$Age)), cervical$Biopsy)
tt = data.frame(matrix(tt, ncol = 2), row.names = rownames(tt))
tt$p.cancer = round(tt[,1]/(tt[,1] + tt[,2]), 2)
kable(tt, col.names = c("# Cancer", "# Healthy", "P(Cancer)"))

load("C:/Users/Mohsen/Documents/GitHub/Interpretable2/R/interpretable-ml-book-master/interpretable-ml-book-master/data/bike.RData")
bike2 <- bike
bike2$days_since_2011 = max(0, bike2$days_since_2011)
bike2$cnt =  cut(bike2$cnt, breaks = quantile(bike$cnt), dig.lab = 10, include.lowest = TRUE)
rule = OneR::OneR(cnt ~ ., data = bike2)
kable(rule.to.table(rule), row.names = FALSE)

library(dplyr)
## 
## Attaching package: 'dplyr'
## The following object is masked from 'package:nlme':
## 
##     collapse
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
set.seed(42)
n = 100
dat = data.frame(x1 = rnorm(n), x2 = rnorm(n))
dat$class = rbinom(n = 100, size = 1, p = exp(dat$x1 + dat$x2) / (1 + exp(dat$x1 + dat$x2)))
dat$class = factor(dat$class)

min.x1 = min(dat$x1)
min.x2 = min(dat$x2)
library(ggplot2)
p1 = ggplot(dat) + geom_point(aes(x = x1, y = x2, color = class, shape = class))+ 
  viridis::scale_color_viridis(guide = "none", discrete = TRUE, option = "D", end = 0.9) +
  scale_shape_discrete(guide = "none") +
  ggtitle("Data")
p2 = ggplot(dat) + 
  geom_rect(xmin = -3, xmax = 0,   ymin = -2, ymax = -0.5, color = "black", fill = NA) + 
  geom_point(aes(x = x1, y = x2, color = class, shape = class)) + 
  viridis::scale_color_viridis(guide = "none", discrete = TRUE, option = "D", end = 0.9) +
  scale_shape_discrete(guide = "none") +
  ggtitle("Step 1: Find rule")

# dat.reduced = filter(dat, !(x1 <= 0 & x2 <= -0.5))

dat.reduced <- 
  dat %>% 
  filter(!(x1 <= 0 & x2 <= -0.5)) %>% 
  print()
p3 = ggplot(dat.reduced) + 
  geom_rect(xmin = -3, xmax = 0,   ymin = -2, ymax = -0.5, color = "black", fill = NA)  + 
  geom_point(aes(x = x1, y = x2, color = class, shape = class)) + 
  scale_x_continuous(limits = c(min.x1, NA)) + 
  scale_y_continuous(limits = c(min.x2, NA)) + 
  viridis::scale_color_viridis(guide = "none", discrete = TRUE, option = "D", end = 0.9) +
  scale_shape_discrete(guide = "none") + 
  ggtitle("Step 2: Remove covered instances")


p4 = p3 + 
  geom_rect(xmin = 0.8, xmax = 2.5, ymin = -1.5, ymax = 1.5, color = "black", fill = NA)  + 
  ggtitle("Step 3: Find next rule")

gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
setwd("C:/Users/Mohsen/Documents/GitHub/Interpretable2/R/interpretable-ml-book-master/interpretable-ml-book-master/manuscript")
knitr::include_graphics("images/learn-one-rule.png")

bike2 = bike
bike2$cnt = round(bike2$cnt)
bike2$cnt =  cut(bike$cnt, breaks = quantile(bike$cnt), dig.lab = 10, include.lowest = TRUE)
bike2$temp = round(bike2$temp)
bike2$windspeed = round(bike2$windspeed)
bike2$hum = round(bike2$hum)
library(RWeka)
rule = JRip(cnt  ~ ., data = bike2)
rule$classifier
extract.rules.jrip(rule)

library("sbrl")
library("arules")
## 
## Attaching package: 'arules'
## The following object is masked from 'package:dplyr':
## 
##     recode
## The following objects are masked from 'package:base':
## 
##     abbreviate, write
data("cervical")

cervical2 = as.data.frame(lapply(cervical, function(x) {
  if(is.factor(x) || length(unique(x)) < 3) {
    as.factor(x)
  } else {
    discretize(x, method = "interval", 3)
    #discretize(x, breaks = max(length(unique(x))-1, 5))
  }
}))

get.sbrl.rules = function(x) {
  res = lapply(1:nrow(x$rs), function(i) {
    if (i == 1) 
      sprintf("If      %s (rule[%d]) then positive probability = %.8f\n", 
              x$rulenames[x$rs$V1[i]], x$rs$V1[i], x$rs$V2[i])
    else if (i == nrow(x$rs)) 
      sprintf("else  (default rule)  then positive probability = %.8f\n", 
              x$rs$V2[nrow(x$rs)])
    else sprintf("else if %s (rule[%d]) then positive probability = %.8f\n", 
                 x$rulenames[x$rs$V1[i]], x$rs$V1[i], x$rs$V2[i])
  })
  data.frame(rules = unlist(res))
}


cervical2$label = cervical2$Biopsy
cervical2$Biopsy = NULL
rules = sbrl(cervical2, pos_sign = "Cancer", neg_sign = "Healthy", rule_maxlen = 2)
kable(get.sbrl.rules(rules))

set.seed(1)
conditions = sample(rules$rulenames, size = 10)
kable(gsub("\\{|\\}", "", conditions), col.names = "pre-mined conditions")

library("sbrl")
library("arules")
#data("bike")
load("C:/Users/Mohsen/Documents/GitHub/Interpretable2/R/interpretable-ml-book-master/interpretable-ml-book-master/data/bike.RData")
bike2 = bike
#bike2 = bike
bike2$label = bike2$cnt > 4000
bike2$cnt = NULL
bike2 = as.data.frame(lapply(bike2, function(x) {
  if(is.factor(x) || length(unique(x)) < 3) {
    as.factor(x)
  } else {
    discretize(x, method = "interval", 3)
    #discretize(x, breaks = max(length(unique(x))-1, 5))
  }
}))
rules = sbrl(bike2, pos_sign = TRUE, neg_sign = FALSE, rule_maxlen = 3)
kable(get.sbrl.rules(rules))
