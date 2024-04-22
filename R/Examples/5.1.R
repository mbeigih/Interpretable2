load("C:/Users/Mohsen/Documents/GitHub/Interpretable2/R/interpretable-ml-book-master/interpretable-ml-book-master/data/bike.RData")
library("mlr") #Machine Learning in R
## Loading required package: ParamHelpers
library("iml")
library("ggplot2")

bike.task = makeRegrTask(data = bike, target = "cnt")
mod.bike = mlr::train(mlr::makeLearner(cl = 'regr.randomForest', id = 'bike-rf'), bike.task)

pred.bike = Predictor$new(mod.bike, data = bike)
pdp = FeatureEffect$new(pred.bike, "temp", method = "pdp") 
p1 = pdp$plot() +  
  scale_x_continuous('Temperature', limits = c(0, NA)) + 
  scale_y_continuous('Predicted number of bikes', limits = c(0, 5500))
## Scale for 'y' is already present. Adding another scale for 'y', which
## will replace the existing scale.
pdp$set.feature("hum")
p2 = pdp$plot() + 
  scale_x_continuous('Humidity', limits = c(0, NA)) + 
  scale_y_continuous('', limits = c(0, 5500))
## Scale for 'y' is already present. Adding another scale for 'y', which
## will replace the existing scale.
pdp$set.feature("windspeed")
p3 = pdp$plot() + 
  scale_x_continuous('Wind speed', limits = c(0, NA)) + 
  scale_y_continuous('', limits = c(0, 5500))
## Scale for 'y' is already present. Adding another scale for 'y', which
## will replace the existing scale.
gridExtra::grid.arrange(p1, p2, p3, ncol = 3)
## Warning: Removed 3 rows containing missing values (geom_path).



pdp = FeatureEffect$new(pred.bike, "season", method = "pdp") 
pdp$results
ggplot(pdp$results) + 
  geom_col(aes(x = season, y = .y.hat), fill = "red", width = 0.3) + 
  scale_x_discrete('Season') + 
  scale_y_continuous('', limits = c(0, 5500))
