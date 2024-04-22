load("C:/Users/Mohsen/Documents/GitHub/Interpretable2/R/interpretable-ml-book-master/interpretable-ml-book-master/data/cervical.RData")


mod = glm(Biopsy ~ Hormonal.Contraceptives + Smokes + Num.of.pregnancies + STDs..Number.of.diagnosis + IUD,
          data = cervical, family = binomial())
# Print table of coef, exp(coef), std, p-value
coef.table = summary(mod)$coefficients[,c('Estimate', 'Std. Error')]
coef.table = cbind(coef.table, 'Odds ratio' = as.vector(exp(coef.table[, c('Estimate')])))
