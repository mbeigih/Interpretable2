library("OneR")
load("C:/Users/Mohsen/Documents/GitHub/Interpretable2/R/interpretable-ml-book-master/interpretable-ml-book-master/data/cervical.RData")
rule = OneR::OneR(Biopsy ~ ., data = cervical)
rule$rules
tt = table(paste0("Age=", bin(cervical$Age)), cervical$Biopsy)
tt = data.frame(matrix(tt, ncol = 2), row.names = rownames(tt))
tt$p.cancer = round(tt[,1]/(tt[,1] + tt[,2]), 2)
library(kableExtra)
kable(tt, col.names = c("# Cancer", "# Healthy", "P(Cancer)"))
