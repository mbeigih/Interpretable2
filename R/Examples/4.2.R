library("ggplot2")
df = data.frame(x = c(1,2,3,8,9,10,11,9),
                y = c(0,0,0,1,1,1,1, 0),
                case = '0.5 threshold ok')
df_extra  = data.frame(x=c(df$x, 7, 7, 7, 20, 19, 5, 5, 4, 4.5),
                       y=c(df$y, 1,1,1,1, 1, 1, 1, 1, 1),
                       case = '0.5 threshold not ok')
df.lin.log = rbind(df, df_extra)
p1 = ggplot(df.lin.log, aes(x=x,y=y)) +
  geom_point(position = position_jitter(width=0, height=0.02)) +
  geom_smooth(method='lm', se=FALSE) +
  scale_y_continuous('', breaks = c(0, 0.5, 1), labels = c('benign tumor', '0.5',  'malignant tumor'), limits = c(-0.1,1.3)) +
  scale_x_continuous('Tumor size') +
  facet_grid(. ~ case) +
  geom_hline(yintercept=0.5, linetype = 3)
p1

logistic = function(x){1 / (1 + exp(-x))}
x = seq(from=-6, to = 6, length.out = 100)
df = data.frame(x = x,
                y = logistic(x))
ggplot(df) + geom_line(aes(x=x,y=y))

logistic1 = glm(y ~ x, family = binomial, data = df.lin.log[df.lin.log$case == '0.5 threshold ok',])
logistic2 = glm(y ~ x, family = binomial, data = df.lin.log)
lgrid = data.frame(x = seq(from=0, to=20, length.out=100))
lgrid$y1_pred = predict(logistic1, newdata = lgrid, type='response')
lgrid$y2_pred = predict(logistic2 , newdata = lgrid, type='response')
lgrid.m = data.frame(data.table::melt(lgrid, measure.vars = c("y1_pred", "y2_pred")))
colnames(lgrid.m) = c("x", "case", "value")
lgrid.m$case = as.character(lgrid.m$case)
lgrid.m$case[lgrid.m$case == "y1_pred"] = '0.5 threshold ok'
lgrid.m$case[lgrid.m$case == "y2_pred"] = '0.5 threshold ok as well'
df.lin.log$case = as.character(df.lin.log$case)
df.lin.log$case[df.lin.log$case == "0.5 threshold not ok"] = '0.5 threshold ok as well'
p1 = ggplot(df.lin.log, aes(x=x,y=y)) +
  geom_line(aes(x=x, y=value), data = lgrid.m, color='blue', size=1) +
  geom_point(position = position_jitter(width=0, height=0.02)) +
  scale_y_continuous('Tumor class', breaks = c(0, 0.5, 1), labels = c('benign tumor', '0.5',  'malignant tumor'), limits = c(-0.1,1.3)) +
  scale_x_continuous('Tumor size') +
  facet_grid(. ~ case) +
  geom_hline(yintercept=0.5, linetype = 3)
p1

