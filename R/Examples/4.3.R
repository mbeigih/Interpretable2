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

