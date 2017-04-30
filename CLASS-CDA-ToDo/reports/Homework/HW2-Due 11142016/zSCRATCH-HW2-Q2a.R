# library(stats) ## "glm()" ##
lgm.a <- glm(abuse ~ boyfriend, data = dat, family = "binomial")
lgm.a
lgm.a2 <- glm(abuse ~ program, data = dat, family = "binomial")
lgm.a2
lgm.a3 <- glm(abuse ~ program + boyfriend, data = dat, family = "binomial")
lgm.a3

library(pander)
pander(anova(lgm.a, lgm.a2, lgm.a3))
anova(lgm.a, lgm.a2, lgm.a3)
lgm.p <- glm(program ~ boyfriend, data = dat, family = "binomial")
lgm.p
lgm.p2 <- glm(program ~ abuse, data = dat, family = "binomial")
lgm.p2
lgm.p3 <- glm(program ~ abuse + boyfriend, data = dat, family = "binomial")
lgm.p3

anova(lgm.p, lgm.p2, lgm.p3)

library(modEvA)
print(RsqGLM(lgm.a))
print(RsqGLM(lgm.p))
library(lmtest) ## "lrtest()" ##
lrchsq <- lrtest(lgm.a)
lrchsq


## Q2d

## The code below is based on an example from Bilder (2014)
## (see https://www.dropbox.com/s/r9wk5yy8mt9ey0j/GPA.R?dl=0) ##

## 1 row and 2 columns of plots ##
par(mfrow = c(1,2))
## Scatter plot of the data ##
plot(x = lgm.fit$abuse, y = lgm.fit$welfare, xlab = "abuse", ylab = "welfare", main = "welfare vs. abuse", panel.first = grid(col = "gray", lty = "dotted")); abline(a = abs(lgm$coefficients[1]), b = abs(lgm$coefficients[2]), lty = "solid", col = "blue", lwd = 2)

## Same scatter plot as before ##
plot(x = lgm.fit$abuse, y = lgm.fit$welfare, xlab = "abuse", ylab = "welfare", main = "welfare vs. abuse",
     panel.first = grid(col = "gray", lty = "dotted")); curve(expr = abs(lgm$coefficients[1]) + abs(lgm$coefficients[2])*x, xlim = c(min(lgm.fit$abuse),max(lgm.fit$abuse)),
                                                              col = "blue", add = TRUE, lwd = 2); segments(x0 = min(lgm.fit$abuse), y0 = abs(lgm$coefficients[1]) + abs(lgm$coefficients[2])*min(lgm.fit$abuse), x1 = max(lgm.fit$abuse), y1 = abs(lgm$coefficients[1]) + abs(lgm$coefficients[2])*max(lgm.fit$abuse), lty = 1, col = "blue", lwd = 2)


plot(x = lgm.fit$abuse, y = lgm.fit$welfare, xlab = "abuse", ylab = "welfare", main = "welfare vs. abuse",
     panel.first = grid(col = "gray", lty = "dotted")); curve(expr = abs(lgm$coefficients[1]) + abs(lgm$coefficients[2])*x, xlim = c(min(lgm.fit$abuse),max(lgm.fit$abuse)),
                                                              col = "blue", add = TRUE, lwd = 2); segments(x0 = min(lgm.fit$abuse), y0 = abs(lgm$coefficients[1]) + abs(lgm$coefficients[2])*min(lgm.fit$abuse), x1 = max(lgm.fit$abuse), y1 = abs(lgm$coefficients[1]) + abs(lgm$coefficients[2])*max(lgm.fit$abuse), lty = 1, col = "blue", lwd = 2)


regr <- function(x, y, data) {
    mod.fit <- lm(formula = y ~ x, data = data)
    plot(x = x, y = y, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), main = paste0(deparse(substitute(x)), "vs.", deparse(substitute(y))),
         panel.first = grid(col = "gray", lty = "dotted")); curve(expr = mod.fit$coefficients[1] + mod.fit$coefficients[2] *
                                                                      x, xlim = c(min(x), max(x)), col = "blue", add = TRUE,
                                                                  lwd = 2)
    # This is the object returned
    mod.fit
}

plot.new(); regrPlot(x = dat2.n$abuse, y = dat2.n$welfare, data = dat2.n)
