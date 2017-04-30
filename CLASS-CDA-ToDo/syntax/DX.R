#' ---
#' title: "Homework 3 - Question 1: Early Head Start Program"
#' author: "Riley Smith"
#' date: "`r format(Sys.Date(), '%d %b %Y')`"
#' ---

#+ setup, echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE
source("../SETUP.R")
dat <- read.spss("data/child2.sav", use.value.labels = FALSE, to.data.frame = TRUE)
dat <- na.omit(dat)
id <- seq(1:nrow(dat)); 
dat <- cbind(id, dat)
lgm <- glm(abuse ~ program + boyfriend + white + welfare,
           data = dat,
           family = binomial(link = "logit"))
# plot(lgm, which = 1)
lgm.dx.o <- LogisticDx::dx(lgm, byCov = FALSE)
lgm.dx <- LogisticDx::dx(lgm, byCov = TRUE)
# dx.id <- cbind(dat$id, lgm.dx, dat$id)
# names(dx.id) <- c("id", names(lgm.dx), "id")
# setequal(lgm$fitted.values, lgm.dx$yhat)
# lgm.yhat <- lgm$fitted.values
# lgm.yhat <- data.frame(attributes(lgm.yhat), lgm.yhat)
# names(lgm.yhat) <- c("id", "lgm.yhat")
# lgm.dxx <- merge(lgm.dx, lgm.yhat, by.x = "yhat", by.y = "lgm.yhat")

## Subsetting to just the columns we need for the assisngment (standardized (pearson) residuals ("sPr"), predicted probabilities ("P"), delta-x2 ("dChisq"), delta-deviance ("dDev"), & delta-beta ("dBhat")):

lgm.dx <- LogisticDx::dx(lgm, byCov = TRUE)

x <- seq(1:30)
lgm.dx <- cbind(x, lgm.dx)
dx1 <- lgm.dx[order(lgm.dx$n), c("x", "n", "welfare", "sPr","P", "dChisq", "dDev", "dBhat")] %>% round(5)
kable(dx1)
write.csv(dx1, "data/hw3-q1b.csv", row.names = FALSE)

lgm.dx2 <- LogisticDx::dx(lgm, byCov = FALSE) 
x2 <- seq(1:nrow(dat))
lgm.dx2 <- cbind(x2, lgm.dx2)
dx2 <- lgm.dx2[, c("x2", "n", "welfare", "sPr","P", "dChisq", "dDev", "dBhat")] %>% round(5)
write.csv(dx2, "data/hw3-q1b2.csv", row.names = FALSE)

dx1.x2 <- dx1[order(dx1$dChisq), c("dChisq", "dDev", "dBhat", )]
	## Note that you need to load the "dplyr" package for the "%>%" operator above ##
dx.id1 <- as.data.frame(dx.id1)
## Subsetting, again, to grab just the cases flagged in the (base graphics) residual plots as outliers:
dx.id2 <- dx.id1[c(324, 592, 1046), ]
dx.id3 <- dx.id1[c(-324, -592, -1046)]
kable(dx.id) 


plot(dx.id1$P, dx.id1$dChisq)
plot(dx.id1$P, dx.id1$dBhat)
plot(dx.id1$P, dx.id1$dD)
plot(dx.id1$P, dx.id1$sPr)

dx.id1[dx.id1$P > 0.85 & dx.id1$sPr < -10, "id"]
dx.id1[dx.id1$P > 0.8 & dx.id1$sPr < -10, "id"]
R.msmm(as.data.frame(dx.id1))
R.msmm(as.data.frame(dx.id3))
R.msmm(as.data.frame(dx.id2))

# LogisticDx:::plot.glm(lgm, palette = "Accent", alpha = 0.6, cex.main = 1.25, devNew = FALSE)
# detach(package:LogisticDx, unload = TRUE)
# '
# '
# + lgm_resid2
# graphics::plot(lgm, which = 3)
# plot.default(lgm, which = 3)

# h: 29

dat.w5 <- dat[dat$welfare >= 5, ]
dat.w <- rbind(dat.w5, dat[592, ])
dat.w
# 592: 0       0           0         0         1       0  
# plot(lgm, identify = TRUE, )

influencePlot(lgm)
outlierTest(lgm)