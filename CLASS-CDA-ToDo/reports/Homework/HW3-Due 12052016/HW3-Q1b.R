#' ---
#' title: "Homework 3, Question 1b"
#' author: "Rachel M. Smith"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#'
#+ setup, echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE
source("../SETUP.R") ## still need all of my setup tools though ##
knitr::opts_chunk$set(tidy = FALSE, echo = TRUE, cache = FALSE, results = 'asis', Rplot = NULL, dev = 'pdf', fig.path = "graphics/HW3/q1b-", fig.width = 7, fig.height = 7, out.width = "\\linewidth")
#'
#' -----
#'
#' # Data Descriptives
#'
#+ dat1, echo=TRUE
dat <- R.rspss("data/child2.sav", vlabs = FALSE)
dat <- na.omit(dat)
R.msmm(dat[, -5])
#'
#' -----
#'
#' # Question $1.b.$:Multiple Logistic Regression Model
#'
fit <- glm(abuse ~ program + boyfriend + white + welfare,
           data = dat,
           family = binomial(link = "logit"))
fit
anova(fit, test = "LRT")
#'
#' \newpage
#'
#' ## Model Fit Diagnostics
#'
#'
#+ outTest, results='asis', echo=TRUE
car::outlierTest(fit, cutoff = Inf)
#'
#'
#+ glmDX1, fig.height = 6, echo=FALSE
# When no observations are found with a bonferroni p-value exceeding the cutoff, the *one* with the largest Studentized residual is reported. ##
library(car)
dx1 <- read.csv("data/hw3-q1b2.csv") ## Saved output from "LogisticDX::dx()" ##
dx1046 <- as.data.frame(dx1[dx1$x2 == 1213, ])
plot(dx1$P, dx1$sPr, xlab = "Predicted Probabilities", ylab = "Standardized Pearson Residuals"); abline(h = 0, lwd = 1, col = pal_my[19], lty = 'dashed'); loessLine(predict(fit), residuals(fit), log.x = FALSE, log.y = FALSE, col=pal_my[5], smoother.args=list(lty=1, lwd=2, lty.spread=2, lwd.spread=1)); text(x = dx1046$P, y = dx1046$sPr+0.5, labels = "1046")
# car::residualPlot(fit, type = "pearson",
             # col.smooth = pal_my[5], id.n = 1, linear = FALSE)
#'
#'
library(car)
cutoff <- 4/((nrow(dat) - length(fit$coefficients) - 2))
plot(fit, which = 4, cook.levels = cutoff)
#'
#+ glmdx1, echo=FALSE, fig.height = 4.5

dx1$col <- mpal(1:nrow(dx1), p = sci, a = 0.55)
plot(
    dx1$P,
    dx1$dChisq,
    type = 'n',
    xlab = "Predicted Probabilities",
    ylab = expression(Delta ~  ~ Chi ^ 2)
)
points(
    dx1$P,
    dx1$dChisq,
    cex = 2,
    bg = dx1$col,
    col = pal_my[2],
    pch = 21,
    lwd = 0.5
)
text(x = dx1046$P,
     y = dx1046$dChisq - 3,
     labels = "1046")
lines(lowess(dx1$P, dx1$dChisq), lwd = 3, col = pal_my[18])
#'
#'
#' `r tufte::newthought(" ")`
#'
#+ glmdx13, echo=FALSE, fig.height = 4.25
plot(dx1$P,
     dx1$dBhat,
     type = 'n',
     xlab = "Predicted Probabilities",
     ylab = " ")
points(
    dx1$P,
    dx1$dBhat,
    cex = 2,
    bg = dx1$col,
    col = pal_my[2],
    pch = 21,
    lwd = 0.5
)
text(x = dx1046$P,
     y = dx1046$dBhat - 0.035,
     labels = "1046"); mtext(text = 
     						 expression(Delta ~  ~ hat(beta)), 
     						 side = 2, line = 2)
lines(lowess(dx1$P, dx1$dBhat), lwd = 3, col = pal_my[18])
#'
#'
#' `r tufte::newthought(" ")`
#'
#+ glmdx14, echo=FALSE, fig.height = 4.5
plot(
    dx1$P,
    dx1$dD,
    type = 'n',
    xlab = "Predicted Probabilities",
    ylab = expression(Delta ~  ~ "Deviance")
)
points(
    dx1$P,
    dx1$dD,
    cex = 2,
    bg = dx1$col,
    col = pal_my[2],
    pch = 21,
    lwd = 0.5
)
text(x = dx1046$P,
     y = dx1046$dD - 0.25,
     labels = "1046")
lines(lowess(dx1$P, dx1$dD), lwd = 3, col = pal_my[18])
#'
#'
#+ echo=FALSE
kable(dx1[dx1$x2 == 1213, c(-1, -2, -3, -9)], caption = "Residual Diagnostic Statistics for Case No. 1046", col.names = c("Standardized Pearson Residual", "Predicted Probability", "$\\Delta\\chisq$", "$\\Delta Deviance$", "$\\Delta\\hat{\\beta}$"))
#'
#' 
#'
#' `r tufte::newthought("Multiple Logistic Regression Diagnostics Summary")`. In the second residual plot above, with standardized pearson resdiuals on the Y-axis and predicted values on the X-axis, _Case \#`1046`_ is identified as an outlier. Examining the $\Delta\chisq$ and $\Delta\beta$ for this case (see above) against the aggregated descriptives for the full set of observations included in the model (see below), it is clear that this case is is an outlier. This conclusion is supported by the studentized residual outlier test provided above. However, the residual data visualizations collectively suggest that this one observation (i.e., _Case \#`1046`_) is not necessarily heavily influential on the fitted model's coefficients. For example, in the first diagostic plot provided above, the solid line represents the fitted _loess model_ for the tested model's predicted values against the model's residuals. The fitted loess line's slope appears to correspond appropriately with the data with little influence from the outlier case^[Located and labeled in the bottom right corner of the plot]. The same behavior is observed across subsequent visualizations three plots respectively showing the $\Delta\chisq$, $\Delta\beta$, and $\Delta Deviance (D)$ plotted against the tested model's predicted probabilities, where the solid gray line in each plot represents the best fitting (loess) curve for each diagnostic statistic against the predicted probabilities. In all of the above-described visualizations, the best fitting line appears most heavily influened by the data clustered toward the lower ends of each diagnostic statistic's range, rather than the labeled outlying data point in each plot. However, the difference between _Case \#`1046`'s_ predicted probability ($P = 0.9914$ in the table above) against the mean predicted probability for the full set of observations included in the model ($M = 0.07$ in the table below), suggests that this particular data point's predicted value could be influentual on the tested model's outcome (_Abuse_). This influence could increase the risk of Type I error regarding the model's predictors relations with the outcome. In particular, _Case \#`1046`'s_ relatively high score on the _Welfare_ predictor ($Welfare_{1046} = 8$, whereas $\mu_{Welfare} = `r mean(dat$welfare)`$) could influence the regression coefficient obtained for this predictor ($\beta_{Welfare} = `r fit$coefficients[[5]]`$).
#'
#' -----
#' 
#+ echo=FALSE
dxmsmm <- R.msmm(dx1[, c(-1, -2, -3, -9)])[, -5]
rownames(dxmsmm) <- c("Standardized Pearson Residual", "Predicted Probability", "$\\Delta\\chisq$", "$\\Delta Deviance$", "$\\Delta\\hat{\\beta}$")
kable(dxmsmm, caption = "Descriptive Statistics for Residual Diagnostics")
#'