#' ---
#' title: "Homework 3 - Question 1a"
#' author: "Riley Smith"
#' date: "`r format(Sys.Date(), '%d %b %Y')`"
#' ---
#'
#+ setup, echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE
source("../SETUP.R")
knitr::opts_chunk$set(tidy = FALSE, echo = TRUE, cache = FALSE, fig.keep = 'high', fig.show = 'hold', results = 'asis', autodep = TRUE, Rplot = TRUE, dev = 'pdf', fig.path = 'graphics/HW3/rplot-q1-', fig.width = 7, fig.height = 7, out.width = "\\linewidth", echo = FALSE)
#'
#' # Question-1: Second Early Head Start data set
#'
#+ dat1, fig.cap="Bivariate correlations (upper), implied bivariate loess model curves (lower), and individual variable distributions (diagonal)", fig.fullwidth=TRUE
dat <- R.rspss("data/child2.sav")
dat <- na.omit(dat)
R.msmm(dat)[, -5]
#'
#'
#' # Mediation Model Data Descriptive Statistics
#'
#+ medDat
med.n <- dat[, c(1, 6, 5)] ## Predictor (x) as numeric ##
names(med.n) <- c("Y", "M", "X")
#'
#'
#+ dat1Hists, fig.margin=TRUE, echo=FALSE
palette(pal_my)
bcol <- pal_my[c(16, 13)] %>% sapply(adjustcolor, alpha.f = 0.85)
barplot(table(med.n$X), xlab = " ", col = bcol, 
		main = "Mother's Race", 
		names.arg = c("\n0\n(Non-White)", "\n1\n(White)"))
#'
#'
#+ dat1Hists2, fig.margin=TRUE, echo=FALSE
library(MASS)
truehist(med.n$M, xlab = " ", freq = FALSE, col = mpal(seq(1:9), a = 0.75),
     main = "Child neglect reports"); lines(density(med.n$M),
                                                      col = 19, lwd = 2.5)
#'
#'
#+ dat1Hists3, fig.margin=TRUE, echo=FALSE
barplot(table(med.n$Y), xlab = " ", col = bcol, 
		main = "Any Abuse Reported", 
		names.arg = c("\n0\n(No)", "\n1\n(Yes)"))
#'
library(psych) ## pairs.panels() (which is an extension of pairs() from the base package) ##
pairs.panels(med.n, hist.col = pal_my[16], cex.cor = TRUE, col = pal_my[17])
#'
#' \newpage
#'
#+ interactionPlot, echo=FALSE 
with(med.n, {
    interaction.plot(X, M, Y, ## {pkg:stats} ##
        trace.label = "Neglect", fixed = TRUE,
        col = mpal(seq(1:length(unique(med.n$M))), p = sci), lwd = 2,
        main = "Interaction Effects Implied by Variable Correlations",
        ylab = expression(mu[Abuse]), xlab = "White")
})
#'
#'
#' ## Mediation Analysis using the `{lavaan}` `R`-Package
#'
#+ m, echo=TRUE
library(lavaan) ## "lavaan()", "sem()" ###'
m <- '#Y ~ c*X
       M ~ a*X
       Y ~ b*M + c*X
       ab := a*b
       total := c + (a*b)'
fit <- sem(m, data = med.n,
        estimator = "ML", ## Maximum likelihood estimator
                            ## yeilds estimates and params similar to SAS ##
        link = "logit", 
        ## ML only works with the probit link - UNLESS you set "se" to "bootstrap" (!!!) ##
        se = "bootstrap")
#'
#+ fit
## Model fit stats ##
fit.fits <- fitMeasures(fit)[c('aic', 'bic')]
fit.llr <- logLik(fit)
fit.rsq <- inspect(fit, 'r2')

## Model summary stats ##
fit.mus <- fitted(fit)$mean
fit.cov <- fitted(fit)$cov
fit.cor <- cov2cor(fit.cov)

fit.bci <- parameterEstimates(fit, level = 0.95, boot.ci.type = "bca.simple")

fit.par <- parameterEstimates(fit) ## Y ~ 1 == model intercept ##
    ## Below is for formatting purposes ##
params2 <- matrix(paste0(fit.par[, 1], " ",
                           fit.par[, 2], " ",
                           fit.par[, 3]))
fit.par <- cbind(params2, fit.par[, c(-1:-4)]) %>% data.frame()

fit.se <- cbind(params2, fit.par$se)
fit.coefs <- cbind(params2, fit.par$est)

fit.int <- fit.par[7, 2] %>% as.numeric() %>% round(3) ## b0 ##

fit.M <- fit.par[3, 2] %>% as.numeric() %>% round(3) ## Y ~ M (b2) ## 
fit.a <- fit.par[2, 2] %>% as.numeric() %>% round(3) ## M ~ X ##
fit.X <- fit.par[1, 2] %>% as.numeric() %>% round(3) ## Y ~ X (b1) ##
fit.ab <- fit.par[10, 2] %>% as.numeric() %>% round(3) ## Y ~ X*M (b3) ##
#'
#'
#' ## Mediation Model Summary Statistics and Model Fit Indices
#'
#+ fitKabs, echo=FALSE
fit.par[, -1] <- apply(fit.par[, -1], 2, round, digits = 4)
fit.par <- apply(fit.par, 2, R.na, v = " ")

labs <- c("Parameter", "Estimate", "SE", "Z", "P-Value", "$CI_{lower}$", "$CI_{upper}$")

kable(
    fit.par,
    digits = 2,
    caption = "Model 2 (Mediation Model) Summary Statistics",
    align = c("c", rep("r", ncol(fit.par))), col.names = labs
)



kable(fit.rsq, caption = "Model 2 $R\\sq$")

fit.cov[upper.tri(fit.cov)] <- 0
kable(fit.cov, caption = "Model 2 Covariance Matrix",
      format.args = list(zero.print = " "))
kable(fit.mus, caption = "Model 2 Coefficients' Means")

# fit.llr
names(fit.fits) <- c("AIC", "BIC")
kable(fit.fits, caption = "Absolute Fit Indices")

anova(fit)
#'
#' `r tufte::newthought("Mediation Analysis Summary.")` Two regression models were tested to investigate whether the association between mother's race (white/non-white) and any reported abuse (yes/no) is mediated by the number of child neglect reports. The hypothesized mediator variable, number of neglect reports, was first regressed on the mother's race (the hypothesized predictor variable), and in the second model any reported abuse was regressed on both the count of neglect reports and the mothers race variable. In the first regression model, mother's race was significantly related to higher counts of reported neglect, $b = 0.082, SE = .036, p < .05, 95\% CI = .0103, .1553$. In the model, both the mother's race, $b = .045, SE = .015, p = < .01, 95\% CI .016, .013$, and counts of neglect reports, $b = .071, SE = .023, 95\% CI = .031, .121$, were significantly associated whether there were any reports of abuse. In addition, the indirect effect was marginally significant, $b = .006, SE = .003, p = 0.064$, with the boostrap confidence intervals derived from 1000 samples not including 0 ($95\% CI = .0006, .0127$). Taken together, however, these regression analyses do not support the mediation hypothesis regarding the relations among the variables tested, as the hypothesized mediating variable's effect was not influentual on the relationship between the hypotehsized predictor and the outcome.
#'
#'
#' $$ logit(\hat{Y}) = `r fit.int`\beta_{0} + `r fit.M`\beta_{1_{M}} + `r fit.X`c'X $$
#'
#+ fitpathMod, echo=FALSE, fig.cap="Fitted Model - Partial Mediation Effect Observed. '*': p < 0.05, '**': p < 0.01, '***': p < 0.001"
library(pathdiagram)
palette(pal_my)
iv <- list(X <- manifest("White\n(X)", x = 0, y = 0.25,
                         width = 0.2, height = 0.1, cex = .75,
                         fill = NA, col = 16, lwd = 1, border = 16))
mv <- list(M <- manifest("Neglect\n(M)", x = 0.5, y = 0.75,
                         width = 0.25, height = 0.1, cex = .75,
                         fill = NA, col = 13, lwd = 1, border = 13))
dv <- list(Y <- manifest("Abuse\n(Y)", x = 1, y = 0.25,
                         width = 0.2, height = 0.1, cex = .75,
                         fill = NA, col = 5, lwd = 1, border = 5))
aPath <- paste0("a = ", fit.a, "***")
bPath <- paste0("b = ", fit.M, "*")
ab <- paste0("[a X b] = ", fit.ab)
cPath <- paste0("c' = ", fit.X, "***")
txt.x = list(aPath = 0.17, bPath = 0.82, ab = 0.5, cPath = 0.5)
txt.y <- list(aPath = 0.5, bPath = 0.5, ab = 0.4, cPath = 0.19)
R.mpathDia(iv, mv, dv, arrcol = 19, lty.c = 1); text(x = txt.x, y = txt.y,
                                                     labels = c(aPath,
                                                                bPath,
                                                                ab,
                                                                cPath),
                                                     font = 3)


#' \newpage
#' 
#'#' # Question $1.b.$:Multiple Logistic Regression Model
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
#' \newpage
#' 
#'
#' # Question-2: Widows Dataset
#' 
#'
#' ## Data Summary
#'
#+ dat, fig.cap="Bivariate correlations (upper), implied bivariate loess model curves (lower), and individual variable distributions (diagonal)", fig.fullwidth=TRUE
dat <- R.rspss("data/widow.sav")
R.msmm(dat)
psych::pairs.panels(na.omit(dat[, -1]), hist.col = pal_my[16], cex.cor = TRUE, col = pal_my[17])
dat <- na.omit(dat)
#'
#'
#' # Question 2.a.
#'
#' <!--Use `SPSS`, `R`, or `SAS` to test a lagged regression model to investigate whether income adequacy (`INCADQ1`) and health (`HEALTH1`) predicted loneliness at Time 2 (`LONELY2`) after controlling for loneliness at Time 1 (LONELY1). Report and interpret your findings, with special attention to the interpretation of the longitudinal model. Be sure to include the regression coefficients, the odds ratios, confidence limits, model fit information, and a pseudo-R2 measure.-->
#'
dat.a <- dat[, c("lonely2", "lonely1", "incadq1", "health1")]
names(dat.a) <- c("Y", paste0("x", seq(1:3)))
#'
#+ results='asis', echo=FALSE
labs <- cbind(names(dat[, c("lonely2", "lonely1", "incadq1", "health1")]), names(dat.a))
kable(labs, caption = "Variable Name Assignments for Analysis", col.names = c("Original Variable Name", "Label in Model"))
#'
#+ lglm, fig.margin=TRUE
fit <- glm(Y ~ x1 + x2 + x3, data = dat.a, family = binomial(link = "logit"))
fit
anova(fit, test = "LRT")
#'
#'
#+ 
## **Null Deviance** ##
nulldev <- fit$null.deviance
nulldev.df <- fit$df.null
nulldev.p <- c(round(nulldev, 2), nulldev.df)

## **Residual Deviance** ##
dev <- fit$deviance
dev.df <- fit$df.residual
dev.p <- c(round(dev, 2), dev.df)

## **AIC** ##
aic <- fit$aic
aic.p <- c(round(aic, 2), " ")

fits <- as.data.frame(rbind(nulldev.p, dev.p, aic.p))
rownames(fits) <- c("**Null Deviance**", "**Residual Deviance**", "**AIC**")
names(fits) <- c("Estimate", "Degrees of Freedom")

CI.b <- confint(fit, trace = FALSE)

OR <- exp(coef(fit))
CI <- confint(fit)

CIb <- cbind(coef(fit), CI.b)
CIb.labs <- c("**$\\beta$**", dimnames(confint(fit))[[2]])
CIb <- rbind(CIb.labs, round(CIb, 4))
dimnames(CIb)[[1]][1] <- " "
dimnames(CIb)[[2]] <- c(" ", "$CI_{\\beta}$", " ")

## odds ratios and 95% CI ##
ORCI <- exp(cbind(coef(fit), CI))
ORCI.labs <- c("**$\\Phi$**", dimnames(confint(fit))[[2]])
ORCI <- rbind(ORCI.labs, round(ORCI, 4))
dimnames(ORCI)[[1]][1] <- " "
dimnames(ORCI)[[2]] <- c(" ", "$CI_{\\Phi}$", " ")

library(lmtest) ## "lrtest()" ##
lrchsq <- lrtest(fit)[2, -3]
lrchsq <- lrchsq[, c(2, 1, 3, 4)]
names(lrchsq) <- c("Log Likelihood", "_df_", "$\\chisq$", "_p_")

library(modEvA)
rsq <- RsqGLM(model = fit) %>% sapply(round, 4)
rsq.mf <- rsq[3] %>% round(digits = 2)
rsq.perc <- (rsq.mf*100)
#+ echo=FALSE
kable(lrchsq, row.names = FALSE, caption = "Likelihood Ratio $\\chi^{2}$")
#'
#+ results='asis'
kable(CIb, align = rep('r', ncol(CIb)),
      caption = "Logistic Regression Coefficients ($\\beta$) \\&
      Coresponding Confidence Intervals (_CI_)")

kable(ORCI, align = rep('r', ncol(ORCI)),
      caption = "Logistic Regression Odds Ratios ($\\Phi$) \\&
      Coresponding Confidence Intervals (_CI_) [note]") %>%
    add_footnote("Confidence intervals are based on the logistic regression
                 model's profiled log-likelihood function,
                 rather than the standard errors",
                 threeparttable = TRUE)
#'
#+ results='asis'
kable(fits, caption = "Additional Logistic Regression Model Fit Statistics")

fit.coef <- coef(fit) %>% sapply(round, 2)
b0 <- fit.coef[[1]] ## Intercept ##
b1 <- fit.coef[[2]] ## "lonely1" ##
b2 <- fit.coef[[3]] ## "inacdq1" ##
b3 <- fit.coef[[4]] ## "health1" ##

fit.se <- summary(fit)$coefficients[, 2] %>% sapply(round, 2)
se0 <- fit.se[[1]] ## Intercept ##
se1 <- fit.se[[2]] ## "lonely1" ##
se2 <- fit.se[[3]] ## "inacdq1" ##
se3 <- fit.se[[4]] ## "health1" ##

OR0 <- OR[[1]] %>% sapply(round, 2) ## Intercept ##
OR1 <- OR[[2]] %>% sapply(round, 2) ## "lonely1" ##
OR2 <- OR[[3]] %>% sapply(round, 2) ## "inacdq1" ##
OR3 <- OR[[4]] %>% sapply(round, 2) ## "health1" ##

fit.p <- summary(fit)$coefficients[, 4] %>% sapply(round, 3)
p0 <- fit.p[[1]] ## Intercept ##
p1 <- .001 ## "lonely1" ##
p2 <- fit.p[[3]] ## "inacdq1" ##
p3 <- .05 ## "health1" ##
#'
#' \newpage
#' ## Lagged Logistic Regression Summary
#'
#+ fig.margin=TRUE
library(popbio)
logi.hist.plot(dat$incadq1, dat$lonely2, boxp = FALSE, type = "hist", col = "gray", main = "\nIncome Adequacy")
#'
#+ fig.margin=TRUE
logi.hist.plot(dat$health1, dat$lonely2, boxp = FALSE, type = "hist", col = "gray", main = "\nHealth")
#'
#+ fig.margin=TRUE
logi.hist.plot(dat$lonely1, dat$lonely2, boxp = FALSE, type = "hist", col = "gray", main = "\nLonely (T1)")

#'
#'
#' A lagged logistic regression model was tested to investigate whether baseline reported income adequacy and health predicted loneliness at Time-2 after controlling for loneliness at Time-1.<!-- Respondents' self=reported Time-2 longliness was regressed on self-reported Time-1 loneliness and baseline self-reports of income adequecy and overall health.--> In line with expectations based on bivariate correlations among the outcome variable and each predicter included in the model (see Figure 1), income adequecy reported at baseline did not significantly relate to Time-2 reported loneliness ($b = `r b2`, SE = `r se2`, OR = `r OR2`, p = `r p2`$), whereas both Time-1 reported loneliness and baseline  health significantly predicted loneliness at Time-2. Specifically, respondents higher in loneliness at Time-1 were more likely to have higher levels of Time-2 loneliness relative to respondents with lower levels of loneliness at either timepoint, $b = `r b1`, SE = `r se1`, OR = `r OR1`, p < `r p1`$. In contrast, respondents with higher baseline health scores were more likely to have lower Time-2 reported loneliness relative to respondents with lower baseline health scores and higher Time-2 reported loneliness, $b = `r b3`, SE = `r se3`, OR = `r OR3`, p < `r p3`$. Overall,  the model accounted for `r rsq.perc` of the variance in reported Time-2 loneliness (_McFadden's pseudo_-$R\sq = `r rsq.mf`$).
#'
#' \newpage
#' 
#' # Question 2.b.
#'
#+ clgm
library(MASS)
options(contrasts = c("contr.treatment", "contr.poly"))
fit <- polr(factor(incadq1) ~ age1 + educ1  + health1, data = dat, Hess = TRUE, method = "logistic")
## coefficient test
library("AER")
print(coeftest(fit))
#'
#+ echo=FALSE
fit.summ <- summary(fit)[[1]] %>% data.frame()
kable(fit.summ, caption = "Cumulative Logistic Model: $polr(formula = factor(incadq1) ~ age1 + educ1 + health1, data = dat, contrasts = NULL, method = 'logistic')$", col.names = c("_Coefficient_", "_SE_", "_T-Value_"))
#'
#'
#+ 
CI.b <- confint(fit, trace = FALSE)

OR <- exp(coef(fit))
CI <- confint(fit)

CIb <- cbind(coef(fit), CI.b)
CIb.labs <- c("**$\\beta$**", dimnames(confint(fit))[[2]])
CIb <- rbind(CIb.labs, round(CIb, 4))
dimnames(CIb)[[1]][1] <- " "
dimnames(CIb)[[2]] <- c(" ", "$CI_{\\beta}$", " ")

## odds ratios and 95% CI ##
ORCI <- exp(cbind(coef(fit), CI))
ORCI.labs <- c("**$\\Phi$**", dimnames(confint(fit))[[2]])
ORCI <- rbind(ORCI.labs, round(ORCI, 4))
dimnames(ORCI)[[1]][1] <- " "
dimnames(ORCI)[[2]] <- c(" ", "$CI_{\\Phi}$", " ")


library(lmtest) ## "lrtest()" ##
lrchsq <- lrtest(fit)[2, -3]
lrchsq <- lrchsq[, c(2, 1, 3, 4)]
names(lrchsq) <- c("Log Likelihood", "_df_", "$\\chisq$", "_p_")

library(pscl)
rsq <- pR2(fit)
#'
#'
#'`r tufte::newthought("Model Fit Indices")`
#'
#+ results='asis'
kable(lrchsq, row.names = FALSE, caption = "Likelihood Ratio $\\chi^{2}$")


kable(CIb, align = rep('r', ncol(CIb)),
      caption = "Logistic Regression Coefficients ($\\beta$) \\&
      Coresponding Confidence Intervals (_CI_)")

kable(ORCI, align = rep('r', ncol(ORCI)),
      caption = "Logistic Regression Odds Ratios ($\\Phi$) \\&
      Coresponding Confidence Intervals (_CI_) [note]") %>%
    add_footnote("Confidence intervals are based on the logistic regression
                 model's profiled log-likelihood function,
                 rather than the standard errors",
                 threeparttable = TRUE)
#'
#'
#' -----
#'
#'
#' `r tufte::newthought("Cumulative Logistic Regression Summary.")` An ordered logistic regression model was tested to investigate whether age, education, and health at baseline predicted baseline reported income adequacy. The predictors collectiely accounted for a significant amount of variance in the outcome, likelihood ratio $\chisq(3) = 9.379, p < .05$. However, only baseline health independently predicted income adequacy at baseline, $b = .3581, SE = .1607, OR = 1.4306, p < .05$; such that point increases in baseline health were each associated with approximately $35\%$ increases in baseline income adequacy. Overall, the model accounted for $1.3\%$ of the variance in reported baseline income adequacy (_McFadden's pseudo_-$R\sq = 0.013$).
#'
#'
#' \newpage
#' 
#' # Question 2.c.
#'
dat <- within(dat, {
    age1.c <- age1 - mean(age1)
    educ1.c <- educ1 - mean(educ1)
    health1.c <- health1 - mean(health1)
})
#'
#' # Ordinal Probit Model Fit
#'
#+ cfit
library(MASS)

fit <- polr(factor(incadq1) ~ age1.c + educ1.c  + health1.c, data = dat, contrasts = NULL, method = "probit")
library("AER")
print(coeftest(fit))

#'
#'
#+ 
CI.b <- confint(fit, trace = FALSE)

OR <- exp(coef(fit))
CI <- confint(fit)

CIb <- cbind(coef(fit), CI.b)
CIb.labs <- c("**$\\beta$**", dimnames(confint(fit))[[2]])
CIb <- rbind(CIb.labs, round(CIb, 4))
dimnames(CIb)[[1]][1] <- " "
dimnames(CIb)[[2]] <- c(" ", "$CI_{\\beta}$", " ")

## odds ratios and 95% CI ##
ORCI <- exp(cbind(coef(fit), CI))
ORCI.labs <- c("**$\\Phi$**", dimnames(confint(fit))[[2]])
ORCI <- rbind(ORCI.labs, round(ORCI, 4))
dimnames(ORCI)[[1]][1] <- " "
dimnames(ORCI)[[2]] <- c(" ", "$CI_{\\Phi}$", " ")


library(lmtest) ## "lrtest()" ##
lrchsq <- lrtest(fit)[2, -3]
lrchsq <- lrchsq[, c(2, 1, 3, 4)]
names(lrchsq) <- c("Log Likelihood", "_df_", "$\\chisq$", "_p_")

library(pscl)
rsq <- pR2(fit)["McFadden"]
rsq[[1]]
# names(rsq) <- c("McFadden's Pseudo-$R\\sq$")
# rsq.perc <- (rsq[2]*100)
#'
#' ## Model Fit Indices
#'
#+ results='asis', echo=FALSE
pander(lrchsq, row.names = FALSE, caption = "Likelihood Ratio $\\chisq$, $R\\sq$, \\& $G\\sq$")
#'
#+ results='asis'
kable(CIb, align = rep('r', ncol(CIb)),
      caption = "Logistic Regression Coefficients ($\\beta$) \\&
      Coresponding Confidence Intervals (_CI_)")

kable(ORCI, align = rep('r', ncol(ORCI)),
      caption = "Logistic Regression Odds Ratios ($\\Phi$) \\&
      Coresponding Confidence Intervals (_CI_) [note]") %>%
    add_footnote("Confidence intervals are based on the logistic regression
                 model's profiled log-likelihood function,
                 rather than the standard errors",
                 threeparttable = TRUE)
#'
#'
#' -----
#'
#' `r tufte::newthought("Ordinal Probit Regression Summary.")` An ordinal probit regression model was tested to investigate whether age, education, and health at baseline predicted baseline reported income adequacy. The predictors collectiely accounted for a significant amount of variance in the outcome, likelihood ratio $\chisq(3) = 10.84, p < .05$. However, only baseline health independently predicted income adequacy at baseline, $b = .2384, SE = .0947, p < .05$; such that point increases in baseline health were each associated with approximately $23\%$ increases in baseline income adequacy. Overall, the model accounted for $1.5\%$ of the variance in reported baseline income adequacy (_McFadden's pseudo_-$R\sq = 0.015$).
#'
#' `r tufte::newthought("Comparison with Cumulative Logistic Model.")` The above described ordinal probit regression analysis findings only differ from the cumulative findings in terms of the magnitude of the coefficients and, somewhat in terms of total model's variance explained (i.e., _McFadden's pseudo_-$R\sq = 0.015$ versus $0.013$ in the logistic analysis). Overall, however, the same conclusions made based on the cumulative logistic regression model can be made based on findings from the probit model analysis here.
#'
#' \newpage
#' 
#' # Question 3
#' 
#' # Question 3.a.
#'
#' The data for the remaing sets of analyses are from a national telephone interview study of batterer intervention program (BIP) standards advisory and compliance monitoring committees. Respondents were asked a series of questions varying in structure from _open-ended_ to simple _yes-or-no_. These analyses will concern the discrete data collected in response to the following interview questions:
#'
#' > How many members currently serve on your standards committee?
#' > Does your organization have methods for assessing programs’ feedback about the standards?
#' > Do the standards apply to programs designed for all genders?
#' 
#' The primary interest for the below analyses relate to an overarching effort to implement and sustain effective and appropriate anti-violence intervention strategies among female-identified perpetrators of same-sex violence. The above listed questions provide a mix of continuous numeric and dichotomous (`0 = No`; `1 = Yes`) indicators of responding states' current organizational and ideological capacities for such intervention strategies. 
#'
#' # Data Descriptives
#'
#'
#+ q3Dat
dat <- read.csv("data/states_new.csv")
# datmap <- read.csv("data/usmap.csv")

dat <- dat[, c("Q3", "Q7", "Q8")]
dat <- na.omit(dat)
id <- seq(1:nrow(dat))
dat <- cbind(id, dat)
#'
#'
#+ results='asis', fig.margin=TRUE
dat$Q8 <- ifelse(dat$Q8 == 2, 1, 0)
dat$Q7 <- ifelse(dat$Q7 == 2, 1, 0)
dat <- within(dat, {## could also do this with an "apply()" function ##
    Assessments <- factor(Q7, levels = c(0, 1), labels = c("No", "Yes"))
    Gender.Inclusive <- factor(Q8, levels = c(0, 1), labels = c("No", "Yes"))
})

# dat <- within(dat, {## could also do this with an "apply()" function ##
    # Q7 <- Q7 - mean(Q7)
    # Q3 <- Q3 - mean(Q3)
# })
#' # Multiple Logistic Regression Model
#'
#'
fit <- glm(Q8 ~ Q3 + Q7 + Q3*Q7, 
           data = dat, 
           family = binomial(link = "logit")) 

#'
#'
#+ 
CI.b <- confint(fit, trace = FALSE)

OR <- exp(coef(fit))
CI <- confint(fit)

CIb <- cbind(coef(fit), CI.b)
CIb.labs <- c("**$\\beta$**", dimnames(confint(fit))[[2]])
CIb <- rbind(CIb.labs, round(CIb, 4))
dimnames(CIb)[[1]][1] <- " "
dimnames(CIb)[[2]] <- c(" ", "$CI_{\\beta}$", " ")

## odds ratios and 95% CI ##
ORCI <- exp(cbind(coef(fit), CI))
ORCI.labs <- c("**$\\Phi$**", dimnames(confint(fit))[[2]])
ORCI <- rbind(ORCI.labs, round(ORCI, 4))
dimnames(ORCI)[[1]][1] <- " "
dimnames(ORCI)[[2]] <- c(" ", "$CI_{\\Phi}$", " ")


library(lmtest) ## "lrtest()" ##
lrchsq <- lrtest(fit)[2, -3]
lrchsq <- lrchsq[, c(2, 1, 3, 4)]
names(lrchsq) <- c("Log Likelihood", "_df_", "$\\chisq$", "_p_")

library(pscl)
rsq <- pR2(fit)["McFadden"]
rsq[[1]]
# names(rsq) <- c("McFadden's Pseudo-$R\\sq$")
# rsq.perc <- (rsq[2]*100)
#'
#' ## Model Fit Indices
#'
#+ results='asis', echo=FALSE
pander(lrchsq, row.names = FALSE, caption = "Likelihood Ratio $\\chisq$, $R\\sq$, \\& $G\\sq$")
#'
#+ results='asis'
kable(CIb, align = rep('r', ncol(CIb)),
      caption = "Logistic Regression Coefficients ($\\beta$) \\&
      Coresponding Confidence Intervals (_CI_)")

kable(ORCI, align = rep('r', ncol(ORCI)),
      caption = "Logistic Regression Odds Ratios ($\\Phi$) \\&
      Coresponding Confidence Intervals (_CI_) [note]") %>%
    add_footnote("Confidence intervals are based on the logistic regression
                 model's profiled log-likelihood function,
                 rather than the standard errors",
                 threeparttable = TRUE)
#'
#'
#'
#' -----
#'
#' `r tufte::newthought("Ordinal Probit Regression Summary.")` An ordinal probit regression model was tested to investigate whether age, education, and health at baseline predicted baseline reported income adequacy. The predictors collectiely accounted for a significant amount of variance in the outcome, likelihood ratio $\chisq(3) = 10.84, p < .05$. However, only baseline health independently predicted income adequacy at baseline, $b = .2384, SE = .0947, p < .05$; such that point increases in baseline health were each associated with approximately $23\%$ increases in baseline income adequacy. Overall, the model accounted for $1.5\%$ of the variance in reported baseline income adequacy (_McFadden's pseudo_-$R\sq = 0.015$).
#'
#' `r tufte::newthought("Comparison with Cumulative Logistic Model.")` The above described ordinal probit regression analysis findings only differ from the cumulative findings in terms of the magnitude of the coefficients and, somewhat in terms of total model's variance explained (i.e., _McFadden's pseudo_-$R\sq = 0.015$ versus $0.013$ in the logistic analysis). Overall, however, the same conclusions made based on the cumulative logistic regression model can be made based on findings from the probit model analysis here.
#'
#' \newpage
#'
#' # Question 3.b. Multiple Logistic Regression Model
#'
#'
library(MASS)
fit <- polr(factor(Q8) ~ Q3 + Q7  + Q3*Q7, data = dat, contrasts = NULL, method = "probit")
library("AER")
print(coeftest(fit))
#'
#'
#+ 
CI.b <- confint(fit, trace = FALSE)

OR <- exp(coef(fit))
CI <- confint(fit)

CIb <- cbind(coef(fit), CI.b)
CIb.labs <- c("**$\\beta$**", dimnames(confint(fit))[[2]])
CIb <- rbind(CIb.labs, round(CIb, 4))
dimnames(CIb)[[1]][1] <- " "
dimnames(CIb)[[2]] <- c(" ", "$CI_{\\beta}$", " ")

## odds ratios and 95% CI ##
ORCI <- exp(cbind(coef(fit), CI))
ORCI.labs <- c("**$\\Phi$**", dimnames(confint(fit))[[2]])
ORCI <- rbind(ORCI.labs, round(ORCI, 4))
dimnames(ORCI)[[1]][1] <- " "
dimnames(ORCI)[[2]] <- c(" ", "$CI_{\\Phi}$", " ")


library(lmtest) ## "lrtest()" ##
lrchsq <- lrtest(fit)[2, -3]
lrchsq <- lrchsq[, c(2, 1, 3, 4)]
names(lrchsq) <- c("Log Likelihood", "_df_", "$\\chisq$", "_p_")

library(pscl)
rsq <- pR2(fit)["McFadden"]
rsq[[1]]
# names(rsq) <- c("McFadden's Pseudo-$R\\sq$")
# rsq.perc <- (rsq[2]*100)
#'
#' ## Model Fit Indices
#'
#+ results='asis', echo=FALSE
pander(lrchsq, row.names = FALSE, caption = "Likelihood Ratio $\\chisq$, $R\\sq$, \\& $G\\sq$")
#'
#+ results='asis'
kable(CIb, align = rep('r', ncol(CIb)),
      caption = "Logistic Regression Coefficients ($\\beta$) \\&
      Coresponding Confidence Intervals (_CI_)")

kable(ORCI, align = rep('r', ncol(ORCI)),
      caption = "Logistic Regression Odds Ratios ($\\Phi$) \\&
      Coresponding Confidence Intervals (_CI_) [note]") %>%
    add_footnote("Confidence intervals are based on the logistic regression
                 model's profiled log-likelihood function,
                 rather than the standard errors",
                 threeparttable = TRUE)
#'
#'
#'
#' -----
#' 
#' `r tufte::newthought("Ordinal Probit Regression Summary.")` An ordinal probit regression model was tested predicting whether states' batterer intervention program (BIP) standards were gender inclusive (_Q8_) by the size of state standards' committees (_Q3_), whether proccesses were in place for assessing BIPs' feedback about the standards (_Q7_), and the interaction of these two predictors (_Q3-x-Q7_). The predictors collectiely accounted for a significant amount of variance in the outcome, likelihood ratio $\chisq(2) = 18.79, p < .001$. However, only _Q7_ significantly predicted _Q8_, $b = 1.42, SE = .36, p < .001$; such that a one unit increase in _Q7_ was associated with approximately $34\%$ increases in _Q8_. Overall, the model accounted for $21\%$ of the variance in reported baseline income adequacy (_McFadden's pseudo_-$R\sq = 0.2064$).
#'
#' `r tufte::newthought("Comparison with Cumulative Logistic Model.")` The above described ordinal probit regression analysis findings are starkly different from the originally tested logistic regression model, such that the effects of _Q7_ are distinquishable in the probit model whereas they were not in the logistic regression model.
#'