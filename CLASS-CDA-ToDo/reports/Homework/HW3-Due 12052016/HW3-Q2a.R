#' ---
#' title: "Homework 3 - Question 2a"
#' author: "Riley Smith"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#'
#' -----
#' 
#' # Question-2: Widows Dataset
#' 
#+ setup, echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE
source("../SETUP.R")
knitr::opts_chunk$set(tidy = FALSE, echo = TRUE, cache = FALSE, fig.keep = 'high', fig.show = 'hold', results = 'asis', autodep = TRUE, Rplot = TRUE, dev = 'pdf', fig.path = 'graphics/HW2/rplot-q2b-', fig.width = 7, fig.height = 7, out.width = "\\linewidth")
#'
#' Data Summary
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
#+ lglmFits
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
#+ lrmresults2, results='asis'
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
#+ lrmresults3, results='asis'
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