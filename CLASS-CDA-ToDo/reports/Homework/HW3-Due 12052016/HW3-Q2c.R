#' ---
#' title: "Homework 3 - Question 2c"
#' author: "Riley Smith"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#'
#' -----
#'
#'
#+ setup, echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE
source("../SETUP.R")
knitr::opts_chunk$set(tidy = FALSE, echo = TRUE, cache = FALSE, fig.keep = 'high', fig.show = 'hold', results = 'asis', autodep = TRUE, Rplot = TRUE, dev = 'pdf', fig.path = 'graphics/HW3/rplot-q2c-', fig.width = 7, fig.height = 7, out.width = "\\linewidth")
#'
#' # Data Summary
#'
#+ cdat
dat <- R.rspss("data/widow.sav")
R.msmm(dat)
# psych::pairs.panels(dat[, -1], hist.col = pal_my[16], cex.cor = TRUE)

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
#+ lglmFits
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
#'
#' -----
#'
#' `r tufte::newthought("Ordinal Probit Regression Summary.")` An ordinal probit regression model was tested to investigate whether age, education, and health at baseline predicted baseline reported income adequacy. The predictors collectiely accounted for a significant amount of variance in the outcome, likelihood ratio $\chisq(3) = 10.84, p < .05$. However, only baseline health independently predicted income adequacy at baseline, $b = .2384, SE = .0947, p < .05$; such that point increases in baseline health were each associated with approximately $23\%$ increases in baseline income adequacy. Overall, the model accounted for $1.5\%$ of the variance in reported baseline income adequacy (_McFadden's pseudo_-$R\sq = 0.015$).
#'
#' `r tufte::newthought("Comparison with Cumulative Logistic Model.")` The above described ordinal probit regression analysis findings only differ from the cumulative findings in terms of the magnitude of the coefficients and, somewhat in terms of total model's variance explained (i.e., _McFadden's pseudo_-$R\sq = 0.015$ versus $0.013$ in the logistic analysis). Overall, however, the same conclusions made based on the cumulative logistic regression model can be made based on findings from the probit model analysis here.
#'