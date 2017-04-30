#' ---
#' title: "Homework 3, Question 3b"
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
#'
#+ q3Dat
dat <- read.csv("data/states_new.csv")
dat <- dat[, c("Q3", "Q7", "Q8")]
dat <- na.omit(dat)
id <- seq(1:nrow(dat))
dat <- cbind(id, dat)
#'
#' # Multiple Logistic Regression Model
#'
#'
library(MASS)
fit <- polr(factor(Q8) ~ Q3 + Q7  + Q3*Q7, data = dat, contrasts = NULL, method = "probit")
library("AER")
fit.ctest <- coeftest(fit)

fit.ct.r <- dimnames(fit.ctest)[[1]] ## rownames ##
fit.ct.c <- c("Estimate", "_SE_", "_z-value_", "p") ## colnames ##
fit.summ <- matrix(fit.ctest, nrow = 5, dimnames = list(fit.ct.r, fit.ct.c)) %>% as.data.frame
sig <- list(s1 = "$*p < 0.05$.", s2 = "$**p < 0.01$.", s3 = "$***p < 0.001$.", ns = "' ' = n.s.")

R.sig <- function(p) {
	stars <- list(s1 = "$*$", s2 = "$**$", s3 = "$***$", ns = " ")
	p <- ifelse(p > 0.05, stars$ns, ## else...
			    ifelse(p < 0.05 & p > 0.01, stars$s1, ## else...
				       ifelse(p < 0.01 & p > 0.001, stars$s2, stars$s3)
			          )
			   )
    return(p)
}
## TEST OF R.sig() ##
# p <- c(0.1, 0.04, 0.002, 0.0001)
# sapply(p, R.sig)


fit.summ$s <- sapply(fit.summ$p, R.sig)

R.coeftest <- function(x) { ## 'x' = a model fit object ##
	x <- lmtest::coeftest(x)
    rn <- dimnames(x)[[1]] ## rownames ##
    cn <- c("Estimate", "_SE_", "_z-value_", "p") ## colnames ##
    summ <-
        matrix(x, nrow = dim(x)[1], dimnames = list(rn, cn)) %>% as.data.frame
    sig <-
        list(
            s1 = "$*p < 0.05$.",
            s2 = "$**p < 0.01$.",
            s3 = "$***p < 0.001$.",
            ns = "' ' = n.s."
        )
    R.sig <- function(p) {
        stars <- list(
            s1 = "$*$",
            s2 = "$**$",
            s3 = "$***$",
            ns = " "
        )
        p <- ifelse(p > 0.05,
                    stars$ns,
                    ## else...
                    ifelse(
                        p < 0.05 & p > 0.01,
                        stars$s1,
                        ## else...
                        ifelse(p < 0.01 &
                                   p > 0.001, stars$s2, stars$s3)
                    ))
        return(p)
    }
    summ$s <- sapply(summ$p, R.sig)
    return(list(coef.summ = summ, sig.codes = sig))
}
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
#'
#' -----
#' 
#' `r tufte::newthought("Ordinal Probit Regression Summary.")` An ordinal probit regression model was tested predicting whether states' batterer intervention program (BIP) standards were gender inclusive (_Q8_) by the size of state standards' committees (_Q3_), whether proccesses were in place for assessing BIPs' feedback about the standards (_Q7_), and the interaction of these two predictors (_Q3-x-Q7_). The predictors collectiely accounted for a significant amount of variance in the outcome, likelihood ratio $\chisq(2) = 18.79, p < .001$. However, only _Q7_ significantly predicted _Q8_, $b = 1.42, SE = .36, p < .001$; such that a one unit increase in _Q7_ was associated with approximately $34\%$ increases in _Q8_. Overall, the model accounted for $21\%$ of the variance in reported baseline income adequacy (_McFadden's pseudo_-$R\sq = 0.2064$).
#'
#' `r tufte::newthought("Comparison with Cumulative Logistic Model.")` The above described ordinal probit regression analysis findings are starkly different from the originally tested logistic regression model, such that the effects of _Q7_ are distinquishable in the probit model whereas they were not in the logistic regression model.
#'