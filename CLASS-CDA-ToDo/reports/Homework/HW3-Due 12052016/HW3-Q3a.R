#' ---
#' title: "Homework 3, Question 3a"
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
#' `r tufte::newthought("Ordinal Probit Regression Summary.")` An ordinal probit regression model was tested to investigate whether age, education, and health at baseline predicted baseline reported income adequacy. The predictors collectiely accounted for a significant amount of variance in the outcome, likelihood ratio $\chisq(3) = 10.84, p < .05$. However, only baseline health independently predicted income adequacy at baseline, $b = .2384, SE = .0947, p < .05$; such that point increases in baseline health were each associated with approximately $23\%$ increases in baseline income adequacy. Overall, the model accounted for $1.5\%$ of the variance in reported baseline income adequacy (_McFadden's pseudo_-$R\sq = 0.015$).
#'
#' `r tufte::newthought("Comparison with Cumulative Logistic Model.")` The above described ordinal probit regression analysis findings only differ from the cumulative findings in terms of the magnitude of the coefficients and, somewhat in terms of total model's variance explained (i.e., _McFadden's pseudo_-$R\sq = 0.015$ versus $0.013$ in the logistic analysis). Overall, however, the same conclusions made based on the cumulative logistic regression model can be made based on findings from the probit model analysis here.
#'