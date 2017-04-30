#'
#' ---
#' title: "Homework 3, Question 1a (lavaan package)"
#' author: "Riley M. Smith"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#'
#+ setup, echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE
source("../SETUP.R")
knitr::opts_chunk$set(
    tidy = FALSE,
    echo = FALSE,
    cache = FALSE,
    fig.keep = 'high',
    fig.show = 'hold',
    results = 'asis',
    autodep = TRUE,
    Rplot = TRUE,
    dev = 'pdf',
    fig.path = 'graphics/HW2/rplot-q1-',
    fig.width = 7,
    fig.height = 7,
    out.width = "\\linewidth"
)
#'
source("../SETUP.R")
#'
#' # Q1.a 
#'
dat <- R.rspss("data/child2.sav")
# R.msmm(dat)
# na.omit(dat$white)

l.abuse <- c("no", "yes")
l.boyfriend <- c("no", "yes")
l.program <- c("control", "program")
l.white <- c("nonwhite", "white")

dat <- within(dat, {
	abuse.f <- factor(abuse, labels = l.abuse)
	boyfriend.f <- factor(boyfriend, labels = l.boyfriend)
	program.f <- factor(program, labels = l.program)
	white.f <- factor(white, labels = l.white)
})
kable(as.matrix(summary(dat[, 6:9])))
#'
#'
#' -----
#'
#' # Analysis Setup 
#'
#' 
med.f <- dat[, c(9, 8, 6)] ## Mediator variables as factors ##
names(med.f) <- c("Y", "M", "X")
med.f <- na.omit(med.f)

med.n <- dat[, c(1, 2, 5)] ## Mediator variables as numeric ##
names(med.n) <- c("Y", "M", "X")
med.n <- na.omit(med.n)

X <- med.n[, "X"]
M <- med.n[, "M"]
Y <- med.n[, "Y"]
ab <- cor(X, M) ## ab
ac <- cor(X, Y) ## ac
bc <- cor(M, Y) ## bc
#' 
#' Implied Correlations
#' ab*bc

#'
#' -----
#'
#' \newpage
#' ## Using `lavaan()` `{pkg:lavaan}`
#'
library(lavaan) ## "lavaan()", "sem()", & "simulateDate()" ## 
library(semPlot) ## "semPaths()" & "semCors()" ##

model <- '
		  ## Mediator ##
		  M ~ a*X
		  ## Mediation Model ##
		  Y ~ b1*X + b2*M
		 '

fit <- sem(model, data = med.n, estimator = "ML", link = "logit", information = "observed", mimic = "Mplus", meanstructure = TRUE, fixed.x = TRUE, parameterization = "theta")#se = "robust.sem"
pander(summary(fit, fit.measures = TRUE, rsq = TRUE))
pander(parameterEstimates(fit)) ## Y ~ 1 == model intercept ## 
pander(fitted(fit))
# resid(fit)
# AIC(fit)
# vcov(fit)
# fitMeasures(fit)
