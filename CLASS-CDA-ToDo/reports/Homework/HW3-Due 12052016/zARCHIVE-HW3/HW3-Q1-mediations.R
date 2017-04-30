#' ---
#' title: "Homework 3, Question 1a (mediations package)"
#' author: "Riley M. Smith"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#'
#+ setup, echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE
source("../SETUP.R")
knitr::opts_chunk$set( tidy = FALSE, echo = TRUE, cache = FALSE, fig.keep = 'high', fig.show = 'hold', results = 'asis', autodep = TRUE, Rplot = TRUE, dev = 'pdf', fig.path = 'graphics/HW2/rplot-q1-', fig.width = 7, fig.height = 7, out.width = "\\linewidth"
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
# ab*bc
# ac
#'
#'
#' -----
#'
#' ## Mediation Analysis using `mediations() ({pkg:mediation})`
#'
library(mediation)

treatment <- "X"
mediators <- "M"
outcome <- "Y"
datasets <- list(med.n = med.n)
medmodel <- mediations(datasets, treatment, mediators, outcome,			families=c("gaussian","binomial"), interaction=FALSE,			conf.level=.95, sims=50)
# summary(medmodel)
# names(medmodel[["Y.X.M"]])
#'
#' \newpage
#'
mod.y <- medmodel$Y.X.M[["model.y"]]
mod.m <- medmodel$Y.X.M[["model.m"]]

mod.y
mod.m

plot(medmodel, labels = c("Indirect", "Direct", "Total"))


mod.diref0 <- medmodel$Y.X.M[["z0"]]
mod.diref1 <- medmodel$Y.X.M[["z1"]]
mod.medef0 <- medmodel$Y.X.M[["d0"]]
mod.medef1 <- medmodel$Y.X.M[["d1"]]
x <- data.frame(dir = c(mod.diref0, mod.diref1), med = c(mod.medef0, mod.medef1))
rownames(x) <- c("0", "1")
#'
#'
#+ echo=FALSE
kable(x, col.names = c("Direct Effects", "Mediation Effects"), caption = "Average Effects under Control \\& Treatment Conditions") #%>%
	# add_footnote(c("Point estimates for average direct effect under the control and treatment conditions.\n", "Point estimates for average causal mediation effects under the control and treatment conditions"))

