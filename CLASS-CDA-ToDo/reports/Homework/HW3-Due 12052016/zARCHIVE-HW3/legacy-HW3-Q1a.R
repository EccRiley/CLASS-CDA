#' ---
#' title: "Homework 3 - Question 1"
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
R.msmm(dat)[, -1]
#'
#' # Q1.a.\inprogress
#'
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
#'
#' # Mediation Model Data Descriptive Statistics
#'
#+ medDat
med.n <- dat[, c(1, 6, 5)] ## Predictor (x) as numeric ##
names(med.n) <- c("Y", "M", "X")
psych::pairs.panels(med.n, hist.col = pal_my[16], cex.cor = TRUE, col = pal_my[17])
# med.n <- within(med.n, {
	# X <- X - mean(X)
	# M <- M - mean(M)
# })
#'
#'
#+ dat1Hists, fig.margin=TRUE, echo=FALSE
palette(pal_my)
bcol <- pal_my[c(16, 13)] %>% sapply(adjustcolor, alpha.f = 0.85)
barplot(table(med.n$X), xlab = " ", col = bcol,
		main = "Mother's Race",
		names.arg = c("\n0\n(Non-White)", "\n1\n(White)"))

library(MASS)
truehist(med.n$M, xlab = " ", freq = FALSE, col = mpal(seq(1:9), a = 0.75),
     main = "Child neglect reports"); lines(density(med.n$M),
                                                      col = 19, lwd = 2.5)

barplot(table(med.n$Y), xlab = " ", col = bcol,
		main = "Any Abuse Reported",
		names.arg = c("\n0\n(No)", "\n1\n(Yes)"))
#'
#'
#+ interactionPlot, echo=FALSE, eval=FALSE
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
#+ m1, echo=TRUE
library(lavaan) ## "lavaan()", "sem()" ###'
#'
#+ m2, echo=TRUE
m2 <- '#Y ~ c*X
       M ~ a*X
       Y ~ b*M + c*X
       ab := a*b
       total := c + (a*b)'
fit2 <- sem(m2, data = med.n,
        estimator = "ML", #"ML", ## Maximum likelihood estimator
                            ## yeilds the same results as SAS ##
        link = "logit", ## ML only works with the probit link ##
        meanstructure = TRUE,
        se = "bootstrap",
        std.ov = F,
        information = "expected")
        # ordered = c("X", "Y"))
summary(fit2)
## "If you prefer to use an unbiased covariance, and n - 1 as the
    ## multiplier to compute the chi-square statistic"
    ## --- Roseel (2012), p. 27 ##
#'
#+ fit2
## Model fit stats ##
fit2.fits <- fitMeasures(fit2)[c('aic', 'bic')]
fit2.llr <- logLik(fit2)
fit2.rsq <- inspect(fit2, 'r2')

## Model summary stats ##
fit2.mus <- fitted(fit2)$mean
fit2.cov <- fitted(fit2)$cov
fit2.cor <- cov2cor(fit2.cov)

fit2.bci <- parameterEstimates(fit2, level = 0.95, boot.ci.type = "bca.simple")

fit2.par <- parameterEstimates(fit2) ## Y ~ 1 == model intercept ##
    ## Below is for formatting purposes ##
params2 <- matrix(paste0(fit2.par[, 1], " ",
                           fit2.par[, 2], " ",
                           fit2.par[, 3]))
fit2.par <- cbind(params2, fit2.par[, c(-1:-4)]) %>% data.frame()

fit2.se <- cbind(params2, fit2.par$se)
fit2.coefs <- cbind(params2, fit2.par$est)

fit2.int <- fit2.par[7, 2] ## b0 ##

fit2.b <- fit2.par[3, 2] ## Y ~ M (b2) ##
fit2.a <- fit2.par[2, 2] ## M ~ X ##
fit2.c <- fit2.par[1, 2] ## Y ~ X (b1) ##
fit2.ab <- fit2.par[10, 2] ## Y ~ X*M (b3) ##
#'
#'
#' ## Mediation Model Summary Statistics and Model Fit Indices
#'
#+ fit2Kabs, echo=FALSE
fit2.par[, -1] <- apply(fit2.par[, -1], 2, round, digits = 4)
fit2.par <- apply(fit2.par, 2, R.na, v = " ")

labs <- c("Parameter", "Estimate", "SE", "Z", "P-Value", "$CI_{lower}$", "$CI_{upper}$")

kable(
    fit2.par,
    digits = 2,
    caption = "Model 2 (Mediation Model) Summary Statistics",
    align = c("c", rep("r", ncol(fit2.par))), col.names = labs
)



kable(fit2.rsq, caption = "Model 2 $R\\sq$")

fit2.cov[upper.tri(fit2.cov)] <- 0
kable(fit2.cov, caption = "Model 2 Covariance Matrix",
      format.args = list(zero.print = " "))
kable(fit2.mus, caption = "Model 2 Coefficients' Means")

# fit2.llr
names(fit2.fits) <- c("AIC", "BIC")
kable(fit2.fits, caption = "Absolute Fit Indices")

anova(fit2)
#'
#' $$ M = a_{0} + aX + e+_{M} $$
#' $$ \logit(Y) = b_{0} + bM + c′X + e_{y} $$
#' $$ logit(\hat{Y}) = `r fit2.intercept`\beta_{0} + `r fit2.X`\beta_{1_{M}} + `r fit2.M`c'{X}} $$
#'
#+ fit2pathMod, echo=FALSE, fig.cap="Fitted Model - Partial Mediation Effect Observed. '*': p < 0.05, '**': p < 0.01, '***': p < 0.001"
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
aPath <- paste0("a = ", fit2.par[2, 2], "***")
bPath <- paste0("b = ", fit2.par[3, 2], "*")
ab <- paste0("[a X b] =", fit2.par , "*")
cPath <- paste0("c' = ", fit2.par[1, 2], "***")
txt.x = list(aPath = 0.17, bPath = 0.82, ab = 0.5, cPath = 0.5)
txt.y <- list(aPath = 0.5, bPath = 0.5, ab = 0.4, cPath = 0.19)
R.mpathDia(iv, mv, dv, arrcol = 19, lty.c = 1); text(x = txt.x, y = txt.y,
                                                     labels = c(aPath,
                                                                bPath,
                                                                ab,
                                                                cPath),
                                                     font = 3)

#'
#'
b <- fit2.par[3, 2] ## Y ~ M (b2) ##
se.b <- fit2.par[3, 3] ## Y ~ M (b2) ##
a <- fit2.par[2, 2] ## M ~ X ##
se.a <- fit2.par[2, 3] ## M ~ X ##
c <- fit2.par[1, 2] ## Y ~ X (b1) ##
se.c <- fit2.par[1, 3] ## M ~ X ##
ab <- fit2.par[10, 2] ## Y ~ X*M (b3) ##
mu.y <- fit2.mus[[1]]
mu.x <- fit2.mus[[3]]
mu.m <- fit2.mus[[2]]
library(RMediation)
medci(mu.x = mu.x, mu.y = mu.y, se.x = se.a, se.y = se.b, rho = fit2.cor[3, 1], alpha = .05, type = "MC", n.mc = 5000, plot = TRUE, plotCI = TRUE)
#'
#' `r tufte::newthought("Mediation Analysis Summary.")` Two regression models were tested to investigate whether the association between mother's race (white/non-white) and any reported abuse (yes/no) is mediated by the number of child neglect reports. The hypothesized mediator variable, number of neglect reports, was first regressed on the mother's race (the hypothesized predictor variable), and in the second model any reported abuse was regressed on both the count of neglect reports and the mothers race variable. In the first regression model, mother's race was significantly related to higher counts of reported neglect, $b = 0.082, SE = .036, p < .05, 95\% CI = .0103, .1553$. In the model, both the mother's race, $b = .045, SE = .015, p = < .01, 95\% CI .016, .013$, and counts of neglect reports, $b = .071, SE = .023, 95\% CI = .031, .121$, were significantly associated whether there were any reports of abuse. In addition, the indirect effect was marginally significant, $b = .006, SE = .003, p = 0.064$, with the boostrap confidence intervals derived from 1000 samples not including 0 ($95\% CI = .0006, .0127$). Taken together, however, these regression analyses do not support the mediation hypothesis regarding the relations among the variables tested, as the hypothesized mediating variable's effect was not influentual on the relationship between the hypotehsized predictor and the outcome.

#'
#'
#+ medCor, fig.margin=TRUE
# X <- med.n[, "X"]
# M <- med.n[, "M"]
# Y <- med.n[, "Y"]

# ab <- cor(X, M) ## X <-> M ##
# ac <- cor(X, Y) ## X <-> Y ##
# bc <- cor(M, Y) ## M <-> Y ##

# medCor <- cor(med.n)
#'
#'
#+ medCorKab, echo=FALSE
# library(lattice)
# palette(pal_sci)
# grays <- colorRampPalette(pal_my[c(1, 16, 18)], alpha = T)
# levelplot(abs(medCor),
		  # col.regions = grad(seq(length(medCor)), p = grays),
		  # xlab = "", ylab = "",
		  # scales = list(alternating = 3, cex = 1.25),
		  # cuts = length(medCor),
		  # at = seq(range(medCor)[1], range(medCor)[2],
		           # length.out = length(medCor)),
		  # colorkey = list(axis.text = list(cex = 1),
		                  # tick.number = 9, width = 1.25))

# medCor <- round(medCor, 2)
# medCor[upper.tri(medCor, diag = TRUE)] <- " "
# diag(medCor) <- rep("1", 3)
# kable(medCor, caption = "Correlation Matrix for Mediation Model Variables",
      # digits = 2, format.args = list(zero.print = " "))
#'
#' \newpage
#' # Fit-1 (Don't need)
#'
#'
m1 <- '## Direct Effect on Y ##
       Y ~ c*X'

fit1 <- sem(m1, data = med.n,
        # estimator = "ML", ## Maximum likelihood ("ML") estimator
                            ## yeilds the same results as SAS ##
        link = "logit", ## ML only works with the probit link ##
        information = "observed",
        mimic = "Mplus",
        meanstructure = TRUE,
        fixed.x = TRUE,
        se = "bootstrap",
        bootstrap = 5000L,
        likelihood = "wishart")
#'
#'
#+ fit1
fit1.par <- parameterEstimates(fit1, standardized = FALSE) ## Y ~ 1 == model intercept ##
fit1.fitted <- fitted(fit1)
fit1.cov <- fitted(fit1)$cov
fit1.mus <- fitted(fit1)$mean
fit1.llr <- logLik(fit1)
fit1.fits <-
    fitMeasures(fit1)[c('ntotal',
                        'chisq',
                        'df')]
fit1.rsq <- inspect(fit1, 'r2')
fit1.mod <- modificationindices(fit1)
#'
#'
#'
#+ fit1kab, echo=FALSE
fit1.par2 <-
    matrix(paste0(fit1.par[, 1], " ", fit1.par[, 2], " ", fit1.par[, 3]))
fit1.par <- cbind(fit1.par2, fit1.par[, c(-1, -2, -3, -4)])
colnames(fit1.par) <-
    c("Parameter",
      "Estimate",
      "SE",
      "Z",
      "P-Value",
      "$CI_{lower}$",
      "$CI_{upper}$")
fit1.par[, -1] <- apply(fit1.par[, -1], 2, round, digits = 2)
fit1.par <- apply(fit1.par, 2, R.na, v = " ")

kable(fit1.par, caption = "Model 1 (Direct Effect Model) Summary Statistics", align = c("c", rep("r", ncol(fit1.par))))
#'
#'
#+ fit1pathMod, echo=FALSE, fit.margin=TRUE, fig.cap="Observed Direct Effect. '**': p < 0.01"

library(pathdiagram)
palette(pal_my)

iv <- list(X <- manifest("White\n(X)", x = 0, y = 0.5,
                         width = 0.2, height = 0.1, cex = .75,
                         fill = NA, col = 16, lwd = 1, border = 16))
dv <- list(Y <- manifest("Abuse\n(Y)", x = 1, y = 0.5,
                         width = 0.2, height = 0.1, cex = .75,
                         fill = NA, col = 5, lwd = 1, border = 5))
XY <- paste0("c = ", fit1.par[1, 2], "**")
txt.x = 0.5
txt.y <- 0.45

R.pathDia(manifest = iv, latent = dv, arrcol = 19, lty = 1); text(x = txt.x, y = txt.y, labels = XY)
#'
#+ fit2hypMod, echo=FALSE, fit.margin=TRUE, fig.cap="Hypothesized Mediation Model."
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
aPath <- "a*"
bPath <- "b*"
ab <- "[a X b]*"
cPath <- "c' (n.s.)"
txt.x = list(aPath = 0.23, bPath = 0.82, ab = 0.51, cPath = 0.5)
txt.y <- list(aPath = 0.5, bPath = 0.5, ab = 0.4, cPath = 0.2)
R.mpathDia(iv, mv, dv, arrcol = 19); text(x = txt.x, y = txt.y,
                                                     labels = c(aPath,
                                                                bPath,
                                                                ab,
                                                                cPath),
                                                     font = 3)
library(semPlot) ## "semPaths()" (& "semCors()") ##
semPaths(fit2, "est", "par",
         edge.color = pal_my[19],
         border.color = pal_my[19],
         edge.label.cex = 1,
         normalize = F,
         rotation = 1,
         fade = F,
         intercepts = FALSE,
         threshAtSide = F,
         edge.width = 0.25,
         centerLevels = F,
         springLevels = T)
#' `r tufte::newthought("Sidenote:")` As you have probably realized about me at this point, I routinely run the "`interactionPlot()`" function (along with several other descriptive plotting and tabulating functions) as part of my data visualization workflow in preparation for conducting hypothesis testing.
#'
#'

with(med.n, {
    interaction.plot(X, M, Y, ## {pkg:stats} ##
        trace.label = "Neglect", fixed = TRUE,
        col = mpal(seq(1:length(unique(med.n$M))), p = sci), lwd = 2,
        main = "Interaction Effects Implied by Variable Correlations",
        ylab = expression(mu[Abuse]), xlab = "White")
})
#' $$ M = a_{0} + aX + e+_{M} $$
#'
#' $$ logit(Y) = b_{0} + bM + c'X + e_{y} $$
#'
