#' ---
#' title: "Homework 3, Question 1a (lavaan package)"
#' author: "Riley M. Smith"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#'
#+ setup, echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE
source("../SETUP.R")
knitr::opts_chunk$set( tidy = FALSE, echo = TRUE, cache = FALSE, fig.keep = 'high', fig.show = 'hold', results = 'asis', autodep = TRUE, Rplot = TRUE, dev = 'pdf', fig.path = 'graphics/HW3/rplot-q1-', fig.width = 7, fig.height = 7, out.width = "\\linewidth"
)
#'
#' # Q1.a Data
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
#'
#+ echo=FALSE
kable(as.matrix(summary(dat[, 6:9])))
#'
#' # Mediation Model Data
#'
#'
#+ medDat
med.n <- dat[, c(1, 2, 5)] ## Mediator variables as numeric ##
med.n <- na.omit(med.n)
names(med.n) <- c("Y", "M", "X")
#'
#+ datHists, fig.margin=TRUE, echo = FALSE
palette(pal_my)

hist(med.n$X, xlab = " ", col = c(16, 13), main = "Mother's Race", axes = FALSE); axis(2); axis(1, at = c(0.05, 0.95), labels = c("\n0\n(Non-White)", "\n1\n(White)"), tick = FALSE)

hist(med.n$M, xlab = " ", col = c(16, 13), main = "Boyfriend is Biological Father", axes = FALSE); axis(2); axis(1, at = c(0.05, 0.95), labels = c("\n0\n(No)", "\n1\n(Yes)"), tick = FALSE)

hist(med.n$Y, xlab = " ", col = c(16, 13), main = "Any Abuse Reported", axes = FALSE); axis(2); axis(1, at = c(0.05, 0.95), labels = c("\n0\n(No)", "\n1\n(Yes)"), tick = FALSE)
#'
#+ interactionPlot
with(med.n, {
    interaction.plot(Y, M, X, 
    col = c("darkgray", pal_my[5]), lwd = 2, 
    main = "Interaction Effects Implied by Variable Correlations",
    ylab = expression(mu[White]), xlab = "Abuse", 
    trace.label = "Boyfriend")
    })
#'
#' \newpage
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

ab <- cor(X, M) ## X <-> M ##
ac <- cor(X, Y) ## X <-> Y ##
bc <- cor(M, Y) ## M <-> Y ##

medCor <- cor(med.n)
#'
#'
#+ echo=FALSE
kable(medCor, caption = "Correlation Matrix for Mediation Model Variables", digits = 2)
#'
#'
#+ echo=FALSE
library(lattice)
palette(pal_my)

grays <- colorRampPalette(pal_my[c(1, 18, 20)], alpha = T)

levelplot(medCor, 
		  col.regions = grad(seq(length(medCor)), p = grays), 
		  # xlab = "", ylab = "", 
		  cuts = length(medCor), 
		  at = seq(range(medCor)[1], range(medCor)[2], length.out = length(medCor)), 
		  colorkey = list(axis.text = list(cex = 0.75), tick.number = 3, width = 1))
#' 
#'
#' -----
#'
#' \newpage
#' ## Using `lavaan()` `{pkg:lavaan}`
#'
library(lavaan) ## "lavaan()", "sem()", & "simulateDate()" ## 
library(semPlot) ## "semPaths()" & "semCors()" ##

m1 <- '## Direct Effect on M ##
	   # M ~ X
	   ## Direct Effect on Y ##
	   Y ~ c*X
	  '

fit1 <- sem(m1, data = med.n, estimator = "ML", link = "probit", information = "observed", mimic = "Mplus", meanstructure = TRUE, fixed.x = TRUE, se = "standard")

fit1.est <- parameterEstimates(fit1) ## Y ~ 1 == model intercept ## 
fit1.est <- parameterEstimates(fit1) ## Y ~ 1 == model intercept ##
fit1.est2 <- matrix(paste0(fit1.est[, 1], " ", fit1.est[, 2], " ", fit1.est[, 3]))
fit1.est <- cbind(fit1.est2, fit1.est[, c(-1, -2, -3, -4)])
colnames(fit1.est) <- c("Parameter", "Estimate", "SE", "Z", "P-Value", "$CI_{lower}$", "$CI_{upper}$")
fit1.est[, -1] <- apply(fit1.est[, -1], 2, round, digits = 2)
fit1.est <- apply(fit1.est, 2, R.na, v = " ")
kable(fit1.est, caption = "Model 1 (Direct Effect Model) Summary Statistics", align = c("c", rep("r", ncol(fit2.est))))

fit1.fitted <- fitted(fit1)
fit1.fits <- fitMeasures(fit1)[c('ntotal', 'chisq', 'df', 'aic', 'bic', 'cfi', 'tli', 'rmsea', 'srmr')]
fit1.rsq <- inspect(fit1, 'r2')
fit1.mod <- modificationindices(fit1)

semPaths(fit1, "est", edge.color = pal_my[19], border.color = pal_my[19], edge.label.cex = 1, normalize = F, rotation = 1, fade = F, intercepts = FALSE, threshAtSide = F, edge.width = 0.25, curve = 4)

m2 <- '## Mediator ##
	   M ~ a*X
	   ## Mediation Model ##
	   Y ~ b1*X + b2*M
	  '

fit2 <- sem(m2, data = med.n, estimator = "ML", link = "probit", information = "observed", mimic = "Mplus", meanstructure = TRUE, fixed.x = TRUE, se = "standard")

fit2.est <- parameterEstimates(fit2) ## Y ~ 1 == model intercept ##
fit2.est2 <- matrix(paste0(fit2.est[, 1], " ", fit2.est[, 2], " ", fit2.est[, 3]))
fit2.est <- cbind(fit2.est2, fit2.est[, c(-1, -2, -3, -4)])
colnames(fit2.est) <- c("Parameter", "Estimate", "SE", "Z", "P-Value", "$CI_{lower}$", "$CI_{upper}$")
fit2.est[, -1] <- apply(fit2.est[, -1], 2, round, digits = 2)
fit2.est <- apply(fit2.est, 2, R.na, v = " ")
kable(fit2.est, digits = 2, caption = "Model 2 (Mediation Model) Summary Statistics", align = c("c", rep("r", ncol(fit2.est))))

fit2.fitted <- fitted(fit2)
fit2.fits <- fitMeasures(fit2)[c('ntotal', 'chisq', 'df', 'aic', 'bic', 'cfi', 'tli', 'rmsea', 'srmr')]
fit2.rsq <- inspect(fit2, 'r2')
fit2.mod <- modificationindices(fit2)
#'
#+ echo=FALSE
semPaths(fit2, "est", "par", edge.color = pal_my[19], border.color = pal_my[19], edge.label.cex = 1, normalize = F, rotation = 1, fade = F, intercepts = FALSE, threshAtSide = F, edge.width = 0.25, centerLevels = FALSE)
anova(fit1, fit2)

#'
#' <!-- ------------------------------------------------------------------------------------------ -->
#' <!-- ------------------------------------------------------------------------------------------ -->
#' <!-- ------------------------------------------------------------------------------------------ -->
#+ echo=FALSE
# semPaths(m.fit, "est", "par", posCol = pal_my[16], negCol = pal_my[5],  edge.label.cex = 1, normalize = F, # rotation = 2, fade = TRUE, intercepts = FALSE, threshAtSide = F, edge.width = 0.25, springLevels = T, exoVar = F, exoCov = F, curveAdjacent = FALSE, # style = "openMX", # layout = "tree3",  # XKCD = T,  borders = F) 

# semPaths(m.fit, "par", posCol = pal_my[16], negCol = pal_my[5], normalize = FALSE, intercepts = FALSE, edge.width = 0.5, exoVar = F, exoCov = F, style = 'lisrel', covAtResiduals = FALSE, residuals = FALSE, layout = 'tree', curveAdjacent = FALSE, curve = -2, springLevels = TRUE)#, XKCD = T

# fit.p <- semPlotModel(m.fit)
# semCors(m.fit, posCol = pal_my[16], negCol = pal_my[5], normalize = FALSE, fade = TRUE, XKCD = TRUE)#, layout = 'trees2') ## NOT INFORMATIVE ##

# m.model <- '# direct effect              # Y ~ c*X            # # mediator              # M ~ a*X              # Y ~ b*M            # # indirect effect (a*b)              # ab := a*b            # # total effect              # total := c + (a*b)          # '

# semSyntax(m.fit, syntax = 'sem')
# anova(d.fit, m.fit)

# springLevels = T, exoVar = F, exoCov = F, curveAdjacent = FALSE, # style = "openMX", # layout = "circle3",  # XKCD = T,  borders = F) 
# fit0 <- sem(model, data = med.f, estimator = "ML", link = "probit", orthogonal = TRUE, information = "observed", se = "standard", meanstructure = TRUE)
# summary(fit0, rsq = TRUE)

# model.constr <- ' # model with labeled parameters
                    # Y ~ b1*X + b2*M
                  # # constraints
                    # # b1 == (b2)^2
                    # # b1 > exp(b2) '

# fit <- sem(model.constr, data=med.n)
# coef(fit)
# summary(fit, fit.measures = TRUE, rsq = TRUE)

# model <- '
		  # ## Mediator ##
		  # M ~ a*X
		  # ## Indirect Effect ##
		  # Y ~ b*M
		  # Y ~ c*X
		  # ab := a*b
		  # ac := a*c
		  # bc := b*c
		  # ## Total Effect ##
		  # abc := c*(a*b)
		 # '
# layout(t(1:2))
# semPaths(fit0, "est", "par", edge.color = pal_my[19], border.color = pal_my[19], edge.label.cex = 1, normalize = F, rotation = 1, fade = TRUE, intercepts = FALSE, threshAtSide = F, edge.width = 0.25)

# d.model <- '
			# ## direct effect ##            # Y ~ c*X
            # '# d.fit <- sem(d.model, data = med.f, estimator = "DWLS", link = "probit", information = "observed", se = "standard")
# summary(d.fit)

# m.model <- '
			# ## Mediator##
			# M ~ a*X
			# '# m.fit <- sem(m.model, data = med.f, estimator = "WLS", link = "probit", information = "observed", se = "standard")#, parameterization = "theta")
# summary(m.fit)
# hist(dat$program, freq = FALSE, 
	 # xlab = " ", 
	 # col = c(16, 13), 
	 # border = NA, main = "Program Condition", 
	 # axes = FALSE); axis(1, at = c(0.05, 0.95), 
				        # labels = c("\n0\n(Control)", "\n1\n(Program)"), 
				        # tick = FALSE); lines(density(dat$program, na.rm = T), 
				        					# col = 18, lwd = 2.5)

# hist(dat$welfare, freq = FALSE, 
	 # xlab = " ", 
	 # col = 16, 
	 # border = NA, main = "Welfare", 
	 # axes = FALSE); axis(1, 
	 					 # at = c(round(min(dat$welfare), digits = 0), 
	 					 # round(mean(dat$welfare), digits = 0), 
	 					 # round(max(dat$welfare), digits = 0))); lines(density(dat$welfare), 
	 					 						  # col = 18, lwd = 2.5)
	 					 						  
# fit.boot <- sem(model, data = med.n, estimator = "ML", link = "logit", information = "observed", mimic = "Mplus", meanstructure = TRUE, fixed.x = TRUE, parameterization = "theta", se = "bootstrap")
# pander(parameterEstimates(fit.boot)) ## Y ~ 1 == model intercept ## 
# pander(fitted(fit.boot))
# pander(fitMeasures(fit.boot)[c('ntotal', 'chisq', 'df', 'aic', 'bic', 'cfi', 'tli', 'srrm')])