#'
#'
#'  -----
#'
#' ## Using `lavaan()` `{pkg:lavaan}`
#'
#' `r tufte::newthought("Mediation")`
#' 
library(lavaan) 
m1 <- ' ## direct effect ##
		response ~ c*ind ## the "a*" component is the path label ##
		## mediation effect ##
		sex ~ a*ind
		response ~ b*sex
		## indirect effect ##
		ab := a*b
		## total effect ##
		total := c + (a*b)
		'
fit1 <- sem(m1, data = dat)summary(fit1)
semPaths(fit1, "path", c("par"), intercepts = TRUE, layout = "tree2", edge.color = pal_my[19], edge.label.cex = 1.5)
#'
#' `r tufte::newthought("Moderation")`
#'
#'
dat$XW <- dat$ind*dat$sex
m2 <- ' ## direct effect ##
		response ~ a*ind #+ b*XW
		## indirect effect ##
		total := a + (b*XW)
		'
fit2 <- sem(m2, data = dat)summary(fit2)
semPaths(fit2, "path", c("par"), intercepts = TRUE, edge.color = pal_my[19], edge.label.cex = 1.5)

dat$XW <- dat$ind*dat$sex
m3	<-	'response ~ c*ind + b*sex
  		 sex ~ a*ind + aw*XW
		 ## indirect and total effects, conditional on W == 0
		 total0 := c + a*b
		 '
		 #		 ## indirect and total effects, conditional on W == 1
		 #ab1 := ab0 + 1*aw*b
		 #total1 := ab1 + c + 1*cw
		 ## indirect and total effects, conditional on W == 2
		 #ab2 := ab0 + 2*aw*b
		 #total2 := ab2 + c + 2*cw


fit3 <- sem(m3, data = dat)summary(fit3)
semPaths(fit3, "mod", "label", intercepts = F, edge.color = pal_my[19], edge.label.cex = 1.5)

#' -----
#' 
#' Plotting with the `{semPlot}` package
#'
library(semPlot)
# A silly dataset:
Y <- rnorm(100) #outcome (A = Y)
Z <- Y + rnorm(100) #mediator (B = Z)
X <- Z + rnorm(100) #predictor (C = X)
XZ <- X*Z

DF <- data.frame(X, Y, Z, XZ)

# Two regressions:
res1 <- lm(Z ~ X, data = DF)
res2 <- lm(Y ~ Z + X, data = DF)
res3 <- lm(Y ~ Z + X + XZ, data = DF)

# Plot both in the same path diagram in two ways:
semPaths(list(res1, res2), "model", "est", intercepts = F, edge.color = pal_my[19], edge.label.cex = 1.5)
semPaths(list(res1, res3), "model", "est", intercepts = F, edge.color = pal_my[19], edge.label.cex = 1.5)
#'
#' Likelihood-Ratio Test Statistic for IxJ tables^[Source: [Jason Newsom]()]

LRstats=function(data)
{
	G2=2*sum(data*log(data/chisq.test(data)$expected))
 	G2pvalue=1-pchisq(G2,df=(dim(data)[1]-1)*(dim(data)[2]-1))
 	ans=c(G2,G2pvalue)
 	ans
 }
 

# download.file("http://www.statmodel.com/usersguide/chap5/ex5.8.out",
#               outfile <- tempfile(fileext = ".out"))
# Plot model:
# semPaths(outfile, intercepts = FALSE)
#' 
#' \newpage
#' 
#' # Simulate Data Based on SEM Coefficients
#'
#'
library(lavaan)
modsim <- 
		   '
            f1 =~ 0.5*y1 + 0.5*y2 + 0.5*y3
            f2 =~ 0.5*y4 + 0.5*y5 + 0.5*y6
            f3 =~ 0.5*y7 + 0.5*y8 + 0.5*y9
            f4 =~ 0.5*y10 + 0.5*y11 + 0.5*y12
            f5 =~ 0.5*y13 + 0.5*y14 + 0.5*y15
            
            f1 ~~ 0.4*f2 + 0.4*f3
            f2 ~~ 0.4*f3
            f4 ~~ 0.4*f5
            
            f4 ~ 0*f1 + 0.6*f2 + 0.5*f3
            f5 ~ 0.6*f1 + 0*f2 + 0*f3
            
            f1 ~ 0*y16 + 0.6*y17 + 0.2*y18
            f2 ~ 0.5*y16 + 0*y17 + 0.2*y18
            f3 ~ 0.5*y16 + 0.6*y17 + 0.2*y18
            f4 ~ 0.5*y16 + 0*y17 + 0.2*y18
            f5 ~ 0*y16 + 0*y17 + 0.2*y18
           '
datsim <- simulateData(modsim, std.lv = T, meanstructure = T,
                     sample.nobs = 300, seed = 42)
# datsim2 <- datsim <- simulateData(modsim, std.lv = F, meanstructure = F,
#                      sample.nobs = 300, seed = 42, standardized = TRUE) 
## ^returns error^
datsim2 <- datsim <- simulateData(modsim, std.lv = T, meanstructure = T,
                     sample.nobs = 300, seed = 42, empirical = TRUE)
kable(R.msmm(datsim)[, -5], caption = "Simulated Dataset 1 Summary")
kable(R.msmm(datsim2)[, -5], caption = "Simulated Dataset 2 Summary") 
	## no diff when using "empirical = TRUE" ##

sim.fit <- sem(modsim, data = datsim)
semPaths(sim.fit, edge.color = pal_my[19], edge.label.cex = 1.5)
#'
#' \newpage
#' # Simple Mediation^[Rosseel (2012)]
#'
X <- rnorm(100)
M <- 0.5*X + rnorm(100) 
Y <- 0.7*M + rnorm(100)

dat <- data.frame(X = X, Y = Y, M = M)

md <- ' # direct effect              Y ~ b*X            # mediator              M ~ a*X            # indirect effect (a*b)              ab := a*b            # total effect            #  total := c + (a*b)
       '
fit <- sem(md, data = dat)
summary(fit)
semPaths(fit, edge.color = pal_my[19], edge.label.cex = 1.5)
#'
#' Transforming Rosseel's Model into a Moderation Model

X <- rnorm(100)
M <- 0.5*X + rnorm(100) 
Y <- 0.7*M + rnorm(100)

dat <- data.frame(X = X, Y = Y, M = M)

md <- ' # direct effect              Y ~ c*X            # mediator              M ~ a*X              Y ~ b*M            # indirect effect (a*b)              ab := a*b            # total effect              total := c + (a*b)
       '
fit <- sem(md, data = dat)
summary(fit)
semPaths(fit, edge.color = pal_my[19], edge.label.cex = 1.5)


#'
#' \newpage
#' 
#' # Simple Multivariate Logistic Regression
#'
#' 
dat <- R.rspss("data/child2.sav") ## numeric variables ##
dat <- na.omit(dat)
dat.c <- within(dat, {## could also do this with an "apply()" function ##
    boyfriend <- boyfriend - mean(boyfriend)
    program <- program - mean(program)
    welfare <- welfare - mean(welfare)
    white <- white - mean(white)
})
vars <- list(y = "abuse", x1 = "boyfriend", x2 = "program", x3 = "welfare", x4 = "white")

names(dat) <- names(vars)
names(dat.c) <- names(vars)
R.msmm(dat)
R.msmm(dat.c)

library(lavaan) ## "sem()" ##
m1 <-  'y ~ x1 + x2 + x3 + x4'
m1.fit <- sem(model = m1, data = dat, )
m1c.fit <- sem(model = m1, data = dat.c)
summary(m1.fit)
summary(m1c.fit)

cov(mod)
#'
#' \newpage
#' Manual SEM Specficiation Example[^By [Sacha Epskamp (2014)](http://www.sachaepskamp.com/files/semPlot.pdf).]
#'
mod <- specify.model()
 1: AF -> D, NA, 1
 2: STW -> D, NA, 1
 3: D -> MFIPV, lam1, NA
 4: AF <-> STW, psi1, NA
 5: STW <-> AF, psi2, NA


L <- matrix( c(    "y1", "y2", "y3", "y4",    "y5", "y6", "y7",   "y8",    "x1", NA,   NA,   "dem60", NA,    NA,  "dem65", NA,    "x2", NA,   NA,   "ind60", NA,    NA,   NA,     NA,    "x3", NA,   NA,    NA,     NA ,   NA,   NA,     NA),,4 )

input<- matrix(1,3,3) 
print(input)
library(qgraph)
qgraph(input)


#' # Formulae - Logistic Regression with an Interaction Term
#'
#' ## Odds, Odds Ratios, and Logit
#'
#' $$ odds = \frac{p(success)}{p(failure)} \longRightArrow \frac{p}{q} $$
#'
