#' ---
#' title: "Interactions with Logistic Regression"
#' author: "Riley M. Smith"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#'
#+ setup, echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE}
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
#' -----
#' 
#' # Data Descriptions \& Preparation for Analysis`r tufte::margin_note("\\textbf{\\texttt{data/cnnpoll.sav}}, \Rhref{http://www.ropercenter.cornell.edu/CFIDE/cf/action/catalog/abstract.cfm?label=&keyword=USORCCNN2015+008&fromDate=&toDate=&organization=Any&type=&keywordOptions=1&start=1&id=&exclude=&excludeOptions=1&topic=Any&sortBy=DESC&archno=USORCCNN2015-008&abstract=abstract&x=32&y=11}{Source}")`
#'
#'
#+ dat
dat <- R.rspss("data/cnnpoll.sav")
R.msmm(dat)
#'
#' 
#' ## Data Cleaning \& Preparation
#'
#' `r tufte::newthought("Remove \\texttt{NAs}")`
#'
dat <- na.omit(dat)
#'
#' -----
#' 
#' `r tufte::newthought("Adjust Scales")`
#' 
#' If you look at the output above, you may notice there's something slightly off in how the data are coded: The "`ind`" variable uses the typical "`0-1`" binary data coding scheme, while the "`sex`" \& "`response`" variables use a "`1-2`" coding scheme. This was probably done somewhere in the variable definitions in `SPSS`, but I suspect this is the cause of some mismatch between _`GLM`_ outputs from `R` and `SPSS`. To fix the coding, I'm just going to create and run a convient little function to dichotomize the numeric data on a "`0-1`" scale. 
#'
#'
#+
dat$ind <- dat$ind + 1

R.dich<-function(x, min = 1, values = c(0, 1)) { ## Function for dichotomizing numeric data ##
    if(is.numeric(x)) {
        x[x <= min] <- values[1]
        x[x > min] <- values[2]
        }
    else
        stop('x must be numeric')
    return(x)
}
dat <- within(dat, { ## Apply "R.dich()" on the variables with the wrong coding scheme ##
	response <- R.dich(response)
	sex <- R.dich(sex)
	ind <- R.dich(ind)
})
#'
#'
#' -----
#' 
#' `r tufte::newthought("Create Factored \\& Numeric Variables")` for the numeric and factored dataframes, respectively (i.e., create factors in "`dat`" and numerics in "`dat.o`" - see above).
#'
dat <- within(dat, {
	response.f <- factor(response, labels = c("Clinton", "Trump"))
	sex.f <- factor(sex, labels = c("Male", "Female"))
	ind.f <- factor(ind, labels = c("Affiliate", "Independent"))
})
#'
#'
#' -----
#' 
#' ## Summary of Cleaned Data
#'
#'
#+ cleanSum, Rplot=FALSE
R.msmm(dat)
kable(as.matrix(summary(dat[, 4:6])))

dat.ft <- with(dat, {ftable(response, sex, ind, row.vars = 2:3, col.vars = 1)}) 

dat.fta <- dat.ft %>% array(dim = c(2, 2, 2))

dimnames(dat.fta) = list(Affiliation = c("Party Affiliate", "Independent"), Sex = c("Male", "Female"), Response = c("Clinton", "Trump"))
dat.fta

palette(pal_my.a75); par(mgp = c(3, 1, -0.5), tck = NA, family = "Serif")

hist(dat$ind, freq = FALSE, 
	 xlab = " ", 
	 col = c(16, 13), 
	 border = NA, main = "Party Affiliation", 
	 axes = FALSE); axis(1, at = c(0.05, 0.95), 
				        labels = c("\n0\n(Party Affiliate)", "\n1\n(Independent)"), 
				        tick = FALSE); lines(density(dat$ind), 
				        					col = 18, lwd = 2.5)
hist(dat$sex, freq = FALSE, 
	 xlab = " ", 
	 col = c(16, 13), 
	 border = NA, main = "Sex", 
	 axes = FALSE); axis(1, at = c(0.05, 0.95), 
				        labels = c("\n0\n(Male)", "\n1\n(Female)"), 
				        tick = FALSE); lines(density(dat$ind), 
				        					col = 18, lwd = 2.5)
hist(dat$response, freq = FALSE, 
	 xlab = " ", 
	 col = c(16, 13), 
	 border = NA, main = "Polling Response", 
	 axes = FALSE); axis(1, at = c(0.05, 0.95), 
				        labels = c("\n0\n(Clinton)", "\n1\n(Trump)"), 
				        tick = FALSE); lines(density(dat$ind), 
				        					col = 18, lwd = 2.5)
#'
#' 
#' \newpage
#' 
#' 
#' # Data Analysis - Logistic Regression with an Interaction Term
#' 
#' ## Using `glm()` `{pkg:stats}`
#'
glm1 <- glm(response ~ ind*sex, data = dat, family = binomial(link = "logit"), x = TRUE)
glm1
anova(glm1)
glm1.y <- data.frame(glm1$y)
glm1.x <- data.frame(glm1$x)
glm1.coef <- glm1$coefficients
glm1.var <- sqrt(abs(var(glm1.x[, -1])))
kable(glm1.var, caption = "Covariance Matrix among Model Predictors")


dat$ind.sex <- dat$ind*dat$sex
dat.var <- sqrt(abs(var(dat[, c(3, 2, 7)])))
kable(glm1.var, caption = "Raw Covariance Matrix among Model Predictors")

b1.var <- mean(glm1.x$ind)*(nrow(glm1.x) - mean(glm1.x$ind))/nrow(glm1.x) ## "ind" ##
b2.var <- mean(glm1.x$sex)*(nrow(glm1.x) - mean(glm1.x$sex))/nrow(glm1.x) ## "sex" ##
b3.var <- mean(glm1.x$ind.sex)*(nrow(glm1.x) - mean(glm1.x$ind.sex))/nrow(glm1.x) ## "ind*sex" ##

b.vars <- c(b1.var, b2.var, b3.var)
R.sqrt <- function(x){x.a <- abs(x); sqrt(x.a)}
b.sd <- apply(glm1.x, 2, sd)
b.se <- sqrt(b.sd)

dat.ftm <- matrix(dat.fta, nrow = 4, byrow = F)
colnames(dat.ftm) <- c("Clinton", 
					   "Trump")
rownames(dat.ftm) <- c("Male.Affiliate", 
					   "Male.Independent", 
					   "Female.Affiliate", 
					   "Female.Independent")

ind <- matrix(rep(c(0, 1), 2))
sex <- matrix(c(0, 0, 1, 1))
dat.ftm2 <- data.frame(sex, ind, dat.ftm)
dat.ftm2 <- within(dat.ftm2, {
	sex <- factor(sex, levels = c(0, 1), labels = c("Male", "Female"))
	ind <- factor(ind, levels = c(0, 1), labels = c("Affiliate", "Independent"))
})
kable(dat.ftm2)


mod <- "cbind(Clinton, Trump) ~ ind*sex"
glm2 <- glm(mod, family = binomial("logit"), data = dat.ftm2)
summary(glm2)
anova(glm2, test = "Chisq")

dat.ftm3 <- matrix(dat.fta)
dat.ftm4 <- data.frame(sex, ind, dat.ftm2)
colnames(dat.ftm2) <- c("Sex", "Affiliation", "Count")
rownames(dat.ftm2) <- c("Male-Affiliate-Clinton", 
					    "Male-Independent-Clinton", 
					    "Female-Affiliate-Clinton", 
					    "Female-Independent-Clinton",
					    "Male-Affiliate-Trump", 
					    "Male-Independent-Trump", 
					    "Female-Affiliate-Trump", 
					    "Female-Independent-Trump")
# plot(glm2$fitted); lines(dat.ftm2$, glm.out$fitted, type="l", col="red")

#'
#' -----
#'
#' ## Using `lm()` `{pkg:stats}`
#'
lm1 <- lm(response ~ ind*sex, data = dat)
lm1
anova(lm1)
aov1 <- aov(response ~ ind*sex, data = dat)
aov1

library(semPlot)
semPaths(lm1, edge.color = pal_my[19], edge.label.cex = 1.5)
library(MASS)
llm <- loglm(response ~ ind + sex + ind*sex, data = dat)
llm
#'
#' -----
#'
#' ## Using `lavaan()` `{pkg:lavaan}`
#'
library(lavaan)
dat$ind.sex <- dat$ind*dat$sex
lmod <- ' 
		 response.f ~ ind + ind.sex
		'
lmod.fit <- sem(lmod, data = dat, link = "logit", test = "bootstrap")
summary(lmod.fit)
semPaths(list(lm1, lmod.fit), edge.color = pal_my[19], edge.label.cex = 1.5)
#'
#'
#' -----
#' 
#' \newpage
#' # Datadump
dat.o <- read.spss("data/cnnpoll.sav", to.data.frame = TRUE)
summary(dat.o)
dat.o <- na.omit(dat.o) %>% droplevels
summary(dat.o)

dat.o <- within(dat.o, {
	response.n <- as.numeric(response)
	sex.n <- as.numeric(sex)
	ind.n <- as.numeric(ind)
})
kable(R.msmm(dat.o[, 4:6])[, -5])
#'
#' -----
#'
#+ centerVars, echo=FALSE, eval=FALSE
#Center Predictor Variables ## < NOT FOR GLM ! > ##
# indMu <- mean(dat$ind)
# sexMu <- mean(dat$sex)
# respMu <- mean(dat$response)

# dat <- within(dat, {
	# response.c <- response - respMu
	# ind.c <- ind - indMu
	# sex.c <- sex - sexMu
# })

#'