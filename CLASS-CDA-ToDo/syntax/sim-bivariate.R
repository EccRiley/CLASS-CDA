#' ---
#' title: "Simulation of Data following the Bivariate Normal Distribution"
#' author: "Riley Smith"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#'
#+ setup, echo=FALSE, message=FALSE, warning=FALSE, results='hide', include=FALSE, eval=TRUE
source("../SETUP.R")
#'
#' The following simple simulation demonstration was partially informed through a [related post](http://blog.revolutionanalytics.com/2016/08/simulating-form-the-bivariate-normal-distribution-in-r-1.html) by Revolution Analytics' contributor Joseph Rickert's
#' # Data Parameters
#'
#' The following parameters are from @maccullum2002
#'
N <- 50
pi0 <- 0.5
rxy <- 0.4 # rho
Mx <- 10 # mean-x
My <- 20 # mean-y
SDx <- 2 # standard dev.-x
SDy <- 4 # standard dev.-y
Vx <- SDx^2 # variance-x
Vy <- SDy^2 # variance-y
#'
#'
#' # Covariance matrix
mu <- c(Mx,My)
sigma <- matrix(c(Vx^2, SDx*SDy*rxy,
				  SDx*SDy*rxy, Vy^2), nrow = 2, byrow = TRUE)
print(sigma)

#'
#' Simulated Bivariate Normal Data
#'
library(MASS) ## for mvrnorm() and other useful functions for simulating data ##
set.seed(42)

dat <- tbl_df(mvrnorm(N, mu = mu, Sigma = sigma, empirical = TRUE)) # 'empirical' is set to true because I am re-creating a simulation in MacCullum (2002), for which they reported the means and sds used in the above-set parameters, which thus makes those parameters "empirical", rather than population estimates.

colnames(dat) <- c("X", "Y")

datCov <- cov(dat)
datCov
datCor <- cor(dat)
datCor
datMu <- apply(dat, 2, mean)
datMu
#'
#'
#' _`This next bit is just for formatting purposes`_
#'
datA <- head(dat, nrow(dat)*pi0)
datB <- tail(dat, nrow(dat)*pi0)
datkab <- cbind(datA, datB)
kable(datkab)
#'
#' Plotting the simulated data
#'
library(mixtools)
par(family = "ETBembo", pch = 20)
plot(dat$X, dat$Y, xlab = "X", ylab = "Y", ylim = c(min(dat$Y)-5, max(dat$Y)+2), xlim = c(min(dat$X)-5, max(dat$X)+2))
ellipse(datMu, datCov, alpha = .05, col = mypal[14])
#'
#' Dichotomizing the simulated data
#'
#' MacCullum et al split X's data at the median, so we will too.
#'
dat <- as.data.frame(dat)
dat.s <- dat
medX <- median(dat.s[, "X"])
splt <- function(x, m) {
    ifelse(x >= m, 2, 1)
}
dat.s$Xd <- sapply(dat.s$X, splt, m = medX)
par(family = "ETBembo", pch = 20)
plot(dat.s$Xd, dat.s$Y, xaxt = 'n', xlab = "X", ylab = "Y"); axis(1, at = c(1, 2), xaxp = c(1, 2, 2), labels = c("1", "2"))
#'
#' -----
#'
#' A blogger, [Francis Smart](https://plus.google.com/113450672614304141574), at [EconBS](http://www.econometricsbysimulation.com/) offerred an alternative approach for simulating data from any distribution in a February, 2014 post: [_"Easily generate correlated variables from any distribution"_](http://www.econometricsbysimulation.com/2014/02/easily-generate-correlated-variables.html)
#'
#'
sigma2 <- matrix(c(0.6), nrow = 2, ncol = 2) + diag(2)*rxy
print(sigma2)
dat2 <- tbl_df(mvrnorm(N, mu = mu, Sigma = sigma2, empirical = TRUE))
colnames(dat2) <- c("X2", "Y2")
dat2Cov <- cov(dat2)
dat2Cov
dat2Cor <- cor(dat2)
dat2Cor
dat2Mu <- apply(dat2, 2, mean)
dat2Mu
#'
dat2A <- head(dat2, nrow(dat2)*pi0)
dat2B <- tail(dat2, nrow(dat2)*pi0)
dat2kab <- cbind(dat2A, dat2B)
kable(dat2kab)
#'
#' Plotting the simulated dat2a
#'
library(mixtools)
par(family = "ETBembo", pch = 20)
plot(dat2$X, dat2$Y, xlab = "X", ylab = "Y", ylim = c(min(dat2$Y)-1, max(dat2$Y)+2), xlim = c(min(dat2$X)-1, max(dat2$X)+2))
ellipse(dat2Mu, dat2Cov, alpha = .05, col = mypal[14])
#'
#' Dichotomizing the simulated dat2a
#'
#' MacCullum et al split X's dat2a at the median, so we will too.
#'
dat2 <- as.dat2a.frame(dat2)
dat2.s <- dat2
medX <- median(dat2.s[, "X"])
splt <- function(x, m) {
    ifelse(x >= m, 2, 1)
}
dat2.s$Xd <- sapply(dat2.s$X, splt, m = medX)
par(family = "ETBembo", pch = 20)
plot(dat2.s$Xd, dat2.s$Y)
#'
#'
#' [Another approach](http://stackoverflow.com/a/10539448/5944560) using the `{bindata}` package from SO user, [Josh O'Brien](http://stackoverflow.com/users/980833/josh-obrien).
#'
library(bindata)
size <- N
p1 <- pi0
p2 <- pi0
rho <- rxy
trials <- rmvbin(size, c(p1,p2), bincorr=(1-rho)*diag(2)+rho)
colSums(trials)

rmvBinomial <- function(n, size, p1, p2, rho) {
    X <- replicate(n, {
             colSums(rmvbin(size, c(p1, p2), bincorr = (1 - rho)*diag(2) + rho))
         })
    t(X)
}

dat3 <- tbl_df(rmvBinomial(N, size = size, p1 = p1, p2 = p2, rho = rho))
colnames(dat3) <- c("X", "Y")

cor(dat3$X, dat3$Y)
dat3Cov <- cov(dat3)
dat3Cov
dat3Cor <- cor(dat3)
dat3Cor
dat3Mu <- apply(dat3, 2, mean)
dat3Mu
plot(dat3$X, dat3$Y, xlab = "X", ylab = "Y"); ellipse(dat2Mu, dat2Cov, alpha = .05, col = mypal[14])
ellipse(dat3Mu, dat2Cov, alpha = .05, col = mypal[14])
#'
#+
A <- rbinom(n, 1, PAA)
A <- as.data.frame(A)
A$id <- seq(1:nrow(A))
A <- A[, c(2, 1)]
A$A <- factor(A$A, levels = c(0, 1), labels = c("White", "African American"))
