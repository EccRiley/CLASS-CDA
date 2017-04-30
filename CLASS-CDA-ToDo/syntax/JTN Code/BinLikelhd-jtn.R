#### SIMPLE Binomial LIKELIHOOD and LogLIKELIHOOD FUNCTIONS
#' 
[Video Lecture](https://onlinecourses.science.psu.edu/stat504/sites/onlinecourses.science.psu.edu.stat504/files/lesson02/02_Bin_Likelihood_viewlet_swf.html)

########### First Example ###############################
#### Define the likelihood function for a binomial sample with N=2 and X=1:

likelhd <- function(p) 2*p*(1-p)
loglik <- function(p) log(p)+log(1-p)

#### Plot the likelihood function:

plot(likelhd,0,1,xlab="pi",ylab="l(p)",main="Binomial likelihood, N=2, X=1")
plot(loglik,0,1,xlab="pi",ylab="l(p)",main="Binomial loglikelihood, N=2, X=1")


#########Second Example ##############################
#### Define the likelihood function for binomial sample with N=5 and X=2:

likelhd <- function(p) 10*p^2*(1-p)^3
loglik <- function(p) 2*log(p)+3*log(1-p)

#### Plot the likelihood function:

plot(likelhd,0,1,xlab="pi",ylab="l(p)",main="Binomial likelihood, N=5, X=2")
plot(loglik,0,1,xlab="pi",ylab="l(p)",main="Binomial loglikelihood, N=5, X=2")


############# MLE ######################################
#### Find the MLE (for multiparameter problems, we would use the nlm() function)
#### $maximum is the value of the MLE

optimize(likelhd,c(0,1),maximum=TRUE)


########## Another way to do all of the above with the density function (dbinom) #######
#### What if N=2, X=1

likelhd <- function(p) dbinom(1,2,p)
plot(likelhd,0,1,xlab="pi",ylab="l(p)",main="Binomial likelihood, N=2, X=1")
optimize(likelhd,c(0,1),maximum=TRUE)


#### What if N=5 X=2

likelhd <- function(p) dbinom(2,5,p)

plot(likelhd,0,1,xlab="pi",ylab="l(p)",main="Binomial likelihood, N=5, X=2")
optimize(likelhd,c(0,1),maximum=TRUE)

######## Example from the class ####################
#### calculate the LR test (statistic, and 95% CI)

loglik<-function(p)log(dbinom(650,1118,p))

#### or another way to do the same, and this is better ####

loglik<-function(p)dbinom(650, 1118,p, log=TRUE)

#### calculating Likelihood Ratio Statistics and testing for it's significance 

LRstats<-2*(loglik(0.58)-loglik(0.5))

#### p-value from chisq distribution with degrees of freedom=1 

pvalue<-1-pchisq(LRstats, 1)