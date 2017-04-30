#######################################################################
##### Penn State student drinking example for Binomial likelihood and loglikelihood functions
##### Estimate of the MLE
##### Test for 1-sample proportion based on the normal approximation
##### Test for 1-sample proportion based on Likelihood Ratio Test
##### R code that corresponds to the SAS code
######################################################################
#' [Video tutorial](https://onlinecourses.science.psu.edu/stat504/sites/onlinecourses.science.psu.edu.stat504/files/lesson02/Drinking_R_viewlet_swf.html)

########## Likelihood & Loglikelihood ######################################
##### Define the likelihood function for binomial sample with N=1315 and X=630:

likelhd = function(p) dbinom(630,1315,p)
loglik = function(p) dbinom(630,1315,p, log=TRUE)

##### Plot the likelihood function and save it into a file:
##### If you do not want it to be saved into a file directly, comment out the
##### postscript() and dev.off() lines

postscript("drinkingLik.ps")
plot(likelhd,0,1,xlab="pi",ylab="l(p)",main="Binomial likelihood, N=1315, X=630")
dev.off()

postscript("drinkingLogLik.ps")
plot(loglik,0,1,xlab="pi",ylab="l(p)",main="Binomial loglikelihood, N=1315, X=630")
dev.off()

############# MLE ######################################
##### Find the MLE (for multiparameter problems, we would use the nlm() function)
##### $maximum is the value of the MLE

mle=optimize(likelhd,c(0,1),maximum=TRUE)
mle
mle=mle$maximum

######### 1-proportion Hypothesis test, Approximate 95% confidence interval, and MLE #####
##### H0: p=0.5 vs. Ha: p is not equal 0.5 

prop.test(630, 1315, 0.5)

### when the sample size is small, we can do an exact test via:

binom.test(630,1315,0.5)

##### Now the 1-proportion Hypothesis test and the 95% CI based on the LR test (statistic, and 95% CI)
##### calculating Likelihood Ratio Statistics and testing for it's significance 

LRstats=2*(loglik(0.48)-loglik(0.5))
LRstats

##### p-value from chisq distribution with degrees of freedom=1 

pvalue<-1-pchisq(LRstats, 1)
pvalue

###### Likelihood ratio based 95% CI #####
### First compute the horizontal cutoff, and plot it on the loglikelhood

cutoff=loglik(0.48)-1.92
cutoff

postscript("drinkingCI.ps")
plot(loglik,0,1,xlab="pi",ylab="l(p)",main="Binomial loglikelihood, N=1315, X=630", ylim=c(-60,-3), xlim=c(0.3,0.65))
abline(h=cutoff)

### compute where the cutoff line intersects the loglikelihood 
### and plot the vertical lines at those points

loglik.optim=function(p){abs(cutoff-loglik(p))}

min1=optimize(loglik.optim, c(0,mle))
min2=optimize(loglik.optim, c(mle,1))

min1
min2

abline(v=min1$minimum)
abline(v=min2$minimum)

#abline(v=0.453)
#abline(v=0.506)
dev.off()

##################################################
##### Here is R code version of this problem 
####  that corresponds to the SAS code and its output 
#################################################

drinking=c(rep("high risk",630),rep("low risk",685))

##### Freq Procedure

Percentage=100*as.vector(table(drinking))/sum(table(drinking))
CumulativeFrequency=cumsum(c(630,685))
CumulativePercentage=cumsum(Percentage)
Freq=data.frame(table(drinking),Percentage,CumulativeFrequency,CumulativePercentage,row.names=NULL)
row.names(Freq)=NULL
Freq

##### asymptotic standard error

phat=630/(630+685)
p0=0.5
n=630+685
ASE=sqrt(phat*(1-phat)/n)
ASE

##### The asymptotic test statistic Z

Z=(phat-0.5)/sqrt(p0*(1-p0)/n)
Z

##### One-sided Pr <  Z

pnorm(Z)

##### Two-sided Pr > |Z|

2*pnorm(Z) 

##### Using the normal approximation, calculate the asymptotic confidence interval

CI_upper=phat+qnorm(0.025,lower.tail=F)*ASE
CI_lower=phat-qnorm(0.025,lower.tail=F)*ASE
CI=cbind(CI_lower,CI_upper)
CI 

##### The approximate confidence interval can also be calculated by prop.test()

prop.test(630,630+685,p=630/(630+685))

##### Apply the exact binomial test to test H0:p=0.5 for high-risk drinkers

binom.test(c(630,685),p=0.5)

