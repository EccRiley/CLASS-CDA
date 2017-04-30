#### Some basic BINOMIAL Calculations
#' [Video Lecture](https://onlinecourses.science.psu.edu/stat504/sites/onlinecourses.science.psu.edu.stat504/files/lesson02/02_Binomial_Calc_viewlet_swf.html)
#### 
#### How to obtain a sequence from 0 to 10:

0:10

############ First Example ##############################
#### Calculate p(x) for X=0,1,...,10 when N=10 and pi=0.5
#### dbinom() is the binomial probability function or "density"
####### for x=1 ############################

dbinom(1, 10, 0.5)

#### for x=1, 2, ...., 10 

dbinom(0:10, 10, 0.5)

#### Make a table of the distribution, rounding values to 4th decimal place:

round( cbind(0:10, dbinom(0:10, 10, 0.5)), 4)

#### Plot this distribution:

plot(0:10,dbinom(0:10,10,0.5),type="h",xlab="x",ylab="p(x)",main="Binomial Distribution (N=10, pi=1/2)")


########### Second Example #############################
#### How does the distribution change if we change the parameter value pi?
#### Same as above, but change pi=1/6:

round(cbind(0:10, dbinom(0:10, 10, 1/6)), 4)
plot(0:10,dbinom(0:10,10,1/6),type="h",xlab="x",ylab="p(x)",main="Binomial Distribution (N=10, pi=1/6)")


######### Example from class ####################
#### 1-proportion Hypothesis test, Approximate 95% confidence interval, and MLE
#### It also gives the value of the LR statistic 
#### N=1118, X=650, H0: p=0.5 vs. Ha: p is not equal 0.5 

prop.test(650, 1118, 0.5)

#### compare this to Minitab->1-proportion-> Summarized data-> Trials: 1118, Event: 650