#### Some basic POISSON calculations ####
#' [Video tutorial](https://onlinecourses.science.psu.edu/stat504/sites/onlinecourses.science.psu.edu.stat504/files/lesson02/02_Poisson_Calc_viewlet_swf.html)
#### Example: Poisson distribution with lambda=2 ####
### Calculate p(x) for x=0,1,...,10 when lambda=2 ####
### dpois() is the Poisson probability function or "density" ####
### For x=1 ####


dpois(1,2)


#### For x=1, 2, ... ,10 ####


dpois(0:10, 2)


#### Make a table of the distribution with rounding at 3 decimal places ####


round(cbind(0:10, dpois(0:10,2), 3))


#### Plot the probabilities and give the plot a title and label the axis ####
### type="h" makes the plot with the vertical lines): ####


plot(0:10,dpois(0:10,2),type="h",xlab="x",ylab="p(x)",main="Poisson Distribution (lambda=2)")



#### ppois() is the cumulative distribution function, P(X <= x) ####
### Find P(X <= 6) when lambda=2: ####


ppois(6, 2)


#### Instead of using ppois(), you could have added the probabilities ####
### from dpois() via sum() function ####


sum(dpois(0:6, 2))


#### Find P(Y>6) when lambda=2: ####


1 - ppois(6, 2)


#### Make a table of the first 11 Poisson probabilities and ####
### cumulative probabilities when lambda=2: ####


round(cbind(0:10, dpois(0:10,2), ppois(0:10,2)), 3)