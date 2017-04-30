#' [Video tutorial](https://onlinecourses.science.psu.edu/stat504/sites/onlinecourses.science.psu.edu.stat504/files/lesson03/McNemar_Test_viewlet_swf.html)

vote=matrix(c(794,86,150,570),nr=2,dimnames=list("1st Survey"=c("Approve","Disapprove"),"2nd Survey"=c("Approve","Disapprove")))
vote
##Set correct=F to apply McNemar without continuity correction
mcnemar.test(vote,correct=F)
##Set correct=t to apply McNemar with continuity correction
##Note: we don't really need the correction here since the sample size is large
mcnemar.test(vote,correct=T)

### Simple Kappa Coefficient

### Using the original formula to calculate the Simple Kappa Coefficient
prop=vote/sum(vote)
Po=sum(diag(prop))
Pe=rowSums(prop)[1]*colSums(prop)[1]+rowSums(prop)[2]*colSums(prop)[2]
kappa=(Po-Pe)/(1-Pe)
kappa

### Using a function Kappa() in package vcd. 
### Please first load packages VR, colorspace and grid and finally load vcd.
#install.packages("vcd")
library(vcd)
kappa=Kappa(vote)
CI_kappa=cbind(0.69959267-qnorm(0.975)*0.01805518,0.69959267+qnorm(0.975)*0.01805518)
CI_kappa
### or use confint() function
confint(kappa)

###agreement plot
####observed and expected diagonal elements are represented by superposed black and white rectangles, respectively.
agreementplot(vote)


####Consider cross-sectional design 
vote=matrix(c(944,880,656,720),nr=2,dimnames=list(c("1st Survey","2nd Survey"),c("Approve","Disapprove")))
vote
count=vote
##The ususal chi-sq. test
result=chisq.test(vote, correct=F)
result
##Set correct=F to apply McNemar without continuity correction
mcnemar.test(vote,correct=F)
##Set correct=t to apply McNemar with continuity correction
##Note: we don't really need the correction here since the sample size is large
mcnemar.test(vote,correct=T)

Contingency_Table=list(Frequency=count,Expected=result$expected,Deviation=count-result$expected,Percentage=prop.table(count),RowPercentage=prop.table(count,1),ColPercentage=prop.table(count,2))
Contingency_Table