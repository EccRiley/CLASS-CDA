critique=matrix(c(24,8,10,8,13,9,13,11,64),nr=3,dimnames=list("siskel"=c("con","mixed","pro"),"ebert"=c("con","mixed","pro")))
critique

#### Chi Square Test

result=chisq.test(critique)
result

#### Likelihood Ratio Chi-Square Statistic

LRresult=2*sum(critique*log(critique/result$expected))

#### p-value for the Likelihood Ratio Chi-Square Test

LRchisq=1-pchisq(LRresult,df=4)
LRresult
LRchisq

#### Simple Kappa Coefficient
#### Using a function Kappa() in package vcd. 
#### Please first load packages VR, colorspace and grid and finally load vcd.

library(vcd)
kappa=Kappa(critique)
kappa