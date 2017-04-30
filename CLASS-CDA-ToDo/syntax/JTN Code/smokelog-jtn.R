#### Fitting logistic regression to smoking data and loglinear model

S=factor(c("smoke","nonsmoke"))
y=c(816,188)
n=c(4019,1356)
count=cbind(y,n-y)
result=glm(count~S,family=binomial("logit"))

#### without intercept

result=glm(count~1,family=binomial("logit"))
summary(result)
prob=result$fitted
Deviance=resid(result,type="deviance")
Pearson=resid(result,type="pearson")

#### Log-linear model of independence 2x2 table

options(contrasts=c("contr.treatment","contr.poly"))
s=factor(rep(c("smoke","nonsmoke"),c(2,2)))
y=factor(rep(c("yes","no"),2))
count=c(816,3203,188,1168)
result=glm(count~s+y,family=poisson)
summary(result)
prob=result$fitted
Deviance=resid(result,type="deviance")
Pearson=resid(result,type="pearson")