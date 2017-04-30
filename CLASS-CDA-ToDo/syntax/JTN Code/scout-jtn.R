#### corresponds with scout1.SAS
#### Fit a Logistic regression with S="scout" or "nonscout"

S=factor(c("scout","nonscout"))
Sscout=(S=="scout")
Snonscout=(S=="nonscout")
y=c(33,64)
n=c(376,424)
count=cbind(y,n-y)
result=glm(count~Sscout+Snonscout,family=binomial("logit"))
summary(result)

#### corresponds with scout2.SAS
#### Fit a Logistic regression with S="low","medium" or "high"

S=factor(c("low","medium","high"))
y=c(53,34,10)
n=c(265,270,265)
count=cbind(y,n-y)
Smedium=(S=="medium")
Shigh=(S=="high")
result=glm(count~Smedium+Shigh,family=binomial("logit"))
summary(result)

#### corresponds with scout3.SAS
#### Fit a Logistic regression with S="low","medium" or "high"(reference level)and
#### B="scout" or "nonscout"(reference level)

S=factor(rep(c("low","medium","high"),c(2,2,2)))
Slow=(S=="low")
Smedium=(S=="medium")
B=factor(rep(c("scout","nonscout"),3))
Bscout=(B=="scout")
y=c(11,42,14,20,8,2)
n=c(54,211,118,152,204,61)
count=cbind(y,n-y)
result=glm(count~Bscout+Slow+Smedium,family=binomial("logit"))
summary(result)

#### corresponds with scout4.SAS
#### Fit a Logistic regression with S="low","medium" or "high"(reference level)and
#### B="scout" or "nonscout"(reference level)
#### changed y and n

S=factor(rep(c("low","medium","high"),c(2,2,2)))
Smedium=(S=="medium")
Shigh=(S=="high")
B=factor(rep(c("scout","nonscout"),3))
Bscout=(B=="scout")
y=c(11,42,14,20,8,2)
n=c(11,42,14,20,8,2)+c(43,169,104,132,196,59)
count=cbind(y,n-y)
result=glm(count~Bscout+Smedium+Shigh+Bscout*Smedium+Bscout*Shigh,family=binomial("logit"))
summary(result)
#to get estimated scaled covariance matrix
covmat=summary(restul)$cov.scaled 
## get the standard error for beta_1+beta_4 estimate
sqrt(covmat[2,2]+covmat[5,5]+2*covmat[2,5])
## get the estimate of beta_1+beta_4
summary(result)$coefficients[2,1]+summary(result)$coefficients[5,1]

#### corresponds with scout5.SAS
#### Fit a Logistic regression with x1-x6 with intercept.
#### need to work on the glm() function to change it to "without intercept"

S=factor(rep(c("low","medium","high"),c(2,2,2)))
B=factor(rep(c("scout","nonscout"),3))
y=c(11,42,14,20,8,2)
n=c(11,42,14,20,8,2)+c(43,169,104,132,196,59)
x1=(S=="low")*1
x2=(S=="low")*(B=="scout")
x3=(S=="medium")*1
x4=(S=="medium")*(B=="scout")
x5=(S=="high")*1
x6=(S=="high")*(B=="scout")
count=cbind(y,n-y)
result=glm(count~cbind(x1,x2,x3,x4,x5,x6)-1,family=binomial("logit"))
result$deviance
summary(result)
