### Logistic regression 
r=c(32,38)
n=c(107,59)
sex=c(1,0)
counts=cbind(r,n-r)
model=glm(counts~sex,family=binomial("logit"))
result=summary(model,corr=TRUE)
result$coefficients
result$corr

### Predicted probability of sex=0

phat0=1/(exp(-result$coefficients[1])+1)
upper=1/(exp(-result$coefficients[1]-qnorm(0.975)*result$coefficients[3])+1)
lower=1/(exp(-result$coefficients[1]+qnorm(0.975)*result$coefficients[3])+1)
pred=c(phat0,lower,upper)

###Predicted probability of sex=1

phat1=1/(exp(-result$coefficients[1]-result$coefficients[2])+1)

### estimated odds ratio

est_odds=exp(result$coefficients[2])

### Likelihood Ratio Test: the deviance change

LRT=drop1(model,test="Chisq")


