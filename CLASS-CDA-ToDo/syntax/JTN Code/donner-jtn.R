donner=read.table("donner.txt")
survive=donner[,3]
age=donner[,1]
sex=donner[,2]

### Table sex*survive
table=as.matrix(table(sex,survive))
contingency_table=list(Frequency=table,Expected=chisq.test(table)$expected)
contingency_table
chisq.test(table, correct=FALSE) ## chisq. test of independence 
LM=lm(survive~age) ## linear regression
summary(LM)
LMANOVA=anova(LM) ## anova
LMANOVA

### Plot Survive*Age

plot(age,survive,xlim=c(15,70),ylim=c(-1.5,2.0),main="survive v.s. age")
abline(LM,col="red")
abline(confint(LM)[1],confint(LM)[2],col="green")
abline(confint(LM)[3],confint(LM)[4],col="purple")
### Plot Predicted*Age
plot(age,fitted(LM),main="Predicted v.s. Age")

### Q-Q plot
qqnorm(residuals(LM),main="Q-Q Plot")

### Studentized residuals v.s Observation
plot(rstudent(LM),main="Studentized residual v.s. observation")
abline(h=0)

###----------fitting logistic regression survive~age
result=glm(survive~age,family=binomial("logit"))
summary(result)

## to get the specific coefficient, this command will produce a vector
coefficients(result)
## to access the estimated slope and turn it into the odds-ratio
exp(coefficients(result)[2])

confint(result) ## confidence interval for parameters
exp(confint(result)) ## exponentiate to get on the odds-scale

### Diagnostics Measures

lm.influence(result)

### Pearson Residuals v.s. observation 
plot(residuals(result,type="pearson"),main="pearson residual plot")

### Deviance Residuals v.s. observation
plot(residuals(result,type="deviance"),main="deviance residual plot")

### Hat Diagonal Plot
plot(hatvalues(result),ylab="H",xlab="Case Number Index")

### Intercept DfBeta Plot
plot(dfbetas(result)[,1],ylab="DFBETA0",xlab="Case Number Index")

### Intercept DfBeta Plot
plot(dfbetas(result)[,2],ylab="DFBETA1",xlab="Case Number Index")

### Table age*survive
table=as.matrix(table(age,survive))
contigency_table=list(Frequency=table,Expected=chisq.test(table)$expected,Percent=prop.table(table),RowPct=prop.table(table,1),ColPct=prop.table(table,2))
contigency_table
chisq.test(table)

###----------fitting logistic regression survive~age+sex
result=glm(survive~age+sex,family=binomial("logit"))
summary(result)
confint(result) ## confidence interval for the parameters 

### Diagnostics Measures

lm.influence(result)

### Pearson Residuals v.s. observation 
plot(residuals(result,type="pearson"),main="pearson residual plot")

### Deviance Residuals v.s. observation
plot(residuals(result,type="deviance"),main="deviance residual plot")

### Hat Diagonal Plot
plot(hatvalues(result),ylab="H",xlab="Case Number Index")

### Intercept DfBeta Plot
plot(dfbetas(result)[,1],ylab="DFBETA0",xlab="Case Number Index")

### Intercept DfBeta Plot
plot(dfbetas(result)[,2],ylab="DFBETA1",xlab="Case Number Index")

###----------fitting logistic regression survive~age+sex+age*sex
donner=as.data.frame(donner)
sort(donner,c(donner$V1,donner$V2))
result=glm(survive~age+sex+age*sex,family=binomial("logit"))
out=data.frame(survive,age,sex,pi=result$fitted)
out

### Sort by sex age and Plot by sex group
group1=(out[which(sex==0),])[sort.int(age[which(sex==0)],index=TRUE)$ix,]
group2=(out[which(sex==1),])[sort.int(age[which(sex==1)],index=TRUE)$ix,]
plot(group1$age,group1$pi,col="red",type="l",xlim=c(10,70),ylim=c(0,1),ylab="Estimated Probability",xlab="age")
lines(group2$age,group2$pi,col="blue",type="c")

##what if we have two categorical predictors and want to add an interaction term? 
