### Test of conditional independence Lesson 5:
### Use of oddsratio(), confit() from {vcd}
### Cochran-Mantel-Haenszel test
### Also testing via Log-linear models: related Lesson 10
#########################################################

### Input the table
deliquent=c("no","yes")
scout=c("no", "yes")
SES=c("low", "med","high")
table=expand.grid(deliquent=deliquent,scout=scout,SES=SES)
count=c(169,42,43,11,132,20,104,14,59,2,196,8)
table=cbind(table,count=count)
table
temp=xtabs(count~deliquent+scout+SES,table)
temp

### Create "flat" contigency tables

ftable(temp)

##Let's see how we can create various subtables
### One-way Table SES
Frequency=as.vector(margin.table(temp,3))
CumFrequency=cumsum(Frequency)
cbind(SES,Frequency=Frequency,Percentage=Frequency/sum(Frequency),CumFrequency=CumFrequency,CumPercentage=CumFrequency/sum(Frequency))

### One-way Table scout
Frequency=as.vector(margin.table(temp,2))
CumFrequency=cumsum(Frequency)
cbind(scout,Frequency=Frequency,Percentage=Frequency/sum(Frequency),CumFrequency=CumFrequency,CumPercentage=CumFrequency/sum(Frequency))

### One-way Table deliquent
Frequency=as.vector(margin.table(temp,1))
CumFrequency=cumsum(Frequency)
cbind(deliquent,Frequency=Frequency,Percentage=Frequency/sum(Frequency),CumFrequency=CumFrequency,CumPercentage=CumFrequency/sum(Frequency))

### Test the Mutual Independence, step by step 
### compute the expected frequences
E=array(NA,dim(temp))
for (i in 1:dim(temp)[1]) {
for (j in 1:dim(temp)[2]) {
for (k in 1:dim(temp)[3]) {
E[i,j,k]=(margin.table(temp,3)[k]*margin.table(temp,2)[j]*margin.table(temp,1))[i]/(sum(temp))^2
}}}
E

### compute the X^2, and G^2
df=(length(temp)-1)-(sum(dim(temp))-3)
X2=sum((temp-E)^2/E)
X2
1-pchisq(X2,df)
G2=2*sum(temp*log(temp/E))
G2
1-pchisq(G2,df)


### Test for Mutual indpendence (and other models) by considering anlaysis of all two-way tables
### Two-way Table SES*scout
### This is a test of Marginal Independence for SES and Scout
SES_Scout=margin.table(temp,c(3,2))
result=chisq.test(SES_Scout)
result
result$expected
result=chisq.test(SES_Scout,correct = FALSE)
result
SES_Scout=list(Frequency=SES_Scout,RowPercentage=prop.table(SES_Scout,2))


### Two-way Table SES*deliquent
### temp=xtabs(count~scout+SES+deliquent,table)
### SES_deliquent=addmargins(temp)[1:2,1:3,3]
### This is a test of Marginal Independence for SES and deliquent status
SES_deliquent=margin.table(temp,c(3,1))
result=chisq.test(SES_deliquent)
result
result$expected
result=chisq.test(SES_deliquent,correct = FALSE)
result
SES_deliquent=list(Frequency=SES_deliquent,RowPercentage=prop.table(SES_deliquent,2))

### Two-way Table deliquent*scout
### This is a test of Marginal Independence for Deliquent status and the Scout status
deliquent_scout=margin.table(temp,c(1,2))
result=chisq.test(deliquent_scout)
result$expected
result=chisq.test(deliquent_scout,correct = FALSE)
result
deliquent_scout1=list(Frequency=deliquent_scout,RowPercentage=prop.table(deliquent_scout,1))

## compute the log(oddsraio), oddsratio and its 95% CI using {vcd} package
lor=oddsratio(deliquent_scout)
lor
OR=exp(lor)
OR
OR=oddsratio(deliquent_scout, log=FALSE)
OR
CI=exp(confint(lor))
CI
CI=confint(OR)
CI


### Table of deliquent*scout at different level of SES
temp

### Test for Joint Independence of (D,BS)
## creating 6x2 table, BS x D
SESscout_deliquent=ftable(temp, row.vars=c(3,2))
result=chisq.test(SESscout_deliquent)
result

### Test for Marginal Independence (see above analysis of two-way tables)

#### Test for conditional independence
### To get partial tables of DB for each level of S
temp[,,1]
chisq.test(temp[,,1], correct=FALSE)
temp[,,2]
chisq.test(temp[,,2], correct=FALSE)
temp[,,3]
chisq.test(temp[,,3], correct=FALSE)
X2=sum(chisq.test(temp[,,1], correct=FALSE)$statistic+chisq.test(temp[,,2], correct=FALSE)$statistic+chisq.test(temp[,,3], correct=FALSE)$statistic)
1-pchisq(X2,df=3)

### Cochran-Mantel-Haenszel test
mantelhaen.test(temp)
mantelhaen.test(temp,correct=FALSE) 

### Breslow-Day test
### make sure to first source/run breslowday.test.R
breslowday.test(temp)

################################################################
#################### Log-linear Models: Lesson 10 ##############
#### Let's see how we would this via Log_linear models 
#### We will look at the details later in the course
### test of conditional independence
### via loglinear model, but the object has to be a table!

is.table(temp)  ### to check if table
temp<-as.table(temp) ### to save as table
temp.condind<-loglin(temp, list(c(1,3), c(2,3)), fit=TRUE, param=TRUE) ### fit the cond.indep. model
temp.condind
1-pchisq(temp.condind$lrt, temp.condind$df)

#### There is no way to do Breslow-Day stats in R; you need to write your own function or fit the homogeneous association model to test for identity of odds-ratios

### test of homogenous association model
temp.hom<-loglin(temp, list(c(1,3), c(2,3), c(1,2)), fit=TRUE, param=TRUE)
temp.hom
1-pchisq(temp.hom$lrt, temp.hom$df)


### Here is how to do the same but with the glm() function in R
### test of conditional independence
### via loglinear mode, but using glm() funcion
### the object now needs to be dataframe
temp.data<-as.data.frame(temp)
temp.data
temp.condind<-glm(temp.data$Freq~temp.data$scout*temp.data$SES+temp.data$deliquent*temp.data$SES, family=poisson())
summary(temp.condind)
fitted(temp.condind)

### test of homogenous association model
temp.hom<-glm(temp.data$Freq~temp.data$scout*temp.data$SES+temp.data$deliquent*temp.data$SES+temp.data$scout*temp.data$deliquent, family=poisson())
summary(temp.hom)
fitted(temp.hom)

### Here is a way to fit a logistic regression
### for a 2x2 table scout vs. delinquent
### with the glm() function in R
### the object now needs to be dataframe
is.table(temp) ##check that the object is a table
tempBD<-margin.table(temp, c(2,1))## create a two way table of interst
counts<-cbind(tempBD[,2],tempBD[,1])
counts
scout
scout<-as.factor(scout)
temp.logit<-glm(counts~scout,family=binomial("logit"))
temp.logit


