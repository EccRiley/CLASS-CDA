
## Read the data file for the Credit Card example

data=read.table("creditcard.txt") 
lcases= log(data[,2])
data=cbind(data,lcases)
colnames(data)=c("income","cases","CrCards","lcases")
data

## Fit the data with poisson linear model with offset=lcases

log.fit=glm(CrCards~income+offset(lcases),family=poisson,data=data)
summary(log.fit)

## Fitted Values under poisson linear model

fitted(log.fit)

