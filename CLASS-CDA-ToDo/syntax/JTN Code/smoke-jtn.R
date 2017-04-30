################################
#### Example: Students and parents smoking
####  LOGLIN() & GLM() functions
#### Related: See smoke.sas  
#################################

#### Fitting logistic regression for a 2x2 table ##### 
#### Here is one way to read the data from the table and use glm()
#### NOTE:  If the data come from a datafile, or are in a different format
#### we would need a slightly different than what is used code below
#### You need to have the data for "success" in one column and "failure" in another
#### Notice how we need to use family=binomial (link=logit) 
#### while with log-linear models we used family=poisson(link=log)

#### define the explanatory variable with two levels: 
#### 1=one or more parents smoke, 0=no parents smoke

parentsmoke=as.factor(c(1,0))

#### NOTE: if we do parentsmoke=c(1,0) R will treat this as 
#### a numeric and not categorical variable

#### need to create a response vector so that it has counts for both "success" and "failure" 

response<-cbind(yes=c(816,188),no=c(3203,1168))
response

#### fit the logistic regression model 

smoke.logistic<-glm(response~parentsmoke, family=binomial(link=logit))

#### OUTPUT 

smoke.logistic
summary(smoke.logistic)
anova(smoke.logistic)

#### NOW, treat parents smoking as a factor



#### Fitting logistic regression for a 2x3 table ##### 
#### Here is one way to read the data from the table and use glm()
#### Notice how we need to use family=binomial (link=logit) 
#### while with log-linear models we used family=poisson(link=log)

parentsmoke=as.factor(c(2,1,0))
response<-cbind(c(400,416,188),c(1380,1823,1168))
response
smoke.logistic<-glm(response~parentsmoke, family=binomial(link=logit))

#### OUTPUT

smoke.logistic
summary(smoke.logistic)
anova(smoke.logistic)

### TO use Homser-Lemeshow statistic (although not relevant here since the number of groups is small)
## First run ROCandHL.R script which has function hosmerlem() in it
## hosmerlem() takes the vector of successes, predicted vector of success and g=# of groups as input
## produce the vector of predicted success "yhat"
yhat=rowSums(response)*predict(smoke.logistic, type="response")
yhat

hosmerlem(response[,1], yhat, g=3) ## here run 3 groups
