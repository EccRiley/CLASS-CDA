#############################
#### Berkeley admissions data for log-linear models
#### See also berkeley.R 
#############################

#### Dataset already exists in R library

UCBAdmissions 

#### To test the odds-ratios in the marginal table and each of the subtables

library(vcd)

#### Two ways of fitting a log-linear model of complete independence

### Via loglin() function
berk.ind<-loglin(UCBAdmissions, list(1,2,3), fit=TRUE, param=TRUE)
berk.ind

#### Via glm() function 
berk.data<-as.data.frame(UCBAdmissions)
berk.data
berk.ind<-glm(berk.data$Freq~berk.data$Admit+berk.data$Gender+berk.data$Dept, family=poisson())
summary(berk.ind)
fits<-fitted(berk.ind)
resids <- residuals(berk.ind,type="pearson")
h <- lm.influence(berk.ind)$hat
adjresids <- resids/sqrt(1-h)
round(cbind(berk.data$Freq,fits,adjresids),2)


##### Saturated log-linear model 
#### via loglin()
berk.sat<-loglin(UCBAdmissions, list(c(1,2,3)), fit=TRUE, param=TRUE)
berk.sat

#### via glm()
berk.sat<-glm(berk.data$Freq~berk.data$Admit*berk.data$Gender*berk.data$Dept, family=poisson())
summary(berk.sat)
fitted(berk.sat)
 

###/* joint independence of Dept and Gender from Admit*/
berk.join=glm(berk.data$Freq~berk.data$Admit+ berk.data$Gender+berk.data$Dept+berk.data$Gender*berk.data$Dept,family=poisson(link=log))
summary(berk.join)

### /*conditional independence of D and A given S */
berk.cind=glm(berk.data$Freq~berk.data$Admit+ berk.data$Gender+berk.data$Dept+berk.data$Gender*berk.data$Dept+berk.data$Admit*berk.data$Gender,family=poisson(link=log))
summary(berk.cind)
anova(berk.cind)

### /*homogeneous associations */
berk.hom=glm(berk.data$Freq~berk.data$Admit+ berk.data$Gender+berk.data$Dept + berk.data$Gender+berk.data$Dept*berk.data$Gender+berk.data$Dept*berk.data$Admit+berk.data$Admit*berk.data$Gender, family=poisson(link=log))
summary(berk.hom)
anova(berk.hom)
