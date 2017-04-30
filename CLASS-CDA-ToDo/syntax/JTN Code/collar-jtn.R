#### job satisfaction example

collar=read.table("collar.txt")
manager=factor(collar[,1])
super=factor(collar[,2])
worker=factor(collar[,3])
count=collar[,4]

#### Saturated Model (MSW)

modelSat=glm(count~manager+super+worker+manager*super+manager*worker+super*worker+manager*super*worker,family=poisson(link=log))
summary(modelSat)

#### get AIC and BIC for the saturated model

AIC(modelSat)
AIC(modelSat, k=log(sum(count)))

#### you can run the above two commands 
#### for each of the models below, and 
#### look for the one with the smallest AIC and the smallest BIC

#### Complete independence model (M,S,W)

model=glm(count~manager+super+worker,family=poisson(link=log))
summary(model)

#### Joint independence of (MS,W)

model=glm(count~manager+super+worker+manager*super,family=poisson(link=log))
summary(model)

#### Joint independence of (MW,S)

model=glm(count~manager+super+worker+manager*worker,family=poisson(link=log))
summary(model)

#### Joint indepence of (SW,M)

model=glm(count~manager+super+worker+super*worker,family=poisson(link=log))
summary(model)

#### Conditional independence (MS,MW)

modelCI=glm(count~manager+super+worker+manager*super+manager*worker,family=poisson(link=log))
summary(modelCI)
predCI=fitted(modelCI)

#### To produce linear regression like residual diagnostics

influence.measures(modelCI)

#### to get standardized devaiance residuals

rstandard(modelCI)

#### to get likelihood residuals 

rstudent(modelCI)

#### To get BIC and AIC in the table in the notes
#### BIC for comparison to the saturated model

bicCISat=AIC(modelCI, k=log(sum(count)))-AIC(modelSat,k=log(sum(count)))

#### AIC for comparison to the saturated model
#### Formula AIC=-2*LogL+2*#Parm=G2-2*#Parm
#### #Parm=number of parameters in the CI model

G2=2*(logLik(modelSat)-logLik(modelCI))
aicCISat=G2[1]-2*6


#### Conditional independence (MS,SW)

model=glm(count~manager+super+worker+manager*super+super*worker,family=poisson(link=log))
summary(model)

#### Conditional independence (MW,SW)

model=glm(count~manager+super+worker+manager*worker+super*worker,family=poisson(link=log))
summary(model)



#### Homogeneous associations (MS,MW,SW)

model=glm(count~manager+super+worker+manager*super+manager*worker+worker*super,family=poisson(link=log))
summary(model)
predHA=fitted(model)

#### Dataset "ALL"

#### Note that 715 is the sample size

dCI=as.vector(abs(count-predCI)/(2*715))
dHA=as.vector(abs(count-predHA)/(2*715))
cbind(count,predCI,predHA,dCI,dHA)
simplestatistics=rbind(summary(count),summary(predCI),summary(predHA))

#### Pearson Correlations

cor(count,predCI)
cor(count,predHA)

#### The dissimilarity indices for Blue Collar Worker Example
                       
Sums=colSums(cbind(count,dCI,dHA))
Sums