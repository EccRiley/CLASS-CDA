
###NEW March 13, 2014 ####
###Example that corresponds to the lecture notes 
###using aggregate data from gator.txt which is a 16x5 flat table of the 2x2x4x5 table with total sample size of 219
## Gender={f=female, m=male}
## Size={<2.3=small, >2.3=large}
## Lake={george, hancock,oklawaha,trafford}
## Food is given in 5 column {Fish, Invertebrate,Reptile,Bird,Other}
install.packages("vgam")
library(VGAM)

# Baseline Categories Logit model for nominal response

gator = read.table("gator.txt",header=T)
gator$Size = factor(gator$Size,levels=levels(gator$Size)[2:1])
totaln=sum(gator[1:16,5:9]) ## total sample size
rown=c(1:16)
for (i in 1:16) {
	rown[i]=sum(gator[i,5:9]) 
	rown
	}
	
rown ## sample size by the profile

##set the ref levels so that R output matches the SAS code
##sets Hancock as the baseline level
contrasts(gator$Lake)=contr.treatment(levels(gator$Lake),base=2)
contrasts(gator$Lake)

##sets "small" as the refernce level
contrasts(gator$Size)=contr.treatment(levels(gator$Size),base=2)
contrasts(gator$Size)

##sets male as the reference level
contrasts(gator$Gender)=contr.treatment(levels(gator$Gender),base=2)
contrasts(gator$Gender)

##Fit all basline logit models with Fish as the basline level
## By default VGLM will use the last level as the baseline level for creating the logits
## to set Fish as the baseline level, specify it last in vglm call below

# intercept only
fit0=vglm(cbind(Bird,Invertebrate,Reptile,Other,Fish)~1, data=gator, family=multinomial)
fit0
summary(fit0)
deviance(fit0) ## gives only the deviance

# with Gender
fit1=vglm(cbind(Bird,Invertebrate,Reptile,Other,Fish)~Gender, data=gator, family=multinomial)
summary(fit1)

# with Size
fit2=vglm(cbind(Bird,Invertebrate,Reptile,Other,Fish)~Size, data=gator, family=multinomial)
summary(fit2)

# with Lake
fit3=vglm(cbind(Bird,Invertebrate,Reptile,Other,Fish)~Lake, data=gator, family=multinomial)
summary(fit3)

# with Lake + Size
fit4 = vglm(cbind(Bird,Invertebrate,Reptile,Other,Fish)~Lake+Size, data=gator, family=multinomial)
summary(fit4)

# with Lake + Size + Gender
fit5=vglm(cbind(Bird,Invertebrate,Reptile,Other,Fish)~Lake+Size+Gender, data=gator, family=multinomial)
summary(fit5)
exp(coefficients(fit5)) ## to get the odds and odds-ratios

# with Lake + Size + LakeXSize
fit6=vglm(cbind(Bird,Invertebrate,Reptile,Other,Fish)~Lake+Size+Lake:Size, data=gator, family=multinomial)
summary(fit6)

# saturated 
fitS = vglm(cbind(Bird,Invertebrate,Reptile,Other,Fish)~Lake+Size+Gender+Lake:Size+Lake:Gender+Size:Gender+Lake:Size:Gender, data=gator, family=multinomial)
fitS

## Parts of Analysis of deviance
## To compare deviances of different models, for example 
deviance(fit5)-deviance(fitS)
df.residual(fit5)-df.residual(fitS)

deviance(fit4)-deviance(fit5)
df.residual(fit4)-df.residual(fit5)


## to adderess overdispersion with model from the fit4
## we usually use the Chi-Sq. statisics devided by its dfs
## here we need to compute it via first computing pearson residuals
pears.res=(fitted.values(fitS)*rown-fitted.values(fit4)*rown)^2/(fitted.values(fit4)*rown)
X2=sum(pears.res)

scaleparm=sqrt(X2/44)  ## 1.148 for fit 4
## then adjust for dispersion
summary(fit4, dispersion=scaleparm)
##or  gives the same
summary(fit4, dispersion=1.148)

## Consider collapsing over Gender
## see notes on ANGEL

### For Sections 8.2 and 8.3 in the notes?
# Baseline Categories Logit model for nominal response
# Adjacent-Categories Logit Model for nominal response

##recall that vglm uses the last level in R is the default level for creating the logits
fit.bcl = vglm(cbind(Bird,Invertebrate,Reptile,Other,Fish)~Lake+Size,data=gator,family=multinomial) # Lake + Size

fit.acl = vglm(cbind(Invertebrate,Reptile,Bird,Other,Fish)~Lake+Size,data=gator,family=acat(rev=T)) # consult help(acat) 

deviance(fit.bcl)
deviance(fit.acl) # model fits are equivalent
junk = expand.grid(Size=levels(gator$Size),Lake=levels(gator$Lake))
(pred.bcl = cbind(junk,predict(fit.bcl,type="response",newdata=junk))) # pred. same  
(pred.acl = cbind(junk,predict(fit.acl,type="response",newdata=junk))) # for both models

t(coef(fit.bcl,matrix=T)) 
t(coef(fit.acl,matrix=T)) # coefficients are different, but related:

rev(cumsum(rev(coef(fit.acl,matrix=T)["(Intercept)",])))  #These are the alpha_j for the baseline-category logit model, derived from the adjacent-categories logit model

rev(cumsum(rev(coef(fit.acl,matrix=T)["Lakehancock",]))) # effects for Lake Hancock for bcl model, derived from acl model

rev(cumsum(rev(coef(fit.acl,matrix=T)["Lakeoklawaha",]))) # effects for Lake Oklawaha for bcl model, derived from acl model

rev(cumsum(rev(coef(fit.acl,matrix=T)["Laketrafford",]))) # effect for Lake Trafford for bcl model, derived from acl model

rev(cumsum(rev(coef(fit.acl,matrix=T)["Size<2.3",]))) # effects for small alligators for bcl model, derived from acl model


####################################################################
### To see other possible pakages in R to fit these data, see additional code on ANGEL
## alligator-working.R and alligator.dat
##############################################################