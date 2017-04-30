icecream=read.table("ice_cream.txt")
colnames(icecream)=c("U","Y ","Freq","U2")

### Fit the model Y=U+U2 and conduct ANOVA analysis
model=glm(icecream$Y~icecream$U+icecream$U2, weight=icecream$Freq)
summary(model)
anova(model)

### Fit the model z1=U+U2 and do the likelihood ratio test
z1=ifelse(icecream$Y<2,0,1)
model=glm(z1~icecream$U+icecream$U2, family=binomial,weight=icecream$Freq)
summary(model)
LRT=drop1(model,test="Chisq")

### Fit the model z2=U+U2 and do the likelihood ratio test
z2=ifelse(icecream$Y<3,0,1)
model=glm(z2~icecream$U+icecream$U2, family=binomial,weight=icecream$Freq)
summary(model)
LRT=drop1(model,test="Chisq")

