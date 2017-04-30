options(contrasts = c("contr.treatment", "contr.poly"))
#options(contrasts = c("contr.treatment", "contr.SAS"))

abortion <- read.table( "abortion.txt", col.names=c("year", "rel", "edu", "att", "count") )

# see if att is a factor
is.factor( abortion$att )

# make it into an ordered factor in the order you want
abortion$att <- factor( abortion$att, levels=c("Neg","Mix","Pos") , ordered=T)
abortion$att

# see if rel is a factor and in the order you want
is.factor( abortion$rel )
abortion$rel
# want to set it up in a different order
abortion$rel=factor(abortion$rel, levels=c("Prot", "SProt", "Cath"))
abortion$rel

# see if edu is a factor
is.factor( abortion$edu)
abortion$edu
# want to set it up in a different order
abortion$edu=factor(abortion$edu, levels=c("Low", "Med", "High") )
abortion$edu

# check the same for year
is.factor( abortion$year )
abortion$year <- factor(abortion$year)
abortion$year

# load the MASS package
library(MASS)

##fit the saturated model
satmodel=polr( att ~ year+rel+edu+year:rel+year:edu+rel:edu+year:rel:edu, weights=count, data=abortion )
summary(satmodel)

##intercept only model
nullmodel= polr( att ~ 1, weights=count, data=abortion )
summary(nullmodel)

# fit the main effects proportional-odds logistic regression model
result = polr( att ~ year+rel+edu, weights=count, data=abortion )
summary(result)
anova(result, satmodel)
 confint(result)
stepAIC(satmodel)

result1=polr( att ~ year+rel+edu+rel:edu, weights=count, data=abortion )
summary(result1)
anova(result1, result)

result0 = polr( att ~ year, weights=count, data=abortion )
summary(result0)

