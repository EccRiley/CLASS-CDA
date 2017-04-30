active=rep(c("r","s","t","u","v","w"),c(6,6,6,6,6,6))
passive=rep(c("r","s","t","u","v","w"),6)
wt=c(0,1,5,8,9,0,29,0,14,46,4,0,0,0,0,0,0,0,2,3,1,0,38,2,0,0,0,0,0,1,9,25,4,6,13,0)
display=data.frame(active,passive,wt)[which(active!="t"),]

### Fit Log-linear model wt=active+passive: Test Quasi-independence for the incomplete table
library(MASS)
active=display[,1]
passive=display[,2]
freq=display[,3]
displaytable=table(active,passive)
displaytable[cbind(active,passive)]=freq
LoglinModel=loglm(freq~active+passive,param=T,fit=T)
loglin(fitted(LoglinModel),margin=list(1,2),param=T,fit=T)

glm(freq~active+passive,family=poisson(log))

summary(LoglinModel)
anova(LoglinModel)
fitted(LoglinModel)
#Parameter Estimates
LoglinModel$param



