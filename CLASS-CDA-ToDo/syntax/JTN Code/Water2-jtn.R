### Logistic regression 
r=c(0,2,10,13,20,25)
n=c(10,25,28,31,43,29)
gravity=0:5
counts=cbind(r,n-r)
result=glm(counts~gravity,family=binomial("logit"))
result=summary(result,correlation=TRUE,symbolic.cor = TRUE)
result
result$coefficients
result$correlation

### Predicted probability
phat0=1/(exp(-result$coefficients[1])+1)
phat1=1/(exp(-result$coefficients[1]-result$coefficients[2])+1)
phat0
phat1