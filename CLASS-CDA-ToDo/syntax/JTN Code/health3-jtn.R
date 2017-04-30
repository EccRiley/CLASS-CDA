#### fitting an incomplete table
#### replace a missing value with any value, e.g. 0
#### create a numerical indicator variable 'delta' that takes
#### 1 for the missing value and zero everywhere else

age=rep(c(1,2),c(8,8))
gender=rep(rep(c("male","female"),c(4,4)),2)
health=rep(rep(cbind("sex","mens","healthy","none"),2),2)
count=c(4,0,42,57,9,4,19,71,2,0,7,20,7,8,10,31)
delta1=rep(0,16)
delta2=c(0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0)
age=factor(age)
gender=factor(gender)
health=factor(health)
options(contrast=c("contr.treatment","contr.poly"))

#### Saturated without an indicator variable

model=glm(count~age+gender+health+age*health+age*gender+gender*health+age*gender*health,family=poisson(link=log))
summary(model)

#### Saturated with an indicator variable

model=glm(count~age+gender+health+age*health+age*gender+gender*health+age*gender*health+delta1+delta2,family=poisson(link=log))
summary(model)

#### homogeneous model without an indicator variable

model=glm(count~age+gender+health+age*health+age*gender+gender*health,family=poisson(link=log))
summary(model)

#### homogeneous model with an indicator variable

model=glm(count~age+gender+health+age*health+age*gender+gender*health+delta1+delta2,family=poisson(link=log))
summary(model)

#### independence model without an indicator variable

model=glm(count~age+gender+health,family=poisson(link=log))
summary(model)

#### independence model with an indicator variable

model=glm(count~age+gender+health+delta1+delta2,family=poisson(link=log))
summary(model)

####----------------With missing data------------------

count=c(4,NA,42,57,9,4,19,71,2,NA,7,20,7,8,10,31)

#### homogeneous model without an indicator variable

model=glm(count~age+gender+health+age*health+age*gender+gender*health,family=poisson(link=log))
summary(model)

#### independence model without an indicator variable

model=glm(count~age+gender+health,family=poisson(link=log))
summary(model)