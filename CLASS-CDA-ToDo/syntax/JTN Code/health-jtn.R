#### fitting an incomplete table
#### replace a missing value with any value, e.g. 0
#### create a numerical indicator variable 'delta' that takes 
#### 1 for the missing value and zero everywhere else

gender=rep(c("male","female"),c(4,4))
health=rep(cbind("sex","mens","healthy","none"),2)
count=c(6,0,49,77,16,12,29,102)
delta=c(0,1,0,0,0,0,0,0)

gender=factor(gender)
health=factor(health)
options(contrast=c("contr.treatment","contr.poly"))

#### Without an indicator variable

model=glm(count~gender+health+gender*health,family=poisson(link=log))
summary(model)

#### With an indicator variable

model=glm(count~gender+health+gender*health+delta,family=poisson(link=log))
summary(model)

#### Without an indicator variable: Independence model

model=glm(count~gender+health,family=poisson(link=log))
summary(model)

#### With an indicator variable: Independence model

model=glm(count~gender+health+delta,family=poisson(link=log))
summary(model)

####--------------With missing data-----------------------------

count=c(6,NA,49,77,16,12,29,102)

#### Saturated model without indicator

model=glm(count~gender+health+gender*health,family=poisson(link=log))
summary(model)

#### Saturated model without indicator

model=glm(count~gender+health+gender*health,family=poisson(link=log))
summary(model)

#### Independency model without indicator

model=glm(count~gender+health,family=poisson(link=log))
summary(model)

