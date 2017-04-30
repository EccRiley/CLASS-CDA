X=factor(c(rep("x1",4),rep("x2",4)))
Y=factor(c(rep(c("y1","y1","y2","y2"),2)))
Z=factor(c(rep(c("z1","z2"),4)))
count=c(0,6,5,9,16,5,7,0)
table=xtabs(count~X+Y+Z)

### Table 1 of X*Y
list(Frequency=table,RowPct=prop.table(table[,,1],1))
chisq.test(table[,,1],correct=FALSE)

### Table 2 of X*Y

list(Frequency=table,RowPct=prop.table(table[,,2],1))
chisq.test(table[,,2],correct=FALSE)


### Saturated Model

model=glm(count~X+Y+Z+X*Y+X*Z+Y*Z+X*Y*Z,family=poisson(link=log))
summary(model)

### Complete Independence Model:(X,Y,Z)

model=glm(count~X+Y+Z,family=poisson(link=log))
summary(model)

### Joint Independence Model:(XY,Z)

model=glm(count~X+Y+Z+X*Y,family=poisson(link=log))
summary(model)

### Joint Independence Model:(XZ,Y)

model=glm(count~X+Y+Z+X*Z,family=poisson(link=log))
summary(model)

### Joint Independence Model:(YZ,X)

model=glm(count~X+Y+Z+Y*Z,family=poisson(link=log))
summary(model)

### Conditional Indepence Model:(XY,XZ)

model=glm(count~X+Y+Z+X*Y+X*Z,family=poisson(link=log))
summary(model)

### Conditional Indepence Model:(XY,YZ)

model=glm(count~X+Y+Z+X*Y+Y*Z,family=poisson(link=log))
summary(model)

### Conditional Indepence Model:(XZ,YZ)

model=glm(count~X+Y+Z+X*Z+Y*Z,family=poisson(link=log))
summary(model)

### Homogeneous Association (XY,XZ,YZ)

model=glm(count~X+Y+Z+X*Y+X*Z+Z*Y,family=poisson(link=log))
summary(model)

### Zero Margin,Bad Pattern of Zeros

count=c(0,6,5,9,0,5,16,7)
table=xtabs(count~X+Y+Z)

### Homogeneous Association (XY,XZ,YZ)with zero margin: (XY,XZ,YZ)

model=glm(count~X+Y+Z+X*Y+X*Z+Z*Y,family=poisson(link=log))
summary(model)

### Delete Zero Values

count=c(0,6,5,9,0,5,16,7)
count=count[which(count!=0)]
X=X[which(count!=0)]
Y=Y[which(count!=0)]
Z=Z[which(count!=0)]

### Homogeneous Association with zero margin: (XY,XZ,YZ)

model=glm(count~X+Y+Z+X*Y+X*Z+Z*Y,family=poisson(link=log))
summary(model)
