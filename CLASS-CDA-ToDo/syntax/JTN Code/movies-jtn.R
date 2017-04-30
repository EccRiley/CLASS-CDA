siskel=rep(c("con","mixed","pro"),c(3,3,3))
ebert=rep(c("con","mixed","pro"),3)
count=c(24,8,13,8,13,11,10,9,64)
siskel=factor(siskel)
ebert=factor(ebert)

### Create indicator variables for each diagonal cell
icon=(siskel=="con")*(ebert=="con")
siskel_con=(siskel=="con")
siskel_mixed=(siskel=="mixed")
siskel_pro=(siskel=="pro")
ebert_con=(ebert=="con")
ebert_mixed=(ebert=="mixed")
ebert_pro=(ebert=="pro")
imixed=(siskel=="mixed")*(ebert=="mixed")
ipro=(siskel=="pro")*(ebert=="pro")
symm3=3*(siskel=="pro")*(ebert=="pro")
symm1=1*(siskel=="con")*(ebert=="con")
symm4=4*(siskel=="mixed")*(ebert=="con")+4*(siskel=="con")*(ebert=="mixed")
symm6=6*(siskel=="con")*(ebert=="pro")+6*(siskel=="pro")*(ebert=="con")
symm2=2*(siskel=="mixed")*(ebert=="mixed")
symm5=5*(siskel=="mixed")*(ebert=="pro")+5*(siskel=="pro")*(ebert=="mixed")
symm=symm3+symm1+symm4+symm6+symm2+symm5

### generate dataset movies
movies=data.frame(siskel,ebert,count,icon,imixed,ipro,symm)

### Contigency Table
table=xtabs(count~siskel+ebert)
Contingency_Table=list(Frequency=table,Percent=prop.table(table),RowPct=prop.table(table,1),ColPct=prop.table(table,2))
Contingency_Table
result=chisq.test(table,correct=FALSE)

### Simple Kappa Coefficient
### Using a function Kappa() in package vcd. 
### Please first load packages VR, colorspace and grid and finally load vcd.
library(vcd)
kappa=Kappa(table)
kappa

options(contrast=c("contr.treatment","contr.poly"))
### Independence Model

model=glm(count~siskel+ebert,family=poisson(link=log))
summary(model)

### Quasi-Independence Model

model=glm(count~siskel+ebert+icon+imixed+ipro,family=poisson(link=log))
summary(model)

### Symmetry Model

model=glm(count~symm1+symm4+symm6+symm2+symm5+symm3,family=poisson(link=log))
summary(model)

### Quasi-Symmetry Model

model=glm(count~siskel_con+siskel_mixed+siskel_pro+ebert_con+ebert_mixed+ebert_pro+ebert
          +symm1+symm4+symm6+symm2+symm5+symm3,family=poisson(link=log))
summary(model)