CHD=rep(c("chd","nochd"),c(4,4))
serum=rep(c("0-199","200-199","220-259","260+"),2)
CHD=factor(CHD)
serum=factor(serum)
count=c(12,8,31,41,307,246,439,245)
xCHD=rep(c(1,2),c(4,4))
yserum=rep(1:4,2)

#### Contingency_Table

table=xtabs(count~CHD+serum)
Contingency_Table=list(Frequency=table,Percent=prop.table(table),RowPct=prop.table(table,1),ColPct=prop.table(table,2))
Contingency_Table

#### CHM statistic for linear association
options(contrast=c("contr.treatment","contr.poly"))

#### Independence Model

model=glm(count~CHD+serum,family=poisson(link=log))
summary(model)

#### Linear by Linear Association Model

model=glm(count~CHD+serum+xCHD*yserum,family=poisson(link=log))
summary(model)