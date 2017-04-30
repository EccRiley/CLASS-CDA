#################################
#### Death Penalty Example: a 2x2x2 table
#### let A=defendant, B=vicitim, C=penalty 
#### A simple line by line analysis
#### Nice R code that corresponds to SAS output
###############################################
install.packages("vcd")
library(vcd)

#### reading the data into  a table
#### first step is just a vector of values
deathp <- c(19,132, 11,52,0,9, 6,97)
deathp

#### we can represent this table also in 3 dimensions
deathp <- array(deathp, dim=c(2,2,2))
deathp
dimnames(deathp) <- list(DeathPen=c("yes","no"),
                     Defendant=c("white","black"),
                     Victim=c("white","black"))
deathp
#### create a flat contingency table
ftable(deathp, row.vars=c("Defendant","Victim"),
col.vars="DeathPen")

### you can get a same as above
ftable(deathp, row.vars=c(1,3), col.vars=2)


##### test of mutual independence 

### these are the marginal counts
penalty<-margin.table(deathp,1)
defand<-margin.table(deathp, 2)
victim<-margin.table(deathp,3)

### expected value under the mutual independence model
deathexp<-(defand%o%victim%o%penalty)/(sum(deathp)^2)
deathexp

### chi-squared statistic
chisqr<-sum(((deathexp-deathp)^2)/deathexp)
1-pchisq(chisqr,4)


####### test of mutual independence via marginal tables
### recall that this means that we need to test that ALL odds ratios are equal to 1
### since this is a 2x2x2 table and all marginal tables are 2x2, then we can just 
### do the chi-square test of independence 
### let A=defendant, B=vicitim, C=penalty 

AB<-margin.table(deathp, 2:3)
AC<-margin.table(deathp, c(2,1))
BC<-margin.table(deathp, c(3,1))

chisq.test(AB)
chisq.test(AC)
chisq.test(BC)

### compute the odds ratios for AC table for example
## you can use any functions we have seen previously for two-way tables
assocstats(AC)
oddsratio(AC, log=FALSE)
exp(confint(oddsratio(AC)))

### test of conditional independence
### Cochran-Mantel-Haenszel test)
mantelhaen.test(deathp)
mantelhaen.test(deathp,correct=FALSE)

### via odds-ratios
oddsratio(deathp, 3, log=FALSE)
##log odds ratio for a 2x2 table given the levels of the 3rd variable
lor=oddsratio(deathp,3) 
exp(confint(lor)) ## CI 
summary(lor)
plot(lor, xlab="Victim")


## getting the the AC table for B=Victim=white
deathp[,,1]  ## getting the the AC table for B=Victim=white
oddsratio(deathp[,,1], log=FALSE)  ## estimate of a conditional OR
exp(confint(oddsratio(deathp[,,1]))) ## CI of a conditional OR
chisq.test(deathp[,,1], correct=FALSE)

  
deathp[,,2] ## getting the the AC table for B=Victim=black
##In case of zero entries, 0.5 will be added to the table.
oddsratio(deathp[,,2], log=FALSE) ## estimate of a conditional OR
exp(confint(oddsratio(deathp[,,2]))) ## CI of a conditional OR
chisq.test(deathp[,,2])

#########################################
### Here as another way to handle this same data
defendant=rep(c("white","black"),c(4,4))
victim=rep(rep(c("white","black"),c(2,2)),2)
penalty=rep(c("yes","no"),4)
count=c(19,132,0,9,11,52,6,97)

### Table of defendant by penalty
data=xtabs(count~defendant+penalty)
table=list(Frequency=data,RowPct=prop.table(data,1))
table
result=chisq.test(data)
result

### Table of defendant by victim
data=xtabs(count~defendant+victim)
table=list(Frequency=data,Percent=prop.table(data),RowPct=prop.table(data,1),ColPct=prop.table(data,2))
table
result=chisq.test(data)
summary(result)
result

### Table of victim by penalty
data=xtabs(count~victim+penalty)
table=list(Frequency=data,Percent=prop.table(data),RowPct=prop.table(data,1),ColPct=prop.table(data,2))
table
result=chisq.test(data)
summary(result)
result

### Table1 of defendant by penalty given victim
data=xtabs(count~penalty+defendant+victim)
data=addmargins(data,3)[,,1]
table=list(Frequency=data,RowPct=prop.table(data,1))
table
result=chisq.test(data)
result

oddsratio(table)


### Table1 of defendant by penalty
data3=xtabs(count~penalty+defendant+victim)
data=addmargins(data3,3)[,,2]
table=list(Frequency=data,,RowPct=prop.table(data,1))
table
result=chisq.test(data)
result

### Mantel-Haenszel Test on victim*defendant*penalty
mantelhaen.test(data3)

### Test for homogenous associations:
### This is not a built in R function. Make sure you compile this function (i.e., breslowday.test(), copy it into R) from the course site first 
breslowday.test(deathp)



