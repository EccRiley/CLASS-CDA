#############################
#### Berkeley admissions data for log-linear models
#### See also berkeley.R for a different code 
#### R code that matches SAS default setting
#### See also related berkeleyLoglin.R 
#############################

#### Reading in the text files instead of using dataset from R 
#### Make sure you specify the correct path on your computer
#### You can also read it directly from the internet, if you know 
#### the internet address and you are connected.
#### berkeley=read.table("http://onlinecourses.science.psu.edu/stat504/sites/onlinecourses.science.psu.edu.stat504/files/lesson05/berkeley.txt")

berkeley=read.table("berkeley.txt")
colnames(berkeley)=c("D","S","A","count")
berkeley

#### Here are some different ways to create tables
#### Table  A*D*S

temp=xtabs(berkeley$count~berkeley$D+berkeley$S+berkeley$A)
temp
xtabs(berkeley$count~., data=berkeley)


#### Table 1-6 of S*A

temp=xtabs(berkeley$count~berkeley$S+berkeley$A+berkeley$D)
temp

#### Table of S*A withOUT conditioning on berkeley$D

table=addmargins(temp)[1:2,1:2,7]

#### Please repeat doing all the following steps for all tables of S*A: temp[,,1]-temp[,,6]
#### ----------------------------------------------------------------------------
table=temp[1:2,1:2,1]

#table=temp[1:2,1:2,2]
#table=temp[1:2,1:2,3]
#table=temp[1:2,1:2,4]
#table=temp[1:2,1:2,5]
#table=temp[1:2,1:2,6]

#### Chi-Square Test for Table 1 of s*A
#### temp[,,i] denotes the ith table

result=chisq.test(table,correct=FALSE)
result$expected
Table_SA6=list(Frequency=table,Expected=result$expected,Percent=prop.table(table))

#### Fisher Exact Test for Table 1 of s*A

Fisher_Exact_TwoSided=fisher.test(table,alternative = "two.sided")
Fisher_Exact_Less=fisher.test(table,alternative = "less")
Fisher_Exact_Greater=fisher.test(table,alternative = "greater")
list(Fisher_Exact_TwoSided=Fisher_Exact_TwoSided,Fisher_Exact_Less=Fisher_Exact_Less,Fisher_Exact_Greater=Fisher_Exact_Greater)

#### Relative Risk for Table 1 of s*A

RowSums=rowSums(table)
ColSums=colSums(table)

#### Estimate of the Odds of the two rows

odds1=(table[2,1]/RowSums[2])/(table[1,1]/RowSums[1])
odds2=(table[2,2]/RowSums[2])/(table[1,2]/RowSums[1])
odds1
odds2

#### Odds Ratio

oddsratio=odds2/odds1
oddsratio

#### Confidence Interval of the odds ratio

log_CI=cbind(log(oddsratio)-qnorm(0.975)*sqrt(sum(1/table)),log(oddsratio)+qnorm(0.975)*sqrt(sum(1/table)))
CI_oddsratio=exp(log_CI)
CI_oddsratio


#### Table of D*S if A=Accept

Table_ADS=list(Frequency=temp[,,1],Expected=temp[,,1]$expected,Percent=prop.table(temp[,,1]))
Table_ADS

#### Table of D*S if A=Reject

Table_ADS=list(Frequency=temp[,,2],Expected=temp[,,2]$expected,Percent=prop.table(temp[,,2]))
Table_ADS

#### Table of D*S withOUT conditioning on berkeley$A

addmargins(temp)
table=addmargins(temp)[1:6,1:2,3]
result=chisq.test(table,correct=FALSE)
Contigency_Table=list(Frequency=table,Expected=chisq.test(table)$expected,Percent=prop.table(table),RowPCt=prop.table(table,1),ColPct=prop.table(table,2))
Contigency_Table

#### /* joint independence of D and S from A*/

D=factor(berkeley$D)
S=factor(berkeley$S)
A=factor(berkeley$A)
model=glm(berkeley$count~D+S+A+D*S,family=poisson(link=log))
summary(model)


#### -----------------------------------------------------------------------
#### To set up the same coding that is in default SAS
#### First we set the type of contrast to treatment contrasts for factors

options(contrast=c("contr.treatment","contr.poly"))
D=berkeley$D
S=berkeley$S
A=berkeley$A
count=berkeley$count

#### Notice that R uses base level Department A,Female and Accept which is different from SAS.
#### Complete Independence

temp=glm(count~D+S+A,family=poisson(link=log))
temp

####/* joint independence of D and S from A*/

temp=glm(count~D+S+A+D*S,family=poisson(link=log))
temp

#### /*conditional independence of D and A given S */

temp=glm(count~D+S+A+D*S+A*S,family=poisson(link=log))
temp

#### /*homogeneous associations */

temp=glm(count~D+S+A+D*S+D*A+A*S,family=poisson(link=log))
temp

####  /*saturated model */

temp=glm(count~D+S+A+D*S+D*A+S*A+D*S*A,family=poisson(link=log))
temp
