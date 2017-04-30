### Example: Heart Disease Example Lesson 3 ##
### Simple line by line R code
### Nice R code that corresponds to SAS code and output
#######################################################

## enter data
heart <-c(12,8,31,41,307,246,439,245) 
heart<-matrix(heart,4,2) 
heart=t(heart)

## run the chi-squared test of independence & save it into a new object
result<-chisq.test(heart) 
result

## Let's look at the obseved, expected values and the residuals
result$observed
result$expected
result$residuals

### Likelihood Ratio Test
LR=2*sum(heart*log(heart/result$expected))
LR
LRchisq=1-pchisq(LR,df=(4-1)*(2-1))
LRchisq


 ##make sure you have function LRstats()
 LRstats(heart)

 
 ## Let's calculate the conditional probabilities
 ## the following function gives the desired marginal, in this case, the counts for the serum groups
serum<-margin.table(heart,2)
serum 

## let's look at the counts for the four groups with CHD
heart[1,]

## then counts for the four groups with NOCHD, which is the second column of data in the dataframe we created above
heart[2,]

### conditional probabilities are:
heart[2,]/serum
heart[1,]/serum

########################################
### Nice R code that corresponds to SAS code and output
#######################################################
heart=matrix(c(12,307,8,246,31,439,41,245), ncol=4, dimnames=list(CHD=c("chd", "nochd"), serum=c("0-199", "200-199","220-259","260+")))
heart
count=heart

### Chi-Square Independence Test
result=chisq.test(count)
result$expected

### Let us look at the Percentage, Row Percentage and Column Percentage 
### of the total observations contained in each cell.

Contingency_Table=list(Frequency=count,Expected=result$expected,Deviation=count-result$expected,Percentage=prop.table(count),RowPercentage=prop.table(count,1),ColPercentage=prop.table(count,2))
Contingency_Table

###### Computing various measures of association
library(vcd)
assocstats(heart)

### For the Pearson correlation coefficent 
### and Mantel-Haenszel, 
### for IxJ tables, you can also use 
### pears.cor() function. 
### Mak sure you run this function first!
### c(1,2) and c(1,2,3,4), are the vectors of score values 
pears.cor(heart, c(1,2),c(1,2,3,4)) 
### and this should give you, r=-0.14, M2=26.1475

##Gamma 
Gamma.f(heart)
