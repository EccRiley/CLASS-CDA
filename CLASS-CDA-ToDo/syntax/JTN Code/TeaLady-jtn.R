#### Tea Tasting ####
#### Fisher's Exact test a quick solution in R ####
#### More details on this example corresponding to SAS output ###
##########################################

#### data entry 

tea<- matrix(c(3, 1, 1, 3), ncol= 2, dimnames = list(Truth = c("Tea","Milk" ),Lady_says = c("Tea first","Milk first")))

#### one-sided Fisher's exact test

fisher.test(tea, alternative = "greater")

#### two-sided Fisher's exact test

fisher.test(tea)

#### OUTPUT for one-sided test #########
#### Fisher's Exact Test for Count Data
####
#### data:  tea 
#### p-value = 0.2429
#### alternative hypothesis: true odds ratio is greater than 1 
#### 95 percent confidence interval:
#### 0.3135693       Inf 
#### sample estimates:
#### odds ratio 
#### 6.408309 

######## CONCLUSION ########
#### We cannot reject the null hypothesis, that is there is not enough evidence to establish association

###### What is the conclusion for two-sided test? ##############
################################################################

#### Another approach ####

TeaLady=matrix(c(3, 1, 1, 3), ncol=2, dimnames=list(Poured=c("tea", "milk"), Lady=c("tea", "milk")))
TeaLady

#### Pearson's Chi-squared test with Yates' continuity correction

result=chisq.test(TeaLady)
result

#### Let's look at the observed, expected values and the residuals

result$observed
result$expected
result$residuals


#### Let us look at the Percentage, Row Percentage and Column Percentage 
#### of the total observations contained in each cell.

Contingency_Table=list(Frequency=TeaLady,Expected=result$expected,Percentage=prop.table(TeaLady),RowPercentage=prop.table(TeaLady,1),ColPercentage=prop.table(TeaLady,2))

#### Pearson's Chi-squared test  withOUT Yates' continuity correction

result=chisq.test(TeaLady, correct=FALSE)
result
result$observed
result$expected
result$residuals

#### Likelihood Ratio Chi-Squared Statistic

LR=2*sum(TeaLady*log(TeaLady/result$expected))
LR

#### p-value for the Likelihood Ratio Test

LRchisq=1-pchisq(LR,df=1)
LRchisq

#### Fisher's Exact Test

Fisher_Exact_TwoSided=fisher.test(TeaLady,alternative = "two.sided")
Fisher_Exact_Less=fisher.test(TeaLady,alternative = "less")
Fisher_Exact_Greater=fisher.test(TeaLady,alternative = "greater")
list(Fisher_Exact_TwoSided=Fisher_Exact_TwoSided,Fisher_Exact_Less=Fisher_Exact_Less,Fisher_Exact_Greater=Fisher_Exact_Greater)

RowSums=rowSums(TeaLady)
ColSums=colSums(TeaLady)

#### Column 1 Risk Estimates

risk1_col1=TeaLady[1,1]/RowSums[1]
risk2_col1=TeaLady[2,1]/RowSums[2]
rho1=risk1_col1/risk2_col1
total1=ColSums[1]/sum(RowSums)
diff1=risk1_col1-risk2_col1
list(Row1_Risk=risk1_col1,Row2_Risk=risk2_col1,Total=total1,difference=diff1)

#### The confidence interval for the difference in proportions for column 1

SE_diff1=sqrt(risk1_col1*(1-risk1_col1)/RowSums[1]+risk2_col1*(1-risk2_col1)/RowSums[2])
CI_diff1=cbind(diff1-qnorm(0.975)*SE_diff1,diff1+qnorm(0.975)*SE_diff1)
SE_diff1
CI_diff1

#### Column 2 Risk Estimates

risk1_col2=TeaLady[1,2]/RowSums[1]
risk2_col2=TeaLady[2,2]/RowSums[2]
total2=ColSums[2]/sum(RowSums)
diff2=risk1_col2-risk2_col2
list(Row1_Risk=risk1_col1,Row2_Risk=risk2_col1,Total=total1,difference=diff2)

#### The confidence interval for the difference in proportions for column 2

SE_diff2=sqrt(risk1_col2*(1-risk1_col2)/RowSums[1]+risk2_col2*(1-risk2_col2)/RowSums[2])
CI_diff2=cbind(diff2-qnorm(0.975)*SE_diff2,diff2+qnorm(0.975)*SE_diff2)
SE_diff2
CI_diff2

#### Estimate of the Odds of the two rows

odds1=(TeaLady[2,1]/RowSums[2])/(TeaLady[1,1]/RowSums[1])
odds2=(TeaLady[2,2]/RowSums[2])/(TeaLady[1,2]/RowSums[1])
odds1
odds2

#### Odds Ratio

oddsratio=odds2/odds1
oddsratio

#### Confidence Interval of the odds ratio

log_CI=cbind(log(oddsratio)-qnorm(0.975)*sqrt(sum(1/TeaLady)),log(oddsratio)+qnorm(0.975)*sqrt(sum(1/TeaLady)))
CI_oddsratio=exp(log_CI)
CI_oddsratio

