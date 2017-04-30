#### Example: French skiers

ski<-matrix(c(310, 170, 1090, 1220), ncol=2, dimnames=list(Treatment=c("Placebo", "VitaminC"), Cold=c("Cold", "NoCold")))
ski

#### Let us look at the Percentage, Row Percentage and Column Percentages 
#### of the total observations contained in each cell.

Percentage=100*ski/sum(ski)
RowSums=rowSums(ski)
RowPercentage=100*rbind(ski[1,]/RowSums[1],ski[2,]/RowSums[2])
ColSums=colSums(ski)
ColPercentage=100*cbind(ski[,1]/ColSums[1],ski[,2]/ColSums[2])
Percentage
RowPercentage
ColPercentage

#### Pearson's Chi-squared test  with Yates' continuity correction

result<-chisq.test(ski)
result

#### Let's look at the observed, expected values and the residuals

result$observed
result$expected
result$residuals

#### Pearson's Chi-squared test  withOUT Yates' continuity correction

result<-chisq.test(ski, correct=FALSE)
result
result$observed
result$expected
result$residuals

#### Likelihood Ratio Chi-Squared Statistic

chisq=1-pchisq(2*sum(ski*log(ski/result$expected)),df=1)
chisq

#### Column 1 Risk Estimates

risk1_col1=ski[1,1]/RowSums[1]
risk2_col1=ski[2,1]/RowSums[2]
rho1=risk1_col1/risk2_col1
total1=ColSums[1]/sum(RowSums)
diff1=risk2_col1-risk1_col1
rbind(risk1_col1,risk2_col1,total1,diff1)

#### The confidence interval for the difference in proportions for column 1

SE_diff1=sqrt(risk1_col1*(1-risk1_col1)/RowSums[1]+risk2_col1*(1-risk2_col1)/RowSums[2])
CI_diff1=cbind(diff1-qnorm(0.975)*SE_diff1,diff1+qnorm(0.975)*SE_diff1)
SE_diff1
CI_diff1

#### Column 2 Risk Estimates

risk1_col2=ski[1,2]/RowSums[1]
risk2_col2=ski[2,2]/RowSums[2]
total2=ColSums[2]/sum(RowSums)
diff2=risk2_col2-risk1_col2
rbind(risk1_col2,risk2_col2,total2,diff2)

#### The confidence interval for the difference in proportions for column 2

SE_diff2=sqrt(risk1_col2*(1-risk1_col2)/RowSums[1]+risk2_col2*(1-risk2_col2)/RowSums[2])
CI_diff2=cbind(diff2-qnorm(0.975)*SE_diff2,diff2+qnorm(0.975)*SE_diff2)
SE_diff2
CI_diff2

#### Estimate of the Odds of the two rows

odds1=(ski[2,1]/RowSums[2])/(ski[1,1]/RowSums[1])
odds2=(ski[2,2]/RowSums[2])/(ski[1,2]/RowSums[1])

#### Odds Ratio

oddsratio=odds1/odds2
odds1
odds2
oddsratio

#### Confidence Interval of the odds ratio

log_CI=cbind(log(oddsratio)-qnorm(0.975)*sqrt(sum(1/ski)),log(oddsratio)+qnorm(0.975)*sqrt(sum(1/ski)))
CI_oddsratio=exp(log_CI)
CI_oddsratio

#### Fisher's Exact Test

Fisher_Exact_TwoSided=fisher.test(ski,alternative = "two.sided")
Fisher_Exact_Less=fisher.test(ski,alternative = "less")
Fisher_Exact_Greater=fisher.test(ski,alternative = "greater")
rbind(Fisher_Exact_TwoSided,Fisher_Exact_Less,Fisher_Exact_Greater)