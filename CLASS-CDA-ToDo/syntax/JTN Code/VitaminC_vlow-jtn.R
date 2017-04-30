#### Example: French skiers

#### Here is one way to read the data vector of values with labels for the table

ski<-matrix(c(3, 11, 2, 12), ncol=2, dimnames=list(Treatment=c("Placebo", "VitaminC"), Cold=c("Cold", "NoCold")))
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

#### Pearson's Chi-squared test with Yates' continuity correction

result<-chisq.test(ski)
result

#### Let's look at the obseved, expected values and the residuals

result$observed
result$expected
result$residuals

#### Pearson's Chi-squared test  WITHOUT Yates' continuity correction

result<-chisq.test(ski, correct=FALSE)
result
result$observed
result$expected
result$residuals

#### Likelihood Ratio Chi-Squared Statistic

chisq=1-pchisq(2*sum(ski*log(ski/result$expected)),df=1)
chisq

#### Fisher's Exact Test

Fisher_Exact_TwoSided=fisher.test(ski,alternative = "two.sided")
Fisher_Exact_Less=fisher.test(ski,alternative = "less")
Fisher_Exact_Greater=fisher.test(ski,alternative = "greater")
rbind(Fisher_Exact_TwoSided,Fisher_Exact_Less,Fisher_Exact_Greater)



