### Example: Smoking Behaviors Lesson 3 ##
### Simple line by line R code
### Needs LRStats.R and Gamma.f.R functions too
### Uses {VCD} package to plot the expected counts and residuals
### Nice R code that corresponds to SAS code and output
#######################################################

smoke=matrix(c(400,416,188,1380,1823,1168), ncol=2, dimnames=list(parent=c("both", "one","neither"), child=c("yes", "no")))
smoke

#### Chi-Square Independence Test

result=chisq.test(smoke)
result

#### Let us look at the Percentage, Row Percentage and Column Percentage 
#### of the total observations contained in each cell.

Contingency_Table=list(Frequency=smoke,Expected=result$expected,Percentage=prop.table(smoke),RowPercentage=prop.table(smoke,1),ColPercentage=prop.table(smoke,2))
Contingency_Table

#### Likelihood Ratio Test
LRstats(smoke)

#### a function assocstats in package vcd that computes these association measures 
#### along with the Pearson and LR chi-square tests. If this doesn't run then you 
#### need to install package 'colorspace' too. 
library(vcd)

## produces independence statistics 
assocstats(smoke)

result$expected ## expected counts; compare with the plot below

##plot an area proportional visualization of a (possibly higher-dimensional) table of expected frequencies.
mosaic(smoke)
#produce Pearson residuals then compare to the plot below
result$residuals

##produce an association plot indicating deviations in terms of Pearson residuals from a specified independence model in possibly high-dimensional contingency table.
assoc(smoke) 
#### A function for computing Goodman and Kruskal's gamma adapted 
#### from S-Plus Manual to Accompany Agresti's Categorical Data Analysis(Laura A. Thompson 2001).
Gamma.f(smoke)
