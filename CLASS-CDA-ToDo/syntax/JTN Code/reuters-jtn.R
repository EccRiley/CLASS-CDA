#clear active frame from previous analyses
#rm(mydata)

#need the following line on first use
# install.packages('lessR')
library(lessR)
mydata = Read("data/reuters.sav",quiet=TRUE)

#if need to convert data types in order to compute a correlation in R--lessR function
mydata <-Transform(response = as.numeric(response))
#undo default numeric values 1,2,3 and label values
mydata <- Recode(response, old=1:3, new=seq(0,2,by=1))

mydata <- Subset(response<2)
str(mydata)
#mydata <- rec(Severity, old=1:4, new=seq(10,40,by=10))

#for binomial proportion test, first find the proportion of Clinton voters
SummaryStats(response)
#then enter in the prop.test(x,n,p,continuity correction option)
prop.test(677, 1231, p=0.5, correct=FALSE)

#this lessR BarChart function produces a chi-square test by default
BarChart(response,xlab="Response: Trump=0, Clinton=1")
