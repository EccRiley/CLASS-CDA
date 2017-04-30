#clear active frame from previous analyses
rm(mydata)

#need the following line on first use
# install.packages('lessR')
library(lessR)
mydata = Read("c:/jason/spsswin/cdaclass/proficient.sav",quiet=TRUE)

#if need to convert data types in order to compute a correlation in R--lessR function
mydata <-Transform(level = as.numeric(level))
#undo default numeric values 1,2,3 and label values
mydata <- Recode(level, old=1:4, new=seq(0,3,by=1))

BarChart(level)
