#clear active frame from previous analyses
rm(mydata)

library(lessR)
#replace with your file path
mydata = Read("c:/jason/spsswin/cdaclass/race.sav",quiet=TRUE)

#if need to convert data types in order to compute a correlation in R--lessR function
mydata <-Transform(driver = as.numeric(driver))
mydata <- Recode(driver, old=1:2, new=seq(0,1,by=1))

SummaryStats(driver)
