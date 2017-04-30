#clear active frame from previous analyses
rm(mydata)

library(lessR)
#replace with your file path
mydata = Read("c:/jason/spsswin/cdaclass/child.sav",quiet=TRUE)
