### read the data file. Please put water_level.txt under the default directory
data=read.table("water_level.txt") 
colnames(data)=c("obs","y","sex","gravity","totphys","bryant","vander","triangle","trailer","tree","comphys","moving","total")

### table sex by y
count=table(data$sex,data$y)
Contingency_Table=list(Frequency=count,Percentage=prop.table(count),RowPercentage=prop.table(count,1),ColPercentage=prop.table(count,2))
Contingency_Table

### table gravity by y
count=table(data$gravity,data$y)
Contingency_Table=list(Frequency=count,Percentage=prop.table(count),RowPercentage=prop.table(count,1),ColPercentage=prop.table(count,2))
Contingency_Table

### table totphys by y
count=table(data$totphys,data$y)
Contingency_Table=list(Frequency=count,Percentage=prop.table(count),RowPercentage=prop.table(count,1),ColPercentage=prop.table(count,2))
Contingency_Table
