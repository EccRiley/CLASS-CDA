###### Dice Rolls from Lesson 2: one-way tables & GOF 
#' [Video tutorial](https://onlinecourses.science.psu.edu/stat504/sites/onlinecourses.science.psu.edu.stat504/files/lesson02/Dice_Rolls_R_viewlet_swf.html)
##### Line by line calculations in R
##### Nice R code that corresponds to SAS code and output
##########################################################

### if you want all output into a file use: sink("dice_roll.out")
sink("dice_roll.out")

### run a goodness of fit test
dice<- chisq.test(c(3,7,5,10,2,3))
dice

########OUTPUT gives Pearson chi-squared 
#	Chi-squared test for given probabilities
#
#  data:  c(3, 7, 5, 10, 2, 3) 
#  X-squared = 9.2, df = 5, p-value = 0.1013
########

### to get observed values
dice$observed


### to get expected values
dice$expected

### to get Pearson residuals
dice$residuals


#####Make the output print into a nice table ######
#### creating a table and giving labels to the columns 
out<-round(cbind(1:6, dice$observed, dice$expected, dice$residuals),3)
out<-as.data.frame(out)
names(out)<-c("cell_j", "O_j", "E_j", "res_j")

### printing your table of results into a text file with tab separation
write.table(out, "dice_rolls_Results", row.names=FALSE, col.names=TRUE, sep="\t")


#########TO GET Deviance statistic and it's p-value
G2=2*sum(dice$observed*log(dice$observed/dice$expected))
G2
1-pchisq(G2,5)


##deviance residuals
devres=sign(dice$observed-dice$expected)*sqrt(abs(2*dice$observed*log(dice$observed/dice$expected)))
devres

##to show that the G2 is a sum of deviance residuals
G2=sum(sign(dice$observed-dice$expected)*(sqrt(abs(2*dice$observed*log(dice$observed/dice$expected))))^2)
G2

########## If you want to specify explicitly the vector of probabilities 
dice1<-chisq.test(c(3,7,5,10,2,3), p=c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6))
dice1

############################################################
#### Nice R code that corresponds to SAS code and its output
## vector "face" records the face of the dice you get every time you roll it.
face=c(rep(1,3),rep(2,7),rep(3,5),rep(4,10),rep(5,2),rep(6,3))
## Freq Procedure
Percentage=100*as.vector(table(face))/sum(table(face))
CumulativeFrequency=cumsum(c(3,7,5,10,2,3))
CumulativePercentage=cumsum(Percentage)
Freq=data.frame(table(face),Percentage,CumulativeFrequency,CumulativePercentage)
row.names(Freq)=NULL
Freq
## Chi-Square Test for Equal Proportions
chisq.test(table(face))


### if you used sink("dice_roll.out"), and now want to see the output inside the console use: sink()
sink()