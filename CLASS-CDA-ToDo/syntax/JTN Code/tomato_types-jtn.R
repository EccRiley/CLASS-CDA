#### Lesson 2 Cut-Leaf example
#### (I) Basic GOF line by line calculation 
#### (II) Doing GOF with chisq.test() 
#### (III) Nice R code that corresponds to SAS code and output
#########################################################
#' [Video tutorial](https://onlinecourses.science.psu.edu/stat504/sites/onlinecourses.science.psu.edu.stat504/files/lesson02/Lec6_Tomatoes_viewlet_swf.html)

##(I) Basic GOF line by line computation to demonstrate formulas
ob=c(926,288,293,104) ## data
ex=1611*c(9,3,3,1)/16  ## expected counts under the assumed model

X2=sum((ob-ex)^2/ex) ## X2 statistic
X2
####  Output so you check against your output
#[1] 1.468722

1-pchisq(X2,3) ## p-value
####  Output
#[1] 0.6895079

G2=2*sum(ob*log(ob/ex)) ## deviance statistic
G2
####  Output
#[1] 1.477587

1-pchisq(G2,3) ## pvalue

####  Output
#[1] 0.6874529

########################################################
#### (II) Using the built-in chisq.test function in R
tomato=chisq.test(c(926, 288,293,104), p=c(9/16, 3/16, 3/16, 1/16))
 tomato
 tomato$statistic
 tomato$p.value 
 tomato$residuals
 
#### To get G2
 
G2=2*sum(tomato$observed*log(tomato$observed/tomato$expected))
G2
1-pchisq(G2,3)

##deviance residuals
devres=sign(tomato$observed-tomato$expected)*sqrt(abs(2*tomato$observed*log(tomato$observed/tomato$expected)))
devres

##### Creating a nice ouput 
### Cell id, Observed count, Expected count, Pearson residuals, Deviance residual
out<-round(cbind(1:5, tomato$observed, tomato$expected, tomato$residuals, devres),3)
out<-as.data.frame(out)
names(out)<-c("cell_j", "O_j", "E_j", "res_j", "dev_j")
out

#### printing your table of results into a text file with tab separation
write.table(out, "tomato_Results", row.names=FALSE, col.names=TRUE, sep="\t")

#### Ploting expected and observed values
plot(c(1:4), ex$observed, xlab="cell index", ylab="counts", xlim=c(0,5))
points(ex$expected, pch=3, col="red")
legend(3,700, c("observed", "expected"), col=c(1,"red"), pch=c(1,3))


########################################################
#### (III) Nice R code that corresponds to SAS code and output
type=c(rep("tallc",926),rep("tallp",288),rep("dwarf",293),rep("dwarfp",104))
##Please note the table R provides has different order of rows 
##from that provided by SAS 
table(type)
## Freq Procedure
Percentage=100*as.vector(table(type))/sum(table(type))
CumulativeFrequency=cumsum(c(926,288,293,104))
CumulativePercentage=cumsum(Percentage)
Freq=data.frame(table(type),Percentage,CumulativeFrequency,CumulativePercentage)
Freq
## Chi-Square Test for Specified Proportions
p=c(18.75,6.25,56.25,18.75)
chisq.test(table(type),p=p/sum(p))
########################################################


