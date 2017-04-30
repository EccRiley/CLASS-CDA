##### World Cup Soccer Data 'soccer.txt" ######
#' [Video tutorial](https://onlinecourses.science.psu.edu/stat504/sites/onlinecourses.science.psu.edu.stat504/files/lesson02/Soccer_viewlet_swf.html)

## read the datafile into R
## header=TRUE will take the first row of the data as the label for the columns
soccer<-read.table("soccer2002.txt", header=TRUE) 
soccer

## compute the sample size 
sampleN=sum(soccer$Freq)
sampleN

## compute the sample mean 
## this is also the MLE of lambda
smean=(1/sampleN)*(sum(soccer$Goals*soccer$Freq))
smean

## compute the Poisson probability for X=1, P(X=1) 
dpois(1, lambda=smean)

## Poisson probabilities for X=0, 1, ..., 8 with LAMBDA=smean 
## these are estimates of cell probabilities 
pihat<-dpois(0:8, lambda=smean)
pihat


## compute expected frequencies for each cell j, E(Xj)=n pi_j
efreq<-sampleN*pihat
efreq

## plot observed and expected frequencies versus the number of goals scored, but save direclty into a file "soccerCounts.pdf"###
## if you do NOT want to save into the file direclty, remove the first and the last line of this section of the code ####
pdf("soccerCounts.pdf")
plot(soccer$Goals, soccer$Freq, type="p",col="red", xlab="goals scored",ylab="frequencies",main="World Cup Soccer data")
points(soccer$Goals, efreq, col="blue", pch=22)
legend(list(x=4,y=30), legend=c("Observed count", "Expeced count"), pch=21:22, col=c("red", "blue"))
dev.off()

########### Goodness of Fit ###############################
### (I) Goodness of fit ignoring LAMBDA estimate ####

## this tests if the observed values fit a model that all cells are equally likley
ex<-chisq.test(soccer$Freq)
ex
ex$statistic ##gives the Chi-Squared test statistic only
ex$p.value ## gives the pvalue of the above statistic only
ex$residuals ## gives the Pearson residuals

out<-round(cbind(0:8, ex$observed, ex$expected, ex$residuals),3)
out<-as.data.frame(out)
names(out)<-c("goals", "O_j", "E_j", "res_j")
out

## to get G2, but the problem is devision by zero
 
G2<-2*sum(ex$observed*log(ex$observed/ex$expected))
G2
1-pchisq(G2,8)

## deviance residuals
devres=sqrt(abs(2*ex$observed*log(ex$observed/ex$expected)))*sign(ex$observed-ex$expected)

##one solution add a small count such as 1/2 to each cell
ex1=chisq.test(soccer$Freq+0.5)
ex1
G2<-2*sum(ex1$observed*log(ex1$observed/ex1$expected))
G2
1-pchisq(G2,8)

##deviance residuals
devres=sqrt(abs(2*ex1$observed*log(ex1$observed/ex1$expected)))*sign(ex1$observed-ex1$expected)

## another (less common) solutions is to do the calculations with all values greater than zero for G2
## but that fixes the zero cell counts to so we are estimating two less paramaters in this example
## thus the degrees of freedom should be adjusted
##option na.rm=TRUE removes all zero values from the calculation

G2<-2*sum(ex$observed*log(ex$observed/ex$expected), na.rm=TRUE)
G2
1-pchisq(G2,6)



### (II) Goodness of fit using LAMBDA estimate ####
## This tests if the observed values fit the Poisson model with estimated/given LAMBDA ###

ex2<-chisq.test(soccer$Freq, p=pihat, rescale.p=TRUE)

##Notice the warningn msg. That is because there are some expected probabilities which are very small, e.g. less than 0.2; see zero cell counts. ##

ex2
ex2$statistic
ex2$p.value 
ex2$residuals


######## What's the problem here??? ###### 
## DFs (9-1) are not correct, since we first estimated lambda and then pihats
## DFs=(9-1)-1=7

1-pchisq(ex2$statistic,7)

out<-round(cbind(0:8, ex2$observed, ex2$expected, ex2$residuals),3)
out<-as.data.frame(out)
names(out)<-c("goals", "O_j", "E_j", "res_j") 
out   ##notice that the model actually fits well except for the last cell with a very large residual


##### NEW G2 to do the calculations with all values greater than zero; option na.rm=TRUE removes all zero values from the calculation

G2<-2*sum(ex2$observed*log(ex2$observed/ex2$expected), na.rm=TRUE)
G2
1-pchisq(G2,7)
