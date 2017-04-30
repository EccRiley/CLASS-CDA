
########################################################
#### Number of Children Example
#### Basic Poisson calculations broken down line by line 
#### Nicer R code that corresponds to SAS code and its ouput
#########################################################

#### input data 
ob<-c(19,26,29,13,13) 
ob

# [1] 19 26 29 13 13


#### find estimated expected probabilities 

lambdahat<-c(19*0+26*1+29*2+13*3+10*4+3*5)/100 
lambdahat

# [1] 1.78 


kids<-c(0,1,2,3) 

pihat<-dpois(kids,lambdahat) 
pihat 

# [1] 0.1686381 0.3001759 0.2671566 0.1585129 


#### attach the probability for the 4+ cell 

pihat<-c(pihat,1-sum(pihat)) 
ex<-100*pihat 
X2<-sum((ob-ex)^2/ex) 
X2 

# [1] 2.084625 

G2<-2*sum(ob*log(ob/ex)) 
G2 

# [1] 2.088668 


#### find the p-value for X^2

1-pchisq(X2,3)  

# [1] 0.5550296 



#### find the p-value for G^2 

1-pchisq(G2,3) 

# [1] 0.5542087 

#############################################################
#### Nicer R code that corresponds to SAS code and its ouput

children=c(rep("0",19),rep("1",26),rep("2",29),rep(3,13),rep("4+",13))

#### Freq Procedure

Percentage=100*as.vector(table(children))/sum(table(children))
CumulativeFrequency=cumsum(c(19,26,29,13,13))
CumulativePercentage=cumsum(Percentage)
Freq=data.frame(table(children),Percentage,CumulativeFrequency,CumulativePercentage)
row.names(Freq)=NULL
Freq

#### Chi-Square Test for Specified Proportions

p=c(16.86,30.02,26.72,15.86,10.55)
chisq.test(table(children),p=p/sum(p))