################################
#### Example: Vitamin C & French skiers
#### LOGLIN() & GLM() functions
#### Related: See VitaminC.R 
#################################

#### Here is one way to read the data vector of values with labels for the table

ski<-matrix(c(31, 17, 109, 122), ncol=2, dimnames=list(Treatment=c("Placebo", "VitaminC"), Cold=c("Cold", "NoCold")))
ski

#### Here is how we did this previously, for more details see VitaminC.R
#### Pearson's Chi-squared test with Yates' continuity correction

result<-chisq.test(ski)
result


######################
#### VIA LOGLIN() ####

#### Fitting 2-way log-linear model of independence
#### using the table structure and loglin() function; this is similar to CATMOD in SAS

#### first check if the data are in the table format

is.table(ski)

#### if not make them to be in table format with 

ski<-as.table(ski)


#### fit the model
#### list(1,2) indicates that we are fitting the two margins independently 
#### fit=TRUE and param=TRUE, say that we want have the fitted values of the cells 
#### and the estimated model parameters returned
#### for other options see R help, e.g., type the ?loglin in the prompt

ski.ind<-loglin(ski, list(1, 2), fit=TRUE, param=TRUE)
ski.ind

#### get the p-value; note that this output is the same as the chi-squared test 
#### WITHOUT Yates' continuity correction.

1-pchisq(ski.ind$lrt, ski.ind$df)


#### Fitting 2-way saturated log-linear model
#### using the table structure and loglin() function; 
####this is similar to CATMOD in SAS

ski.sat<-loglin(ski, list(c(1, 2)), fit=TRUE, param=TRUE)

#### notice the zero value for LRT, and the PERFECT fit!!!

ski.sat
1-pchisq(ski.sat$lrt, ski.sat$df)


###################
#### VIA GLM() ####
#### Fitting 2-way log-linear model of independence 
#### using the database structure and glm() function; this is similar to GENMOD in SAS

#### first we must format the data into a data frame format

ski.data<-as.data.frame(ski)
ski.data

#### using the glm() to fit the loglinear model is similar to fitting a regression model 
#### but the trick is in specifying what the correct response, and the family of distributions 
#### (i.e., sampling scheme!) 
#### we need to specify the response that we are modeling which are the cell counts; 
#### ski.data$Freq
#### this line: ski.data$Treatment+ski.data$Cold, specifies that we have two main effects, 
#### and no interaction term
#### we need to specify that sampling distribution, that is for log-linear models when modeling 
#### the counts we assume the most general random mechanism of Poisson sampling: family=poinsson(). 
#### This assures that errors follow Poisson model AND that we are modeling "log" of the response 
#### that is of our counts.

ski.ind<-glm(ski.data$Freq~ski.data$Treatment+ski.data$Cold, family=poisson())

#### to view the model and the relevant statistics

ski.ind

#### another way of viewing the model

summary(ski.ind)

#### this is ANOVA table similar to what you have seen in linear regression models that looks 
#### at the contribution of different model parameters to the overall fit of the model

anova(ski.ind)

#### looking at and saving the fitted values of the cell counts

fits<-fitted(ski.ind)

#### looking at and saving the pearson residuals

resids <- residuals(ski.ind,type="pearson")

#### checking for the influential points

h <- lm.influence(ski.ind)$hat

#### adjusted pearson residuals, e.g., standardized 

adjresids <- resids/sqrt(1-h)

#### putting them together into a table

round(cbind(ski.data$Freq,fits,adjresids),2)


#### Fitting 2-way saturated log-linear model
#### using the database structure and glm() function; this is similar to GENMOD in SAS
#### Notice now that we have an interaction term in the model: ski.data$Treatment*ski.data$Cold

ski.sat<-glm(ski.data$Freq~ski.data$Treatment*ski.data$Cold, family=poisson())
ski.sat
anova(ski.sat)


