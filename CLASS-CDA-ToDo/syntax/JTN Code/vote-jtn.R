#############################
#### Voting data for log-linear models
#############################
#' [Video tutorial](https://onlinecourses.science.psu.edu/stat504/sites/onlinecourses.science.psu.edu.stat504/files/lesson05/Vote_viewlet_swf.html)

count <- c(70,324,56,195,332,101,382,199,117)
pview <- factor(c("lib","lib", "lib","mod","mod","mod","cons","cons" ,"cons"))
choice <- factor(rep(c("bush","clinton","perot"),3))

#### 2-way saturated log-linear model 

vote.sat<-glm(count~choice*pview, family=poisson())
summary(vote.sat)
fitted(vote.sat)


#### 2-way log-linear model of independence

vote.ind<-glm(count~choice+pview, family=poisson())
summary(vote.ind)
fits<-fitted(vote.ind)
resids <- residuals(vote.ind,type="pearson")
h <- lm.influence(vote.ind)$hat
adjresids <- resids/sqrt(1-h)
round(cbind(count,fits,adjresids),2)
