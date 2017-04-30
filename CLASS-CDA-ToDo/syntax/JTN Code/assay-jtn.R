options(contrasts=c("contr.treatment","contr.poly"))

#### Logistic regression 

r=c(10,17,12,7,23,22,29,29,23)
n=c(31,30,31,27,26,30,31,30,30)
logconc=c(2.68,2.76,2.82,2.90,3.02,3.04,3.13,3.20,3.21)
counts=cbind(r,n-r)
result=glm(counts~logconc,family=binomial("logit"))
summary(result,correlation=TRUE,symbolic.cor = TRUE)
result$coefficients

#### plot residuals vs. linear predictor 

plot(residuals(result, type="pearson"),result$linear.predictors)

#### plot logconc vs. empirical logits 

emplogit=log((r+0.5)/(n-r+0.5))
plot(logconc,emplogit)

#### adjusting for overdispersion 
#### This should give you the same model but with adjusted covariance
#### matirix, that is SE for your beta's and also changed z-values.
#### First estimate the dispersion parameter based on the MAXIMAL model; 
#### in our example this is simple since we have only one model
#### X^2/df=4.08
#### Notice that this does not adjust overall fit statistics 
sigma2=sum(residuals(result, type="pearson")^2/result$df.residual)

summary(result, dispersion=4.08,correlation=TRUE,symbolic.cor = TRUE)
summary(result, dispersion=sigma2,correlation=TRUE,symbolic.cor = TRUE)

result1=glm(counts~logconc, family=quasibinomial)
summary(result1)

#### Notice you can also use new package DISPMOD
#### gives a bit different result because it uses G^2/df
#### It adjusts the overall fit statistis too

install.packages("dispmod")
library(dispmod)
glm.binomial.disp(result)




#### For other diagonostic plots, see donner.R

#### Here is another way to get regression type plots

library()

#### This gives a series of plots such as;
#### residuals vs. fitted values
#### Q-Q plots
#### levarage, etc...

plot.lm(result)

#### The following is a function adapted from http//www.math.mcmaster.capeters4f03s4f03_0607index.html
#### roc.plot() will plot the ROC curve given two vectors of scores, 
#### the first for the treatment group (y==1) and the second for the control group (y==0).

roc.plot <-
function (sd, sdc, newplot = TRUE, ...) 
{
    sall <- sort(c(sd, sdc))
    sens <- 0
    specc <- 0
    for (i in length(sall):1) {
        sens <- c(sens, mean(sd >= sall[i], na.rm = T))
        specc <- c(specc, mean(sdc >= sall[i], na.rm = T))
    }
    if (newplot) {
        plot(specc, sens, xlim = c(0, 1), ylim = c(0, 1), type = "l", 
            xlab = "1-specificity", ylab = "sensitivity", ...)
        abline(0, 1)
    }
    else lines(specc, sens, ...)
    npoints <- length(sens)
    area <- sum(0.5 * (sens[-1] + sens[-npoints]) * (specc[-1] - 
        specc[-npoints]))
    lift <- (sens - specc)[-1]
    cutoff <- sall[lift == max(lift)][1]
    sensopt <- sens[-1][lift == max(lift)][1]
    specopt <- 1 - specc[-1][lift == max(lift)][1]
    list(area = area, cutoff = cutoff, sensopt = sensopt, specopt = specopt)
}

#### Let us draw the ROC plot

roc.plot(r,n-r)
