
#'
#+ echo=FALSE, eval=FALSE
##############################
## LEGACY DIAGNOSTICS CODE ##
##############################
# influencePlot(fit, main = "Influence Plot", sub = "Circle size is proportial to Cook's Distance" )
# library(MASS)
# sresid <- studres(fit)
# hist(sresid, freq=FALSE,
#    main="Distribution of Studentized Residuals")
# xfit<-seq(min(sresid),max(sresid),length=40)
# yfit<-dnorm(xfit)
# lines(xfit, yfit)
##############################
## LogisticDx Package Code  ##
# library(LogisticDx)
# plot(lgm, palette = "Accent")
# lgm.dx <- dx(lgm, byCov = TRUE)
# x <- seq(1:30)
# lgm.dx <- cbind(x, lgm.dx)
# dx1 <- lgm.dx[order(lgm.dx$n), c("x", "n", "welfare", "sPr","P", "dChisq", "dDev", "dBhat")] %>% round(5)
# write.csv(dx1, "data/hw3-q1b.csv", row.names = FALSE)
#'
