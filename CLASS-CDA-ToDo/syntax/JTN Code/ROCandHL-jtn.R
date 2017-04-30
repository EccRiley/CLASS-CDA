####-------------Three useful functions download from----------------------------
#### roc.analysis() will do the ROC analysis, including a plot of the curve, for any fit object with binary y.
#### roc.plot() will plot the ROC curve given two vectors of scores, 
#### the first for the treatment group (y==1) and the second for the control group (y==0).
#### hosmerlem() will do the Hosmer-Lemeshow test, given the y and yhat vectors for the fittted data.
###--------------------------------------------------------------------------------

roc.analysis <-
function (object, newdata = NULL, newplot = TRUE, ...) 
{
    if (is.null(newdata)) {
        sd <- object$fitted[object$y == 1]
        sdc <- object$fitted[object$y == 0]
    }
    else {
        sd <- predict(object, newdata, type = "response")[newdata$y == 
            1]
        sdc <- predict(object, newdata, type = "response")[newdata$y == 
            0]
    }
    roc.plot(sd, sdc, newplot, ...)
}

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


####-----------------------------------------------------------------------------
#### This was changed a little bit from the origin hosmerlem function downloaded from
#### www.math.mcmaster.capeters4f03s4f03_0607index.html

hosmerlem <-
function (y, yhat=n*prob, g = 10) 
{
    cutyhat <- cut(yhat, breaks = quantile(yhat, probs = seq(0, 
        1, 1/g)), include.lowest = T)
    obs <- xtabs(cbind(1 - y, y) ~ cutyhat)
    expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
    chisq <- sum((obs - expect)^2/expect)
    if (chisq<1*(10^(-20))) P="." else P=ifelse((g==2),1,pchisq(chisq, g - 2,lower.tail=FALSE))
    yhat
    c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
}
