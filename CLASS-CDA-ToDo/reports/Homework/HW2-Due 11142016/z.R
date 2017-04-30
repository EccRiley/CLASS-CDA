dat <- read.spss("data/child.sav", to.data.frame = TRUE)
dat <- na.omit(dat) %>%
    droplevels()

# ```{r tbl, fig.margin=TRUE, results='asis'}
tbl <- with(dat, {
    table(abuse, boyfriend, program)
})
dimnames(tbl) <- list(Abuse = c("no", "yes"),
                      Boyfriend = c("no", "yes"),
                      Program = c("no", "yes")
                      )
tbl1 <- with(dat, {
    table(abuse, boyfriend)
})
dimnames(tbl1) <- list(Abuse = c("no", "yes"),
                      Boyfriend = c("no", "yes")
)
print(tbl)
print(tbl1)

mosaicplot(~ Abuse + Boyfriend + Program, data = tbl, color = TRUE, main = "Abuse x Boyfriend x Program", type = 'FT')
mosaicplot(Abuse ~ Boyfriend + Program, data = tbl, color = TRUE, main = "Abuse ~ Boyfriend + Program", type = 'deviance')

library(MASS) ## "loglm()" (... and many other useful functions) ##
llm <- loglm(~ Abuse + Boyfriend + Program, data = tbl)
llm
llm1 <- loglm(Abuse ~ Boyfriend, data = tbl1)
llm1
llm2 <- loglm(~ Abuse + Boyfriend + Program, data = tbl)
llm2

anova(llm, llm1, llm2)
par(las = 2)
plot(llm)
plot(llm1)
plot(llm2)

# $2.a.$ Loglinear Model^[\complete]

# $$ \log(\mu_{ijk}) = \lambda + \lambda_{i}^{X} + \lambda_{j}^{Y} + \lambda_{k}^{z} + \lambda_{ij}^{XY} + \lambda_{ik}^{XZ} + \lambda_{jk}^{YZ} + \lambda_{ijk}^{XYZ} $$

    # -----

    # `r tufte::newthought("Loglinear Analysis Summary.")` A loglinear model was used to test whether the likelihood of abuse occuring in households with a boyfriend varied according to program participation.


    # the association between pedestrian race and whether individuals were stopped by Portland Police (_PPD_) for major versus minor offenses. `r pWH` of pedestrians stopped by PPD were White, and `r pAA` were African American ($N_{Total} = `r pT`$). Among the `r AA` African American pedestrians, `r pAAMA` (`r AAMA`) were stopped for major offenses, while `r pAAMI` (`r AAMI`) were stopped for minor offenses. Among the `r WH` White pedestrians, `r pWHMA` (`r WHMA`) were stopped for major offenses, while `r pWHMI` (`r WHMI`) were stopped for minor offenses. Results from the likelihood ratio test of the $2 x 2$ contingency table (see above) indicated no significant differences regarding these factors, $G^{2}(1) = 0.14, p = 0.70$.

# dat2.n$abuse.f <- factor(dat2.n$abuse, levels = c(0, 1))
# with(dat2.n, {
#     plot(welfare, abuse.f)
# })
# curve(predict(lgm, type = "link"), add = TRUE)



BID <- seq(from = 0, to = 700, by = 10)

logOdds.F.young <- (-3.92) + .014*BID
logOdds.M.young <- (-3.92) + .014*BID + .25*1
logOdds.F.old   <- (-3.92) + .014*BID + .15*1
logOdds.M.old   <- (-3.92) + .014*BID + .25*1 + .15*1

pY.F.young <- exp(logOdds.F.young) / (1 + exp(logOdds.F.young))
pY.M.young <- exp(logOdds.M.young) / (1 + exp(logOdds.M.young))
pY.F.old   <- exp(logOdds.F.old) / (1 + exp(logOdds.F.old))
pY.M.old   <- exp(logOdds.M.old) / (1 + exp(logOdds.M.old))

# x11() ## {pkg:grDevices} ##
par(mfrow = c(2, 2))
plot(x = BID, y = pY.F.young, type = "l", col = "blue", lwd = 2,
     ylab = "Pr(Y = 1)", main = "predicted probabilities for young women")
plot(x = BID, y = pY.M.young, type = "l", col = "blue", lwd = 2,
     ylab = "Pr(Y = 1)", main = "predicted probabilities for young men")
plot(x = BID, y = pY.F.old, type = "l", col = "blue", lwd = 2,
     ylab = "Pr(Y = 1)", main = "predicted probabilities for old women")
plot(x = BID, y = pY.M.old, type = "l", col = "blue", lwd = 2,
     ylab = "Pr(Y = 1)", main = "predicted probabilities for old men")

# x11()
par(mfrow = c(1, 1))
plot(x=BID, y=pY.F.young, type="l", col="red", lwd=1,
     ylab="Pr(Y=1)", main="predicted probabilities")
lines(x=BID, y=pY.M.young, col="blue", lwd=1)
lines(x=BID, y=pY.F.old,   col="red",  lwd=2, lty="dotted")
lines(x=BID, y=pY.M.old,   col="blue", lwd=2, lty="dotted")
legend("bottomright", legend=c("young women", "young men",
                               "old women", "old men"), lty=c("solid", "solid", "dotted",
                                                              "dotted"), lwd=c(1,1,2,2), col=c("red", "blue", "red", "blue"))
#'
#' -----
#'
# dat.p1 <- with(dat2.n,
#                data.frame(welfare = mean(welfare), boyfriend = mean(boyfriend), white = mean(white), program = mean(program), abuse = factor(0:1)))
# dat.p1
#
# dat.p1$abuseP <- predict(lgm, newdata = dat.p1, type = "response")
# dat.p1
# dat.p2 <- cbind(dat.p1, predict(lgm, newdata = dat.p1, type = "link", se = TRUE))
#
# dat.p3 <- within(dat.p2, {
#     PredictedProb <- plogis(fit)
#     LL <- plogis(fit - (1.96 * se.fit))
#     UL <- plogis(fit + (1.96 * se.fit))
# })
#
# ## view first few rows of final dataset ##
# head(dat.p3)
#
# ## Plot it! ##
# ggplot(dat.p3, aes(x = abuse, y = PredictedProb)) + theme_tufte() +
#     geom_ribbon(aes(ymin = LL, ymax = UL, fill = abuse), alpha = .2) +
#     geom_line(aes(colour = abuse), size = 1.5) +
#     scale_colour_manual(values = cols2(2)) +
#     scale_fill_manual(values = cols2(2)) +
#     labs(x = "Welfare", y = "Predicted Probability", colour = "Abuse", fill = "Abuse")
