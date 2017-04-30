#+ setup, echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE
source("../SETUP.R")
knitr::opts_chunk$set(tidy = FALSE, echo = TRUE, cache = FALSE, fig.keep = 'high', fig.show = 'hold', results = 'asis', autodep = TRUE, Rplot = TRUE, dev = 'pdf', fig.path = 'graphics/HW3/rplot-q1-', fig.width = 7, fig.height = 7, out.width = "\\linewidth", echo = TRUE)
#'
#' # Huber's Data (Atkinson, 1985)
xh <- c(-4:0, 10)
yh <- c(2.48, .73, -.04, -1.44, -1.32, 0)
summary(lmH <- lm(yh ~ xh))
(im <- influence.measures(lmH))
plot(xh,yh, main = "Huber's data: L.S. line and influential obs.")
abline(lmH); points(xh[im$is.inf], yh[im$is.inf], pch = 20, col = 2)
#'
#' # Irwin's Data (Williams, 1987)
#'
xi <- 1:5
yi <- c(0,2,14,19,30)    # number of mice responding to dose xi
mi <- rep(40, 5)         # number of mice exposed
summary(lmI <- glm(cbind(yi, mi -yi) ~ xi, family = binomial))
signif(cooks.distance(lmI), 3)   # ~= Ci in Table 3, p.184
(imI <- influence.measures(lmI))
stopifnot(all.equal(imI$infmat[,"cook.d"],
                    cooks.distance(lmI)))
#'
#' \newpage
#'
d <- dat1b.n
fit <- lgm
d$predicted <- predict(fit)   # Save the predicted values
d$residuals <- residuals(fit) # Save the residual values
d$fitted <- fitted.values(fit)
d %>% select(abuse, predicted, residuals, fitted) %>% head()
ggplot(d, aes(x = program, y = abuse)) + theme_bw() +
    # geom_segment(aes(xend = program, yend = predicted), alpha = .1) +  ## Lines to connect points ##
    geom_point(aes(color = residuals)) +
    scale_color_gradient2(low = pal_my[15], mid = pal_my[2], high = pal_my[3]) +
    guides(color = FALSE) +
    geom_point(aes(y = predicted), shape = 1)

d %>% gather(key = "iv", value = "x", -abuse, -predicted, -residuals, -fitted) %>%  # Get data into shape
    ggplot(aes(x = x, y = abuse)) +  # Note use of `x` here and next line
    geom_segment(aes(xend = x, yend = predicted), alpha = .2, colour = pal_my[17]) +
    geom_point(aes(colour = residuals)) +
    scale_colour_gradient2(low = pal_my[15], mid = pal_my[2], high = pal_my[3]) +
    guides(color = FALSE) +
    geom_point(aes(y = predicted), shape = 1) +
    facet_grid(~ iv, scales = "free") +  # Split panels here by `iv`
    theme_bw()

ggplot(d, aes(x = x, y = abuse)) +
    geom_segment(aes(xend = x, yend = predicted), alpha = .2) +
    geom_point() +
    geom_point(data = d %>% filter(x != round(predicted)),
               color = "red",
               size = 2) +
    scale_colour_gradient2(low = pal_my[15], mid = pal_my[2], high = pal_my[3]) +
    guides(colour = FALSE) +
    geom_point(aes(y = predicted), shape = 1) +
    theme_bw()
