#'
knitr::opts_chunk$set(tidy=TRUE)
#' # `mediate()`
#'
mediation::mediate
#'
#'\newpage
#' # `polr()`

MASS::polr
logis.cdf <- function(x) expression(over(1, 1+e^(-(over(x-mu, s)))))
