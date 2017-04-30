#' ---
#' title: Reuters 2016 Polling Data Analysis
#' author: Riley Smith
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#'
#' # Riley Smith's Replication of Jason Newsom's Analyses
#'
#+ setup, echo=TRUE, message=FALSE, warning=FALSE, cache=FALSE
source("../SETUP.R", echo = TRUE)
#'
library(foreign); library(car) ## for the `read.spss()` and `recode()` functions, respectively ##
dat <- read.spss("data/reuters.sav", to.data.frame = TRUE)

#' Recode, using the {car} package's `recode()` function to undo default numeric values 1,2,3 and label values
#'
#dat <- within(dat, {recode(response, c("1=0", "2=1", "3=2"))})
#'
#' Binomial Test of Polling Data (count of successes = 677 for Clinton).
#'
#' `r margin_note("Note that the common method for binomial tests in `R` is to use `exact.test = TRUE`, which is actually the most conservative approach and also no ideal for binomial tests. Use `prop.test()` instead.")`
#'
#' Set the "other" response values to NA, then drop NA values.
#'
dat$response <- recode_factor(dat$response, "other/no opinion" = NA_character_) ## see "recode_factor()" in the {dplyr} package ##
dat <- na.omit(dat)
poll.t <- table(dat$response)
pt <- prop.test(677, 1231, p = 0.5, correct = FALSE)
pt
dat$resp2 <- dat$response
summary(dat$resp2)
lessR::BarChart(dat$response, xlab="Response: Trump=0, Clinton=1")
summary(dat$response)
#'
#' Bar Plot of Polling Data (using `R`'s Base Graphics)
#'
#'
par(family = "ETBembo"); palette(mypal)


#' Bar Plot of Polling Data (using the `{ggplot2}` package)
#'
#' Dot-Plot of Polling Data. This would actually be interesting to do with repeated-measures data with the candidates on the Y axis and time (in months/weeks) on the X axis.

poll.t <- data.frame(table(dat$response))
names(poll.t)<-c("Response", "Frequency")
bpoll <- ggplot(poll.t, aes(x=Response, y = Frequency)) + geom_segment(aes(yend=so), xend=-1, colour=mypal[20]) + geom_point(size = 5, aes(colour=freq)) + scale_colour_gradientn(colours = blues(max(so.t$freq)), guide = FALSE) + labs(y = "", x = "")  + thm_tft()
#' # To-Do
#'
#' Find method for entering expected values into binomial test function(s) (e.g., when you want to compare Oregon's polling estimates to those of the general US population).
#'

BarChart(response,xlab="Response: Trump=0, Clinton=1")
