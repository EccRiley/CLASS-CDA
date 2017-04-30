library(foreign)
dat <- read.spss("data/reuters.sav", to.data.frame = TRUE)
R.isna <- function(x) sum(is.na(x))
#'
#' `r margin_note("\\textbf{\\texttt{R.isna()}}: A function for getting a count of \\texttt{NA} values in a dataframe (you could also do this using \\texttt{summary() {pkg:base}}, but I'm not interested in all the other information in the output from \\texttt{summary()} until I know the extent of \\texttt{NAs} in my data)")`
#'
sapply(dat, Risna) 
#'
#' Translation for the above code: "simply apply that function ('`R.isna()`') across the dataframe ('`dat`')". The output shows that "`party`" is the only column with `NAs`, in which there are $70$ cases (rows) with `NAs`.
#'
R.na <- function(x, v = 0){
    x <- ifelse(is.na(x), v, x)
    return(x)
    }
#' `r margin_note("\\textbf{\\texttt{R.na()}: A function for substituting \\texttt{NAs} with a \\texttt{non-NA} value. \\texttt{x = the object to be manipulated}, \\texttt{v = value to assign to NAs}. This works for discrete data (i.e., \\texttt{factors}) that are either in integer or string form, but will distort the data for continuous numeric data.")`
#'    
#'
#'Before passing the data to R.na(), find out which values are currently assigned to "`dat[, 'party']`".
#'
unique(dat$party)
#'
dat$party <- sapply(dat$party, R.na, v = 99)
#'
library(knitr)
R.msmm <- function(x) {
    if (is.null(ncol(x))) {
        xM <- mean(x, na.rm = TRUE)
        xSD <- sd(x, na.rm = TRUE)
        xMIN <- min(x, na.rm = TRUE)
        xMAX <- max(x, na.rm = TRUE)
        xNA <- sum(is.na(x))
        summ <- data.frame(xM, xSD, xMIN, xMAX, xNA)
        names(summ) <- c("M", "SD", "Min", "Max", "NAs")
        return(summ)
    } else {
        nums <- sapply(x, is.numeric)
        xn <- x[, nums]
        xM <- dplyr::summarise_each(xn, funs(mean(., na.rm = TRUE)))
        xSD <- dplyr::summarise_each(xn, funs(sd(., na.rm = TRUE)))
        xMIN <-
            dplyr::summarise_each(xn, funs(min(., na.rm = TRUE)))
        xMAX <-
            dplyr::summarise_each(xn, funs(max(., na.rm = TRUE)))
        summ <- rbind(xM, xSD, xMIN, xMAX)
        row.names(summ) <- c("M", "SD", "Min", "Max")
        summ <- as.data.frame(t(summ))
        return(summ)
    }
}
#'
#' `r margin_note("\\textbf{\\texttt{R.msmm()}}: A function for creating a quick, simple, mostly report-ready, summary table reporting `mean, sd, min, max` (hence '`msmm`') for the `numeric columns` in a given table, dataframe, or matrix. The function returns a datafrmae object with 4 columns.")`
#'
kable(R.msmm(dat$party), caption = "Summary information for '**party**'
        data column _after_ recoding \\texttt{NA}s {#tbl:party2}")
#'
dat$response <- droplevels(dat$response)
summary(dat)
#'
#' \newpage
#' # Analysis
#' 
#' Now that we are done with data cleaning and preparation, we can move on to the analysis at hand: **Breslowday Test**.
#'
dat <- read.spss("data/cnnpoll.sav", to.data.frame = TRUE)
dat <- na.omit(dat)
#'
#' Below is the matrix/array of joint frequencies for the "sex", "ind", and "response" variables supplied in J.Newsom's handout, "Three-Way Contingency Tables".
#'
ary <- array(c(100, 139,
			   106, 128,
			   157, 140,
			    89,  77), dim = c(2, 2, 2))
dimnames(ary) <- list(sex = c("male", "female"),
					  ind = c("party aff", "independent"), 
					  response = c("clinton", "trump"))
t(matrix(ary, ncol = 4, byrow = FALSE, dimnames = list(response = c("clinton", "trump"), sex_ind = c("male_party aff", "male_independent", "female_party aff", "female_independent"))))
#'                             C ,  T 
#' Here is how to acheive the same thing using `ftable()`.
#'
tbl <- ftable(dat, row.vars = c("sex", "ind"), col.vars = "response")
tbl.a <- array(tbl, dim = c(2, 2, 2))
dimnames(tbl.a) <- list(sex = c("male", "female"),
					  ind = c("party aff", "independent"), 
					  response = c("clinton", "trump"))
matrix(tbl.a, dimnames = list(response = c("clinton", "trump"), sex_ind = c("male_party aff", "male_independent", "female_party aff", "female_independent")))

#'
#'
#' 
library(descr)
ary.df <- as.data.frame(matrix(ary, ncol = 2, byrow = TRUE))
tbl.df <- as.data.frame(tbl)

xt <- xtabs(Freq ~ abuse + boyfriend + program, data = tbl.df)
xtbl.f <- ftable(xt)
xtbl.fp <- prop.table(ftable(xt))
ftable(xt) %>%
    prop.table() %>%
    apply(2, function(x) {paste0(round(x*100, 2), "%")}) -> ftabs

Desc(xt)
plot(xt, color = mypal.a75[c(17, 16)], main = "Abuse-x-Boyfriend-x-Program")
print(xt)
BreslowDayTest(xt)

tbl.df <- as.data.frame(tbl)
