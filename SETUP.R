#' ---
#' title: "SETUP.R - NWLGBTQ Research"
#' date: "format(Sys.Date(), '%d %B %Y')"
#' author: "Riley M. Smith"
#' ---
#'
#+ info, echo=FALSE

#----------------------------------------------------------------------#
# R Setup File
#     Created and maintained by Riley Smith
#     Saved with string encoding Unicode (UTF-8)
#----------------------------------------------------------------------#
#'
#' -----
#'
#' # Set Paths to Executables Needed by `R`
#'
#' In general, "`Sys.setenv()`" `{pkg:base}` is used to set [_"`execution paths`"_](http://www.flownet.com/ron/specials.pdf), or _"`PATH`" environment variables_ in `R`. In this case, the following code chunk tells `R` where to look in the computer's system (or _"`environment`"_) for the installed \latex distribution, which is needed whenever the output format is set to "`pdf_document`" (`{Rmarkdown}/{knitr}`).
#'
#+ echo=TRUE
# Sys.setenv(PATH=paste(Sys.getenv("PATH"),"/opt/X11/bin", "/Library/TeX/texbin",sep=":")) ## R.app (i.e., the R console) keeps forgetting my latex path... so I'm just adding this because it can't really hurt even if R.app suddenly remembers the path variable. ##
options(prompt = "> ", continue = "... ", width = 70) ## miscellaenous options set to my personal preference for how my R console looks. ##
#'
#'
#' # My Default Packages
#'
#'
auto.loads <- c("foreign", "plyr", "dplyr", "tidyr", "ggplot2", "rmarkdown", "bibtex", "knitr", "knitcitations", "ggthemes", "scales", "kableExtra", "devtools", "pander", "sysfonts", "papaja", "DT", "highlight", "bib2df", "tufte", "formatR", "extrafont", "showtext")

# install <- c("plyr", "dplyr", "tidyr", "ggplot2", "bibtex", "knitcitations", "ggthemes", "scales", "kableExtra", "devtools", "pander", "sysfonts"," "papaja", "DT", "highlight", "bib2df", "tufte", "extrafont", "showtext", "vcd", "descr", "car", "MASS")
#
# install.git <- c("crsh/papaja", "haozhu233/kableExtra")

sshhh <- function(a.package) { suppressWarnings(suppressPackageStartupMessages(
    library(a.package, character.only=TRUE)))} ## Suppress package warnings (SOURCE: https://www.r-bloggers.com/fun-with-rprofile-and-customizing-r-startup/) ##

invisible(sapply(auto.loads, sshhh))
#'
#' _Additional packages not loaded in this setup file, but which are also useful:_
#'
#+ noload
# library(psych)
# library(extrafont)
# library(broom) #tidy() function
# library(xtable)
# library(yaml)
# library(reports)
# library(markdown)
# library(RefManageR)
#'
#'
#' -----
#'
#' # Fonts
#'
#' ... because I'm stubborn and want to make everything use Edward Tufte(ET)'s "`ETBembo`" font family.
#'
library(extrafont)
loadfonts(quiet = TRUE)

quartzFonts(ETBembo = quartzFont(c("/Library/Fonts/ETBembo-RomanOSF.ttf",
                                   "/Library/Fonts/ETBembo-BoldLF.ttf",
                                   "/Library/Fonts/ETBembo-DisplayItalic.ttf",
                                   "/Library/Fonts/ETBembo-SemiBoldOSF.ttf")))
quartzFonts(serif = quartzFont(c("/Library/Fonts/ETBembo-RomanOSF.ttf",
                                 "/Library/Fonts/ETBembo-BoldLF.ttf",
                                 "/Library/Fonts/ETBembo-DisplayItalic.ttf",
                                 "/Library/Fonts/ETBembo-SemiBoldOSF.ttf")),
            sans = quartzFont(c("GillSansMT",
                                "GillSansMT-Bold",
                                "GillSansMT-Italic",
                                "GillSansMT-BoldItalic")),
            mono = quartzFont(c("Menlo-Regular",
                                "Menlo-Bold",
                                "Menlo-Italic",
                                "Menlo-BoldItalic")))
quartz.options(family = "ETBembo")
#'
#'
#' # Knitr options
#'
#' This first command is directly from the `{tufte}` package's ["**`handout.R`**"](https://github.com/rstudio/tufte/blob/master/R/handout.R). I'm including it in my setup document so that I can use the `{tufte}` package's "`marginfigure`" environtment outside of the the `{tufte}` package's output functions.
#'
#'
# knitr::knit_engines$set(marginfigure = function(options) {
#     options$type = 'marginfigure'
#     eng_block = knitr::knit_engines$get('block')
#     eng_block(options)
# })

#' The following command sets the `{knitr} chunk options`, which controls how `{knitr}`, and subsequently `Pandoc` handles output from chunks of `R-code`.
#'
#'
options(
    # unnamed.chunk.label = "chunk",
    bib.loc = "auxDocs/REFs.bib",
    citation_format = "pandoc",
    knitr.table.format = 'pandoc',
    xtable.floating = FALSE,
    scipen = 5,
    digits = 4,
    zero.print = ".",
    formatR.arrow = TRUE,
    DT.options = list(
        pageLength = 200,
        language = list(search = 'Filter:')
    ),
    bib.loc = "auxDocs/REFs.bib",
    citation_format = "pandoc",
    knitr.table.format = 'pandoc',
    formatR.arrow = TRUE,
    xtable.comment = FALSE,
    xtable.booktabs = TRUE,
    xtable.caption.placement = "top",
    xtable.caption.width = "\\linewidth",
    xtable.sanitize.text.function = function(x) {x},
    xtable.table.placement = "hb",
    xtable.floating = FALSE
)

# knitr::opts_knit$set(unnamed.chunk.label = "chunk")
knitr::opts_chunk$set(warning = FALSE,
                      message = FALSE,
                      comment = '>>',
                      fig.showtext = TRUE,
                      highlight = TRUE,
                      background = "#f2f2f2",
                      size = "footnotesize")
#'
#'
#' The above set `{knitr} chunk options` are my standard, global, options that I will always want to be set in every `.Rmd` file I create for producing any type of output. The first two, "`warning = FALSE`" \& "`message = FALSE`" are set as such because I don't want R's feedback in my output documents - once I'm at the point of producing an output, I'm aware of the warnings and know that they do not threaten my analysis (if they do, I fix the issue within the analysis document, but still do not need warnings and messages in my output document). The third option, "`comment = '>>'`" sets the textual symbol that precedes any `R-code` and corresponding output when either or both "`echo = TRUE`" \&/or "`results = 'markup'`" (as opposed to "`results = 'hide'`" or "results = `'asis'`").
#'
#'
#' Additional useful `{knitr} chunk options` for project- and output-specific `.Rmd` files include: _`echo = TRUE, tidy = FALSE, fig.asp = 1, collapse = TRUE) #dev='svg', fig.width = 6.5, fig.path = 'graphics/rplot_', fig.retina = 4`_.^[_**Note:** The `{knitr} chunk option 'fig.retina' = <X>` is valid for `.html` output formats, as it will cause `LaTeX` errors for `.pdf` outputs, and no figures will render if 'fig.retina' is set when outputting to '`word_document`'.]
#'
#'
#' -----
#'
#'
#' # Session Information and Options
#'
#'
devtools::session_info() ## "This is sessionInfo() re-written from scratch to both exclude data that's rarely useful (e.g., the full collate string or base packages loaded) and include stuff you'd like to know (e.g., where a package was installed from)." ##
#'
#'
#' ## `seed`
#'
seed <- 42; set.seed(seed)
#'
#' ## `testx`
#'
#" _Generating a collection of random normal deviates (i.e., data-points generated from the normal distribution) to use for testing plots_
#'
Rx <- rnorm(1000)
# plot(Rx, main = expression(paste("Multiline &\nPlot Title")), col = mypal, xlab = expression(paste("X-Axis Label With Math: ", hat(Y), " = ", sum(x[i], i==1, n))))
# plot(Rx, main = expression(paste(sum(codes))), col = mypal, xlab = expression(paste(N[Items])), ylab = expression(paste('%'[Codebook])))
#'
#'
#'
#' ## Pander Options
#'
panderOptions('keep.line.breaks', TRUE)
panderOptions('table.style', 'multiline')
panderOptions('table.alignment.default', 'left')
panderOptions('missing', ' ')
panderOptions('digits', 4)
#'
#' ## Citation Options
#'
cite_options(citation_format = "pandoc",
             style = "pandoc", # markdown, html
             hyperlink = "to.bib", # "to.doc" "to.bib"
             #super = FALSE,
             max.names = 6,
             longnamesfirst = TRUE,
             check.entries = TRUE)

#'
#' ## `today`
#'
today <- format(Sys.Date(), "%d %B %Y")
Origin <- "1899-12-30" ## EXCEL'S ORIGIN DATE - SEE `?as.Date` FOR DETAILS.
#'
#'
#' -----
#'
#' # Loading My Favorite Fonts ("ET Bembo", by Edward Tufte, and "Gill Sans MT", by Eric Gill)
#'
#' Note: The commands below are from the {sysfonts} package.
#'
#+ fonts
font.add("ETBembo", regular = "/Library/Fonts/ETBembo-RomanOSF.ttf", bold = "/Library/Fonts/ETBembo-SemiBoldOSF.ttf", italic = "/Library/Fonts/ETBembo-DisplayItalic.ttf") ## ET Bembo doesn't have "bolditalic", so I'm loading the semibold fontface as a substitute for the bolditalic option for the ET Bembo family
font.add("Gill Sans MT", regular = "/Library/Fonts/Microsoft/Gill Sans MT.ttf", bold = "/Library/Fonts/Microsoft/Gill Sans MT Bold.ttf", italic = "/Library/Fonts/Microsoft/Gill Sans MT Italic.ttf", bolditalic = "/Library/Fonts/Microsoft/Gill Sans MT Bold Italic.ttf")
library(showtext)
showtext.auto(enable = TRUE)

#' # My color pallettes
#'
#' ## `mypal`
#'
mypal <- c("#ffffff", "#d1d6dc", "#da00d1", "#a700a0", "#74006f", "#f5ff02", "#dee800", "#08ff8f", "#00f485", "#00b965", "#35f8fe", "#00d8d8", "#008b8b", "#053efa", "#0434d4", "#021860", "#7a8998", "#56636f", "#2e363e", "#181C20")
mypal3 <- mypal[3:16] ## NO WHITE, GRAY, NOR BLACK ##
mypal2 <- mypal[c(4, 9, 12, 15)]
mypal22 <- c("#29B78B", "#0434D4", "#00E6AE", "#01AFD7", "#A700A0")
mypal.a75 <- sapply(mypal, adjustcolor, alpha.f = 0.75)
mypal.a50 <- sapply(mypal, adjustcolor, alpha.f = 0.50)
mb <- colorRampPalette(mypal[c(5, 16)])
magblu <- mb(10)
#'
#' ## `pdxpal`
#'
pdxpal <- c("#8b9535", "#ffffff", "#373737") ## dark/hunter green, white, very dark gray
#'
#' ## BIP Study Palettes
#'
ynpal <- mypal[c(10, 1, 2)]
bmon_pal <- mypal[c(16, 1, 17)]
bmon_pal2 <- mypal[c(16, 18, 17)]
bstds_pal <- mypal[c(5, 1, 17)]
bstds_pal2 <- mypal[c(5, 18, 17)]
stmon_pal <- mypal[c(17, 16, 5)]
bmon_palX <- mypal[c(17, 16)]
bstds_palX <- mypal[c(17, 5)]
R.adjCol <- function(x, a){adjustcolor(x, alpha = a)}
stmon_pal2 <- as.character(lapply(stmon_pal, R.adjCol, a = 0.9))
stmon_pal.ls <- list("Standards" = mypal[17], ##Creating Primarily as a Reference
                     "Monitoring" = mypal[16],
                     "Standards \\&\nMonitoring" = mypal[5])

#'
#' `pal_HC & pal_HCd`
#'
pal_HC <- c("#7cb5ec", "#434348", "#90ed7d", "#f7a35c", "#8085e9", "#f15c80", "#e4d354", "#8085e8", "#8d4653", "#91e8e1") ## 'High Color' Color palette from `{ggthemes}` ##
pal_HCd <- c("#2b908f", "#90ee7e", "#f45b5b", "#7798BF", "#aaeeee", "#ff0066", "#eeaaee", "#55BF3B", "#DF5353", "#7798BF", "#aaeeee") ## 'High Color - Dark' color palette from `{ggthemes}` ##
#'
#'
#' -----
#'
#' ## My color pallette generating functions (using **`grDevices::colorRampPalette()`**)
#'
#'
mpal2 <- colorRampPalette(mypal22[3:5])
colsHC <- colorRampPalette(pal_HC, alpha = T)
colsHCd <- colorRampPalette(pal_HCd, alpha = T)
mag <- colorRampPalette(mypal[c(1, 5)], alpha = 0.75) ## "#ffdafd" = very pale magenta ##
mag2 <- colorRampPalette(mypal[c(18, 5)], alpha = 0.75) ## "#ffdafd" = very pale magenta ##
## "#575e6e" = muted dark grayish magenta (created using colorRampPalette(mypal[c(18, 5)](100)[1, 5] ##
## "#c8c0ce" = muted rosy gray  (created using colorRampPalette(mypal[c(2, 5)])
blues <- colorRampPalette(c(mypal[c(1, 15)]), alpha = T) ## "#d9e1fe" = light grayish blue ##
cyans <- colorRampPalette(c(mypal[1], mypal[2], mypal[11]), alpha = T)
grblues <- colorRampPalette(c(mypal[17], mypal[16]), alpha = T)
grays <- colorRampPalette(mypal[c(2, 18, 19)], alpha = T)
grays2 <- colorRampPalette(mypal[c(1, 18, 20)], alpha = T)
grblues2 <- colorRampPalette(c("#d9e1fe", mypal[16]), alpha=T)
blues2 <- colorRampPalette(c("#ffffff", mypal[14]), alpha = T) ## "#d9e1fe" = light grayish blue ##
whcybl <- colorRampPalette(mypal[c(1, 12, 16)], alpha = T)
whblmg <- colorRampPalette(mypal[c(1, 16, 4)], alpha = T)
blgrmg <- colorRampPalette(mypal[c(16, 2, 5)], alpha = T)
cols <- colorRampPalette(mypal, alpha = T)
cols1 <- colorRampPalette(mypal[3:19], alpha = T)
cols2 <- colorRampPalette(mypal2, alpha = T)
cols3 <- colorRampPalette(mypal[3:16], alpha = T)
cols4 <- colorRampPalette(mypal[c(3:5, 10, 12:16)], alpha = T)
pdxcols <- colorRampPalette(pdxpal[c(3, 1)], alpha = T)
#'
#' ## **`mpal()`**
#'
mpal <- function(x, p = colsHC, a = 1, ...) { ## Random color palette generation, using palette specified from '`colorRampPalette(n)`', with '`n`' determined by length of, or number of rows in, '`x`' ##
    if (class(x) == "data.frame") l <- nrow(x)
    else l <- length(x)
    s <- sample(p(l), l, replace = F)
    adjustcolor(s, alpha.f = a)
    return(s)
}
#'
#' ## **`grad()`**
#'
grad <- function(x, p = cols1, alpha = 1) { ## Gradient generation, using palette specified from '`colorRampPalette(n)`', with '`n`' determined by length of, or number of rows in, '`x`' ##
    if (class(x) == "data.frame") l <- nrow(x)
    else l <- length(x)
    adjustcolor(p(l), alpha.f = alpha)
}
#'
#'
#'
#' ## `mpar``
#'
mpar <- list(bg = "transparent", family = "Gill Sans", font.main = 1, font.axis = 1)
#'
#'
#' -----
#'
#' # My modified `ggplot` Themes (**`ggplot::ggthemes()`**)
#'
library(ggthemes)
#'
#' ## **`thm_ftf() & thm_tft2()`**
#'
thm_tft <- function(base_size = 12, base_family = "ETBembo", lpos = "right", ldir = "vertical", ticks = TRUE, ltitle = FALSE, xline = FALSE, yline = FALSE, xtext = TRUE, ytext = TRUE, xtitle = FALSE, ytitle = TRUE, ptitle = FALSE) {
    require(grid)
    thm <- theme_grey(base_size, base_family) %+replace%
        theme(plot.background = element_rect(colour = "transparent", fill = 'transparent'),
              plot.title = element_text(size = rel(1.25), face = "italic", colour = mypal[19]),
              plot.margin = unit(rep(0.15, 4), "cm"),
              panel.margin = unit(0.15, "cm"),
              panel.background = element_blank(),
              panel.border = element_blank(),
              strip.text = element_text(size = rel(0.7), colour = mypal[20]),
              legend.position = lpos,
              legend.direction = ldir,
              legend.key.width = unit(0.15, "cm"),
              legend.key.height = unit(0.25, "cm"),
              legend.text = element_text(size = rel(0.75), colour = mypal[20]),
              legend.title = element_text(face = "italic", colour = mypal[19]),
              axis.ticks = element_line(size = 0.25, colour = mypal[20]),
              axis.title.x = element_text(size = rel(1), face = "italic", margin = margin(t = 10), colour = mypal[20]),
              axis.title.y = element_text(size = rel(1), face = "italic", angle = 90, margin = margin(r = 10), colour = mypal[20], vjust = 1),
              axis.text.y = element_text(size = rel(1), hjust = 1, margin = margin(r = 2), colour = mypal[20]),
              axis.text.x = element_text(size = rel(1), colour = mypal[20]), #, margin = margin(t = 2)
              axis.line.x = element_line(size = 0.1, colour = mypal[20]),
              axis.line.y = element_line(size = 0.1, colour = mypal[20]),
              complete = TRUE)
    if (!ticks) {thm <- thm + theme(axis.ticks.x = element_blank())}
    if (!xtext) {thm <- thm + theme(axis.text.x = element_blank())}
    if (!ytext) {thm <- thm + theme(axis.text.y = element_blank())}
    if (!xtitle) {thm <- thm + theme(axis.title.x = element_blank())}
    if (!ytitle) {thm <- thm + theme(axis.title.y = element_blank())}
    if (!xline) {thm <- thm + theme(axis.line.x = element_blank())}
    if (!yline) {thm <- thm + theme(axis.line.y = element_blank())}
    if (!ltitle) {thm <- thm + theme(legend.title = element_blank())}
    if (!ptitle) {thm <- thm + theme(plot.title = element_blank())}
    thm
}

thm_tft2 <- function(base_size = 12, base_family = "ETBembo", lpos = "right", ldir = "vertical", ltitle = element_blank()) {
    require(grid)
    theme_tufte(base_size, base_family) %+replace%
        theme(plot.background = element_rect(colour = mypal[19], fill = 'transparent'),
              panel.background = element_rect(colour = mypal[19], fill = 'transparent'),
              legend.position = lpos,
              legend.direction = ldir,
              legend.title = ltitle,
              axis.text.x = element_text(size = rel(0.65), angle = 0),
              axis.text.y = element_text(size = rel(0.65), angle = 0),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              complete = TRUE)
}
#'
#' ## **`thm_cl(), thm_cl_tft(), thm_tr`** (useful for plots involving geographic maps)
#'
thm_cl_tft <- function(base_size = 11, base_family = "ETBembo", lpos = "right", ldir = "vertical", ltitle = FALSE, ptitle = FALSE) {
    require(grid)
    thm <- theme_tufte(base_size, base_family) %+replace%
        theme(plot.background = element_rect(colour = "transparent", fill = 'transparent'),
              panel.background = element_blank(),
              legend.position = lpos,
              legend.direction = ldir,
              legend.text = element_text(size = rel(0.65), colour = mypal[20]),
              legend.key.width = unit(0.15, "cm"),
              legend.key.height = unit(0.25, "cm"),
              legend.title = element_text(face = "italic", colour = mypal[19]),
              plot.title = element_text(face = "italic", colour = mypal[19], size = rel(1)),
              panel.grid = element_blank(),
              axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              complete = TRUE)
        if (!ltitle) {thm <- thm + theme(legend.title = element_blank())}
        if (!ptitle) {thm <- thm + theme(plot.title = element_blank())}
    thm
}

thm_cl <- function(base_size = 12, base_family = "ETBembo") {
    require( grid) # Needed for unit() function
    theme_grey(base_size) %+replace%
        theme(panel.background = element_rect(fill = "#f7f8f9"),
              panel.grid = element_blank(),
              axis.title = element_blank(),
              axis.text = element_blank(),
              axis.line = element_line(colour = "#708090",size = 0.15),
              axis.ticks = element_blank(),
              legend.title = element_text(face = "bold",size = rel(1)),
              legend.text = element_text(colour = "#3d3d3d",size = rel(.75)),
              legend.key.size = unit(0.35, "cm"),
              strip.text = element_text(colour = "#3d3d3d",size = rel(.85)),
              strip.background = element_rect(colour = "#708090", fill = "#ffffff"),
              panel.margin = unit(0.2, "lines"),
              plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "lines"),
              complete = TRUE)
}

thm_tr <- function(base_size = 12, base_family = "Helvetica") {
    theme(
        axis.ticks = element_line(colour = mypal[20], size = 0.2),
        axis.text = element_text(size = rel(0.8), colour = mypal[20], vjust = 1),
        axis.ticks = element_line(colour = mypal[20], size = 0.2),
        axis.title = element_text(colour = mypal[20], vjust = 1),
        axis.title.y = element_text(angle = 90, vjust = 0.5, colour = mypal[20]),
        axis.ticks.length = unit(0.3, "lines"),
        legend.background = element_rect(colour = mypal[20], fill = 'transparent'),
        legend.key = element_rect(colour = mypal[20], fill = 'transparent'),
        legend.key.size = unit(1.2, "lines"),
        legend.key.height = NULL,
        legend.key.width = NULL,
        legend.text = element_text(size = rel(0.8), colour = mypal[20]),
        legend.title = element_text(size = rel(0.8), face = "bold", hjust = 0, colour = mypal[20]),
        legend.position = "right",
        legend.text.align = NULL,
        legend.title.align = NULL,
        #   legend.direction = "vertical",
        legend.box = NULL,
        panel.background = element_rect(fill = "transparent", colour = mypal[20]),
        panel.border = element_rect(fill = NA, colour = mypal[20]),
        #   panel.grid.major = element_line(colour = "grey20", size = 0.2),
        #   panel.grid.minor = element_line(colour = "grey5", size = 0.5),
        #   panel.margin = unit(0.25, "lines"),
        strip.background = element_rect(fill = mypal[20], colour = mypal[20]),
        strip.text = element_text(size = rel(0.8), colour = 'white'),
        strip.text.y = element_text(angle = -90, colour = 'white'),
        plot.background = element_rect(colour = NA, fill = 'transparent'),
        plot.title = element_text(size = base_size * 1.2),
        plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"),
        complete = TRUE)   # denotes that this is a complete theme function
}
#'
#'
#' ## **`thm_A(), thm_B(), & thm_C()`**
#'
#'
thm_A <- function(base_size = 12, base_family = "Helvetica") {
    require( grid) # Needed for unit() function
    theme_grey(base_size) %+replace%
        theme(plot.background = element_rect(colour = mypal[20], fill = 'transparent'),
              panel.background = element_rect(colour = mypal[20], fill = 'transparent'),
              panel.grid.minor = element_blank(),
              panel.grid.major.y = element_line(colour = mypal[20], size = 0.1, linetype = "dashed"),
              panel.grid.major.x = element_blank(),
              axis.title = element_text(colour = mypal[20], size = rel(.8), margin = margin(0,0,5,0), face = "bold"),
              axis.text.x = element_blank(),
              axis.text.y = element_text(colour = mypal[20], size = rel(.7)),
              axis.line = element_line(colour = mypal[20], size = 0.15),
              axis.ticks.length = unit(0.1, "cm"),
              axis.ticks = element_line(size = 0.1, colour = mypal[20]),
              legend.title = element_text(face = "bold", size = rel(0.8)),
              legend.text = element_text(colour = mypal[20], size = rel(0.6)),
              legend.key.size = unit(0.35, "cm"),
              legend.background = element_rect(colour = mypal[20], size = 0.25, linetype = "dotted", fill = 'transparent'),
              legend.position = "right",
              strip.background = element_rect(fill = mypal[20], colour = mypal[20]),
              strip.text.x = element_text(colour = mypal[1], size = rel(0.85), margin = margin(1,0,1,0), face = 'bold'),
              strip.text.y = element_text(angle = -90, colour = mypal[1], size = rel(0.85), margin = margin(1,0,1,0), face = 'bold'),
              panel.margin = unit(0.2, "lines"),
              plot.margin = unit(c(0.75, 0.5, 0, 0.5), "lines"), #top, right, bottom, left
              plot.title = element_text(size = rel(1), colour = mypal[20], margin = margin(0,0,10,0), face = "bold"), #,
              complete = TRUE)
}

thm_B <- function(base_size = 12) {
    require(grid)#Neededforunit()function
    thm_A(base_size) %+replace%
        theme(axis.text.x = element_text(colour = mypal[20], size = rel(.75)))#,)
}

thm_C <- function(base_size = 12) {
    require(grid)
    thm_A(base_size) %+replace%
        theme(axis.text.x = element_text(colour = mypal[20], size = rel(.65)),
              legend.title = element_blank(),
              legend.text = element_text(colour = mypal[20], size = rel(0.65)),
              legend.key.size = unit(0.25, "cm"),
              legend.position = "bottom", legend.direction = "vertical")
}
#'
#' -----
#'
#'
#' # Useful non-packaged functions by others (source links provided with descriptions)
#'
#'
#' ## [**`rpm()`**][^rpm] (Replace Print Methods)
#'
#' The below function essentially wraps `pander::pander()` around all default print methods (thanks to [this source][^rpm] for the original `'replace.print.methods()'` function defintion code)
#'
#' Here's the breakdown of the steps taken in the `'rpm()'` function below:
#'
#' 1. Get a character list (`'as.character()'`) of available generic, S3, and S4 usage methods (`'methods()'`) for the function `'pander()'`.
#' 2. Substitute (`'gsub()'`) the pattern "pander" (`'PKG'`) with "`print`" in the character list created above (`'PKG_methods'`).
#' 3. For each item (`'i'`) in the character list created and manipulated in the last two steps (`'seq_along(PKG_methods)'`), assign the new function `'f()'` (`'eval(parse(text = paste(PKG,":::", PKG_methods[i], sep = "")))'`) to use for print to each print_method in `'PKG_methods'`.
#'
#' [^rpm]: http://www.r-statistics.com/2013/03/write-ms-word-document-using-r-with-as-little-overhead-as-possible/
#'
#'
#' Change default value for "`big.mark`" in `format()` `{pkg:base}`:
#'
Rformat <- function(x, big.mark = ",", ...) {format(x, big.mark = big.mark)}
#'
#'
library(pander)
rpm <- function(PKG = "pander") { ## Replace print methods with pander printing methods ##
    PKG_methods <- as.character(methods(PKG))
    print_methods <- gsub(PKG, "print", PKG_methods)
    for (i in seq_along(PKG_methods)) {
        f <- eval(parse(text = paste(PKG,":::", PKG_methods[i], sep = "")))
        assign(print_methods[i], f, ".GlobalEnv")}
}
rpm()
#'
#'
#' -----
#'
#' ## [**`.simpleCap()`**] \& [**`capwords()`**].
#'
#' This was provided in the examples for _`tolower()/toupper()`_ (see `?tolower()`) and is useful for capitalizing the first letter of each word in a character string (`s`).
#'
capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
                             {s <- substring(s, 2); if (strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}
#'
#' -----
#'
#' ## [**`mp()`**][^mp] (Multiplot for `{ggplot2}`)
#'
#' Function for creating multiple 'ggplot()'s on one plot page (uses the `{grid}` package with `{ggplot2}`)
#'
#' [^mp]: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#'
#' ## **`R.frmt()`
#'
#' Another function for my laziness. This simply makes "," the default argument for `big.mark = <"">` in `base::format(x, ...)`
#'
R.frmt <- function(x, big = ",", dgts = 0, ...) {format(x, big.mark = big, digits = dgts, ...)}
#'
mp <- function(..., plotlist=NULL, file, cols=1, layout=NULL) { ## multiplot function for `ggplot2::ggplot()` ##
    library(grid)
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    numPlots = length(plots)
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))}
    if (numPlots == 1) {
        print(plots[[1]])}
	else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))}}
}
#'
#'
#' ## [**`ggsubt`**][^ggsubt] (Subtitles for `{ggplot2}`, by Bob Rudis)
#'
#'
#' [^ggsubt]: https://rud.is/b/2016/03/16/supreme-annotations/
#'
#'
library(grid)
library(gtable)
ggsubt <- function(gg, label = "", fontfamily = NULL, fontsize = 10, hjust = 0, vjust = 0, bottom_margin = 5.5, newpage = is.null(vp), vp = NULL, ...) { ## Function to add subtitles to 'ggplot()' ##
    if (is.null(fontfamily)) {
        gpr <- gpar(fontsize = fontsize, ...)}
	else {
        gpr <- gpar(fontfamily = fontfamily, fontsize = fontsize, ...)}
    subtitle <- textGrob(label, x = unit(hjust, "npc"), y = unit(hjust, "npc"), hjust = hjust, vjust = vjust, gp = gpr)
    data <- ggplot_build(gg)
    gt <- ggplot_gtable(data)
    gt <- gtable_add_rows(gt, grobHeight(subtitle), 2)
    gt <- gtable_add_grob(gt, subtitle, 3, 4, 3, 4, 8, "off", "subtitle")
    gt <- gtable_add_rows(gt, grid::unit(bottom_margin, "pt"), 3)
    if (newpage)
        grid.newpage()
    if (is.null(vp)) {
        grid.draw(gt)}
	else {
        if (is.character(vp))
            seekViewport(vp)
        else
            pushViewport(vp)
        grid.draw(gt)
        upViewport()}
    invisible(data)
}
#'
#'
#'
#' -----
#'
#'
#' # My Functions
#'
#'
#' ## **`sedit()`** \& **`rsetup()`** (functions for my laziness)
#'
#'
sedit <- function(x) { ## open and source a file ##
    file.edit(x)
    source(x)
    }
#'
rsetup <- function() { ## open this setup.r file ##
    file.edit("SETUP.R")
    }
#'
#' ## **`dt()`**
#'
dt <- function(x, caption = deparse(substitute(x)), rownames = FALSE, class = c("row-border"), filter = 'none',
               options = list(pageLength = 300, lengthMenu = c(200, 500, 1000, 1500), info = TRUE, dom = 'tip', ...), ...) { ## `DT::datatable()` with my usual settings ##
    cap <- htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                                   htmltools::strong(htmltools::em(paste(caption))))
    DT::datatable(x, rownames = rownames, caption = cap, filter = filter, class = class, options = options, ...)}
## ^ thanks to SO user [Sven Hohenstein](http://stackoverflow.com/questions/14577412/how-to-convert-variable-object-name-into-string) for providing the function deparse(substitute(x)) idea to convert an Robject's name to string. Notes: `deparse()` = "Turn unevaluated expressions into character strings"; `substitute()` = "return the parse tree for the unevaluated expression, substituting any variables bound in env." ##
#'
#' ## **`show_colR()`**
#'
show_colR <- function(colours, labels = TRUE, borders = NULL, ...) { ## modified version of `scales::show_col()` ##
    n <- length(colours)
    ncol <- ceiling(sqrt(n))
    nrow <- ceiling(n/ncol)
    colours <- c(colours, rep(NA, nrow * ncol - length(colours)))
    colours <- matrix(colours, ncol = ncol, byrow = TRUE)
    old <- par(pty = "s", mar = c(0, 0, 0, 0))
    on.exit(par(old))
    size <- max(dim(colours))
    if (length(colours >= 40)) t.size <- 0.75
    else t.size <- 0.85
    plot(c(0, size), c(0, -size), type = "n", xlab = "", ylab = "",
         axes = FALSE, cex = t.size, ...)
    rect(col(colours) - 1, -row(colours) + 1, col(colours), -row(colours),
         col = colours, border = borders, ...)
    if (labels) {
        text(col(colours) - 0.5, -row(colours) + 0.75, matrix(seq(1:length(colours)), nrow = nrow, byrow = TRUE), cex = t.size, ...)
        text(col(colours) - 0.5, -row(colours) + 0.25, colours, cex = t.size, ...)}
}
# show_colR(mpal(seq(1:100)))
#'
#' ## **`R.pathDia()`**
#'
R.pathDia <- function(manifest, latent, direct = c("east", "west"), xlim = c(-0.5, 1.25), ylim = c(-0.5, 1.25), bg = NA, arrcol = mypal[20], ptitle = NULL, lwd = 1.5, lty = 2, ...) { ## manifest and latent = lists of lengths 1:n containing objects of class "manifest" or "latent", respectively, direct is used for the arros ##
    ## save number of latent and manifest vars defined in user's latent var list ##
    nlat <- length(latent)
    nmani <- length(manifest)
    ## create a wall ##
    wall(xlim = xlim, ylim = ylim, bg = bg, xpd = TRUE)
    ## draw latentvar(s) using user-specs in list of manifest() args ##
    for (l in 1:nlat) {draw(latent[[l]])
        for (i in 1:nmani) {draw(manifest[[i]])
            ## arrows from manifest to latent vars ##
            pathdiagram::arrow(from = manifest[[i]], to = latent[[l]], start = direct[1], end = direct[2], col = arrcol, lwd = lwd, lty = lty)}}
    ## add title if user specified a title ##
    if (!is.null(ptitle)) {title(paste0(ptitle))}
}
#'
#' ## **`R.mpathDia()`**
#'
R.mpathDia <- function(ivars, mvars, dvars, direct = c("east", "west"), xlim = c(-0.5, 1.25), ylim = c(-0.5, 1.25), bg = NA, arrcol = mypal[20], ptitle = NULL, lwd = 1, ...) { ## ivars and dvars = lists of lengths 1:n containing objects of class "manifest" or "latent", respectively... dir is used for the arrows, the remaining args set various graphical parameter options ##
    ## save number of independent, mediator, and dependent vars, respectively ##
    ndep <- length(dvars)
    nind <- length(ivars)
    nmed <- length(mvars)
    ## create a 'wall' (i.e., the plotting region in the `{pathdiagram}` framework ##
    wall(xlim = xlim, ylim = ylim, bg = bg, xpd = TRUE)
    ## draw independent (predictor) var(s) using user-specs in list of manifest() args ##
    for (d in 1:nind) {
        draw(ivars[[d]])}
    ## draw mediator var(s) using user-specs in list of manifest() args ##
    for (i in 1:nmed) {
        draw(mvars[[i]])}
    ## draw dependent var(s) using specs in list of latent() args ##
    for (d in 1:ndep) {
        draw(dvars[[d]])
        ## arrows from mediator to dep. vars ##
        for (m in 1:nmed) {
            pathdiagram::arrow(from = mvars[[m]], to = dvars[[d]], start = direct[1], end = direct[2], col = arrcol, lwd = lwd, lty = 1)
            ## (dashed) arrows from predictor to dep. vars ##
            for (i in 1:nind)
                pathdiagram::arrow(from = ivars[[i]], to = dvars[[d]], start = direct[1], end = direct[2], col = arrcol, lwd = lwd, lty = 2)
            pathdiagram::arrow(from = ivars[[i]], to = mvars[[m]], start = direct[1], end = direct[2], col = arrcol, lwd = lwd, lty = 1)}}
    ## add title if user specified a title ##
    if (!is.null(ptitle)) {title(paste0(ptitle))}
}
#'
#' ## **`R.search()`**
#'
R.search <- function() {
    s <- search()
    s <- gsub("package:", "", s)
    return(s)
}
#'
#' ## **`R.cite_r()`**
#'
R.cite_r <- function(file = NULL, prefix = "R-", tex = FALSE, footnote = TRUE, ...) { ## modified version of the `papaja::cite_r()` ##
    if (!is.null(file))
        r_version <- as.character(packageVersion("base"))
    cite_just_r <- paste0("**R** [**v** ", r_version, ", @", prefix, "base]")
    if (is.null(file) || !file_test("-f", file)) {
        if (!is.null(file) || pkgs != "all")
            warning("File ", file, " not found. Cannot cite R-packages. If knitting again does not solve the problem, please check file path.")
        return(cite_just_r)}
    R.search <- function() {
        s <- search()
        s <- gsub("package:", "", s)
        return(s)
    }
    pkgs <- R.search()
    r_bib <- readLines(file)
    cite_keys <- r_bib[grepl(paste0("\\@\\w+\\{", prefix), r_bib)]
    cite_keys <- gsub("\\@\\w+\\{", "", cite_keys)
    cite_keys <- gsub("\\,", "", cite_keys)
    bib <- sapply(cite_keys, function(x) {
        y <- list(x)
        names(y) <- gsub(prefix, "", x)
        y}, USE.NAMES = FALSE)
    if (length(pkgs) > 1 && pkgs != "all") {
        pkg_citations <- bib[names(bib) %in% pkgs]}
    else {pkg_citations <- bib[!names(bib) == "base"]}
    if (length(pkg_citations) == 0) {
        return(cite_just_r)}
    vers1 <- function(x) as.data.frame(strsplit(as.character(packageVersion(x)), split = "\\."))[1,]
    vers2 <- function(x) as.data.frame(strsplit(as.character(packageVersion(x)), split = "\\."))[2,]
    pkg_texts <- paste0("_", names(pkg_citations), "-v",
                        sapply(names(pkg_citations), vers1), ".",
                        sapply(names(pkg_citations), vers2), "._ ",
                        "[@", unlist(pkg_citations), "]")
    if (length(pkg_texts) > 1) {
        pkg_info <- paste(pkg_texts[1:(length(pkg_texts) - 1)],
                          collapse = ", ")
        pkg_info <- paste0(pkg_info, ", and ", tail(pkg_texts,
                                                    1))}
    else {pkg_info <- pkg_texts}
    if (footnote) {
        res <- paste0("^[**Note:** This document was created using _**R**-v", r_version, "_ [@", bib$base, "], and the following _**R**-packages_:", " ", pkg_info, "]")}
    # "Note: This document was created using _**R**_ [v.", r_version, ", @", bib$base, "][^papaja_pkg_citations]")
    # res$pkgs <- paste0("\n\n[^papaja_pkg_citations]: The following R-packages were also used: ", pkg_info, ".\n\n")}
    else { if (tex) {
            res <- paste0("\n", newthought("This document was created using "), "_**R**-v", r_version, "_ [@", bib$base, "], and the following _**R**-packages_:", "\n\n\\medskip\n\n", pkg_info)}
            else {res <- paste0("\nThis document was created using _**R**-v", r_version, "_ [@", bib$base, "], and the following _**R**-packages_:", "\n\n", pkg_info)}
        }
    return(res)
}
#'
#' -----
#'
#' ## **`R.newthought()`**
#'
#' This is just an alternative implementation of _`tufte::newthought()`_. The original function `{pkg:tufte}`, manipulates the formatting of _both the font and paragraph environment_ for the `text` passed to it. So, when text is passed to `tufte::newthought()`, the `font` is set to "`small-caps`" ("`\textsc`" in \latex), and the verticle line spacing before the `text's` paragraph is set using multiple variables defined in [`tufte-css.css`](https://github.com/edwardtufte/tufte-css), [`tufte-handout.tex`](https://github.com/rstudio/tufte/blob/master/inst/rmarkdown/templates/tufte_handout/resources/tufte-handout.tex), and [`tufte-common.def`](https://github.com/edwardtufte/tufte-latex/blob/master/tufte-common.def) (the template files provided by [the `{tufte}` `R-package``](https://github.com/rstudio/tufte) and [the `{tufte-latex}` package for \latex](https://github.com/edwardtufte/tufte-latex)). This added paragraph spacing causes issues in certain paragraph environments, such lists and tables, thus the alternative implementation below allows the user to use `small-caps` font formatting across with one command that translates across formats (i.e., the same command is used for `HTML` and `PDF` (\latex)  output formats) by only specifying "`small-caps`"-formatting for the `text`, with no additional manipulation(s) to the paragraph environment of the `text` passed to the function.
#'
#' For reference, here the source-code for the original function is below:
#'
#'
#+ origNewthought
# newthought <- function (text) {
#     if (is_html_output()) {
#         sprintf("<span class=\"newthought\">%s</span>", text)
#     }
#     else if (is_latex_output()) {
#         sprintf("\\newthought{%s}", text)
#     }
#     else {
#         sprintf("<span style=\"font-variant:small-caps;\">%s</span>",
#                 text)
#     }
# }
#'
#' ... And now my alternative implementation (including import (`:::`) of two helper functions from knitr ("`is_html_output`" \& "`is_latex_output`", see [{`pkg:tufte`} source-code on github](https://github.com/rstudio/tufte/blob/master/R/utils.R)):
#'
#+
## Helpers ##
is_html_output <- function(...) knitr:::is_html_output(...)
is_latex_output <- function(...) knitr:::is_latex_output(...)

## New Newthought! ##

Rnewthought <- function(text) {
    if (is_latex_output()) {
        sprintf("\\textsc{%s}", text)
    }
    else {
        sprintf("<span style=\"font-variant:small-caps;\">%s</span>",
                text)
    }
}
#'
#'
#' -----
#'
#' ## **`R.fontSize()`**
#'
#' This is a function for implementing fontsize declarations across output formats (primarily `HTML` and `LaTeX/PDF`). Note that since LaTeX is the pickiest of them all, the "`size`" argument is a character string that can be one of "tiny", "scriptsize", "footnotesize", "small", "normalsize", "large", "Large", "LARGE", "huge", or "Huge".
#'
#+
## Helpers ##
is_html_output <- function(...) knitr:::is_html_output(...)
is_latex_output <- function(...) knitr:::is_latex_output(...)

## Fontsizes ##

R.fontSize <- function(text, size) {
    if (is_latex_output()) {
        sprintf(paste("\\%s{%s}"), size, text)
    }
    else {
        if (size == "tiny") {size <- "xx-small"}
        if (size == "scriptsize") {size <- "xx-small"}
        if (size == "footnotesize") {size <- "x-small"}
        if (size == "small") {size <- "small"}
        if (size == "normalsize") {size <- "medium"}
        if (size == "large") {size <- "large"}
        if (size == "Large") {size <- "x-large"}
        if (size == "LARGE") {size <- "xx-large"}
        if (size == "huge") {size <- "xx-large"}
        if (size == "Huge") {size <- "xx-large"}
        sprintf("<span style=\"font-variant:small-caps;\">%s</span>",
                text)
    }
}
#'
#'
#' -----
#'
#' ## **`R.rrefs()`**
#'
R.rrefs <- function(bib_main = "REFs.bib", bib_r = "rrefs.bib", perl = TRUE) { ## wrapper function to create list of packages using the 'R.cite_r()` function above (which is a modified version of `papaja::cite_r()`) ##
    r_refs(file = bib_main, append = TRUE)
    r_refs(file = bib_r, append = FALSE)
    rrefs <- R.cite_r(file = bib_r)
    rrefs <- gsub(" and the R-packages", ";", rrefs, perl = perl)
    rrefs <- gsub("\\], and", "\\];", rrefs, perl = perl)
    rrefs <- gsub("\\],", "\\];", rrefs, perl = perl)
    rrefs <- strsplit(rrefs, "; ", perl = perl)
    rrefs <- data.frame(rrefs)
    names(rrefs) <- "R-References"
    return(kable(rrefs, col.names = ""))
}
#'
#' ## **`R.perc()`**
#'
R.perc <- function(x, n=14) {
    if (length(x) == 0)
        return(character())
    y <- round(x/n, digits=getOption("digits"))
    paste0(comma(y * 100), "%")
}

R.percTEX <- function(x, n=14) {
    if (length(x) == 0)
        return(character())
    y <- round(x/n, digits=getOption("digits"))
    paste0(comma(y * 100), "\\%")
}
#'
#' ## **`R.pr()`**
#'
R.pr <- function(x) {
    round(x/sum(x), digits = 3)
}
#'
#' ## **`R.dich()`**
#'
R.dich<-function(x, min = 0, values = c(0, 1)) { ## Function for dichotomizing numeric data ##
    if(is.numeric(x)) {
        x[x > min] <- values[2]
        x[x <= min] <- values[1]}
    else
        stop('x must be numeric')
    return(x)
}
#'
#' ## **`R.negs()`**
#'
R.negs <- function(x) {ifelse(x < 0, 2, x)}
#'
#' ## **`R.names()`**
#'
R.names <- function(x, dim = 2, str, pl = "A", sep = "_") { ##Function for appending (pl="A") or prepending (pl="P") character string to matrix dimnames (defaults to colnames by "dim=2" for rownames, set "dim=1")##
    if(dim == 2)
        if (pl == "A")
            colnames(x) <- paste(str, colnames(x), sep=sep)
        else if (pl == "P")
            colnames(x) <- paste(colnames(x), str, sep=sep)
        else if(dim == 1)
            if (pl == "A")
                colnames(x) <- paste(str, colnames(x), sep=sep)
            else if (pl == "P")
                rownames(x) <- paste(rownames(x), str, sep=sep)
            else
                stop('pl must be either "A" or "P"')
            else
                stop('dim must be either "1" or "2"')
            return(x)
}
#'
#' ## **`R.r2var()`**
#'
R.r2var <- function(x) {
    x$CID <- rownames(x)
    return(x)
}
#'
#' ## **`R.idvar()`**
#'
R.idvar <- function(x) {
    x$id <- seq(length(x[,1]))
    return(x)
}
#'
#' ## **`R.revcol()`**
#'
R.revcol <- function(x) {
    y <- names(x)
    x <- x[, rev(y)]
    return(x)
}
#'
#' ## **`R.cperc()`**
#'
R.cperc<-function(x, cb=103) {
    n<-sum(x) # where `x` is a vector
    n<-paste0(comma((n/cb)*100), "%")
    return(n)
}
#'
#' ## Venn Diagram Functions
#'
#' ## **`R.Euvenn()`**
#'
#' This is a skeletal version of the euler venn diagram function I use specifically for my stuff (see **`R.Evenn()`** and **`R.EvennA()`**)
#'
R.Euvenn <- function(x, vcol, ...) {
    evenn <- venneuler(x)
    evenn$labels <- NA
    plot(evenn, col = vcol, alpha = .75, ...)
    text(evenn$centers, labels = colnames(x), col = "#c0c0c0", cex = 2)
    return(evenn)
}
#'
#'
#' ## **`R.Evenn()`**
#'
#' This is a slightly more complex version of **`R.Euvenn()`**, but is still a transferable/skeletal version of the euler venn diagram function I use specifically for my stuff (see **`R.EvennA()`**)
#'
R.Evenn <- function(x, vcol,
                    labs = rev(names(x)), al = 0.5, lcex = 1, adj = NULL, ...){
    A <- x[[2]]
    AB <- x[[1]]
    evenn <- venneuler(c(A = A, "A&B" = AB))
    evenn$labels <- c("", "")
    plot(evenn, col = vcol, alpha = al, ...)
    text(evenn$centers, labels = labs, col = mypal[1], cex = lcex,
         adj = adj)
    return(evenn)
}
#'
#'
#' ## **`R.EvennA()`**
#'
#' This one is the most complex of the venn diagram functions I've written thus far, and its defaults are highly specific to my own needs and conventions.
#'
R.EvennA <- function(x, ffamily = "ETBembo", vcol = mypal1[c(21,22)],
                     labs = rev(names(x)), al = 0.5, lcex = 1,
                     ltextx = evenn$centers[, 1], ltexty = evenn$centers[, 2],
                     ptextx = 0.4, ptexty = 0.45, pmain = NULL, pmcex = 0.95, pmfont = 3, keep = FALSE, ...){
    palette(mypal)
    par(family = ffamily, mar = (c(2, 0.75, 0.75, 0.75) + 0.1))
    A <- x[[2]]
    AB <- x[[1]]
    evenn <- venneuler(c(A = A, "A&B" = AB))
    evenn$labels <- c("", "")
    ABperc <- round((AB/A)*100, 0)
    ABperc <- paste0(ABperc, "%")
    plot(evenn, col = vcol, alpha = al, main = pmain, cex.main = pmcex, font.main = pmfont)
    box(lwd = 0.5, col = 20)
    text(x = ltextx, y = ltexty, labels = labs, col = mypal[1], cex = lcex)
    text(ptextx, ptexty, labels = ABperc, col = mypal[1], cex = lcex, font = 3)
    if (keep == TRUE) {
    return(evenn)
    }
}
#' -----
#'
#' ## **`R.isna()`**
#'
R.isna <- function(x) sum(is.na(x))## Getting a count of NA values in the original dataframe ##
#'
#' -----
#'
#' ## **`R.msmm()`**
#'
R.msmm <- function(x, d = 2) {
    if (is.null(ncol(x))) {
        xM <- mean(x, na.rm = TRUE)
        xSD <- sd(x, na.rm = TRUE)
        xMIN <- min(x, na.rm = TRUE)
        xMAX <- max(x, na.rm = TRUE)
        xNA <- sum(is.na(x))
        summ <- data.frame(xM, xSD, xMIN, xMAX, xNA)
        names(summ) <- c("M", "SD", "Min", "Max", "NAs")
        summ <- apply(summ, 2, round, digits = d)
        return(summ)
    } else {
        nums <- sapply(x, is.numeric)
        xn <- x[, nums]
        xM <- dplyr::summarise_each(xn, funs(mean(., na.rm = TRUE)))
        xSD <- dplyr::summarise_each(xn, funs(sd(., na.rm = TRUE)))
        xMIN <- dplyr::summarise_each(xn, funs(min(., na.rm = TRUE)))
        xMAX <- dplyr::summarise_each(xn, funs(max(., na.rm = TRUE)))
        xNA <- sapply(x, R.isna)
        summ <- rbind(xM, xSD, xMIN, xMAX, xNA)
        row.names(summ) <- c("M", "SD", "Min", "Max", "NAs")
        summ <- as.data.frame(t(summ))
        summ <- apply(summ, 2, round, digits = d)
        return(summ)
    }
}
#'
# R.msmm1 <- function(x) {
#     xM <- mean(x, na.rm = TRUE)
#     xSD <- sd(x, na.rm = TRUE)
#     xMIN <- min(x, na.rm = TRUE)
#     xMAX <- max(x, na.rm = TRUE)
#     xNA <- sum(is.na(x))
#     summ <- data.frame(xM, xSD, xMIN, xMAX, xNA)
#     names(summ) <- c("M", "SD", "Min", "Max", "NAs")
#     return(summ)
# }
#'
#' -----
#'
#' ## **`R.strIndex()`**
#'
R.strIndex <- function(x, str) { ## Assign NA to specified string value within dataframe - primarily for use with 'apply()', 'sapply()', 'vapply()', and/or 'lapply()' `{base}` ##
    is.na(x) <- which(x == str)
    return(x)
}
#'
#' _Test of **`R.strIndex()`**_
#'
# x <- data.frame(id = seq(1:52), dat = rep(paste0(LETTERS, LETTERS), 2))
# R.strIndex(x$dat, "AA")
# rm(x)
#'
#' ## **`R.kappaPB()`**
#'
R.kappaPB <- function(x, n, stat = "pb") { ## Function for calculating prevalence and bias in a contingency table (for kappa/interrater reliability analysis). x: matrix object, or other object that can be coerced to a 2x2 matrix object, containing the contingency table's non-marginal cell values in clockwise order (see details), or a 2x2 matrix-like (e.g., dataframe, table, etc) object representing the contingency table's non-marginal cells. n: total number of ratings in the contingency table. stat: character value of length 1 specifying the supplementary statistic(s) to calculate ("p" = Prevalence, "b" = Bias, "pb" [DEFAULT] = Prevalence and Bias, see details). Citation: @sim_kappa_2005 ##
	if (length(x) != 4) {
		stop('length(x) must equal 4')}
	if (class(x) != "matrix") {
		if (class(x) == "data.frame") {
			x <- matrix(x, 2)}
		else if (class(x) == "table") {
			x <- matrix(x, 2)}
		else if (class(x) == "list") {
			x <- matrix(x, 2)}
		else { stop('x must be either a list of length 4 OR a 2x2 matrix, table, or dataframe')}
		}
	a <- x[1, 1]
	b <- x[1, 2]
	c <- x[2, 1]
	d <- x[2, 2]
	if (stat == "p") {
		Prevalence <- (abs(a - d))/n
		return(Prevalence)}
	if (stat == "b") {
		Bias <- (abs(b - c))/n
		return(Bias)}
	if (stat == "pb") {
		p <- (abs(a - d))/n
		b <- (abs(b - c))/n
		pb <- list(Prevalence = p, Bias = b)
		return(pb)}
}
#'
#' ## **`R.na()`**
#'
R.na <- function(x, v = 0) { ## SURELY THIS ALREADY EXISTS SOMEWHERE, RIGHT??? ###
	x <- ifelse(is.na(x), v, x)
	return(x)
}
#' _Test of **`R.na()`**:_
#'
# xx <- data.frame(id = seq(1:10), y = c(1, 2, NA, 4, 5, NA, 7, 8, NA, 10))
# xx <- apply(xx, 2, R.na)
# rm(xx)
#'
#' -----
#'
#' ## **`R.newthought()`**
#'
#' This is just an alternative implementation of _`tufte::newthought()`_. The original function `{pkg:tufte}`, manipulates the formatting of _both the font and paragraph environment_ for the `text` passed to it. So, when text is passed to `tufte::newthought()`, the `font` is set to "`small-caps`" ("`\textsc`" in \latex), and the verticle line spacing before the `text's` paragraph is set using multiple variables defined in [`tufte-css.css`](https://github.com/edwardtufte/tufte-css), [`tufte-handout.tex`](https://github.com/rstudio/tufte/blob/master/inst/rmarkdown/templates/tufte_handout/resources/tufte-handout.tex), and [`tufte-common.def`](https://github.com/edwardtufte/tufte-latex/blob/master/tufte-common.def) (the template files provided by [the `{tufte}` `R-package``](https://github.com/rstudio/tufte) and [the `{tufte-latex}` package for \latex](https://github.com/edwardtufte/tufte-latex)). This added paragraph spacing causes issues in certain paragraph environments, such lists and tables, thus the alternative implementation below allows the user to use `small-caps` font formatting across with one command that translates across formats (i.e., the same command is used for `HTML` and `PDF` (\latex)  output formats) by only specifying "`small-caps`"-formatting for the `text`, with no additional manipulation(s) to the paragraph environment of the `text` passed to the function.
#'
#' For reference, here the source-code for the original function is below:
#'
#'
#+ origNewthought
# newthought <- function (text) {
#     if (is_html_output()) {
#         sprintf("<span class=\"newthought\">%s</span>", text)
#     }
#     else if (is_latex_output()) {
#         sprintf("\\newthought{%s}", text)
#     }
#     else {
#         sprintf("<span style=\"font-variant:small-caps;\">%s</span>",
#                 text)
#     }
# }
#'
#' ... And now my alternative implementation (including import (`:::`) of two helper functions from knitr ("`is_html_output`" \& "`is_latex_output`", see [{`pkg:tufte`} source-code on github](https://github.com/rstudio/tufte/blob/master/R/utils.R)):
#'
#+

## Helpers ##
is_html_output <- function(...) knitr:::is_html_output(...)
is_latex_output <- function(...) knitr:::is_latex_output(...)

## New Newthought! ##

R.newthought <- function(text) {
    if (is_latex_output()) {
        sprintf("\\textsc{%s}", text)
    }
    else {
        sprintf("<span style=\"font-variant:small-caps;\">%s</span>",
                text)
    }
}
#'
#' -----
#'
#' ## **`R.regTEX()`**
#'
#'
R.regTEX <- function(txt, op = 1) { ## '`op`' = The output format for which
    ## the text should be re-decorated
    ## before being sent to `pandoc`.
    ## Input for `op` can be one of:
    ## `1 [DEFAULT] (LaTeX)` or
    ## `2 (HTML)` ##
    if (op == 1) {
        TX <- gsub("\\*\\*(.*?)\\*\\*", "\\\\textbf\\{\\1\\}", txt, perl = TRUE)
        TX <- gsub("_(.*?)_", "\\\\textit\\{\\1\\}", TX, perl = TRUE)
        TX <- gsub("\`(.*?)\`", "\\\\texttt\\{\\1\\}", TX, perl = TRUE)
    }
    else {
        TX <- gsub("\\\\textbf\\{(.*?)\\}", "\\*\\*\\1\\*\\*", txt, perl = TRUE)
        TX <- gsub("\\\\textit\\{(.*?)\\}", "_\\1_", TX, perl = TRUE)
        TX <- gsub("\\\\texttt\\{(.*?)\\}", "\`\\1\`", TX, perl = TRUE)
    }
    return(TX)
}
#'
#' ## **`R.rspss()`**
#'
#' `r margin_note("Another laziness function.")`
#'
#' A wrapper for `foreign::read.spss()`, with the defaults for "`use.value.labels`" and "`to.data.frame`" respectively changed to "`FALSE`" and "`TRUE`".
#'
#+ Rrspss
R.rspss <- function(x, vlabs = FALSE, df = TRUE){
    read.spss(x, use.value.labels = vlabs, to.data.frame = df)
}
#'
#' -----
#'
#' ## Knit Hook: **`Rplot()`**
#'
knitr::knit_hooks$set(Rplot = function(before, options, envir) { ## SETTING UP A CHUNK HOOK FOR SETTING PLOT PARAMS CORRESPONDING TO '`ggthemes::theme_tufte()`' WHEN USING BASE GRAPHICS ##
    if (before) {
        palette(mypal)
        par(bg = "transparent", font.main = 3, family = "ETBembo")
    }})
knitr::knit_hooks$set(Rrule = function(before, options, envir) {
    if (before) {
        "\\Rrule\n\n"
    } else {
        "\n\n\\Rerule"
    }
})
knitr::opts_hooks$set(echoRule = function(options) {
    if (options$echo == FALSE) {
        options$Rrule = NULL
    } else {
        options$Rrule = TRUE
    }
    options
})

knitr::opts_chunk$set(echoRule = TRUE)


#'
#' -----
#'
#' #' ## **`R.glmdf()`**
#'
#' A function for restructuring output from "summary.glm()" as a dataframe
#'
R.glmdf <- function(x){ ## "x" must be an object of class "summary.glm"...
    ## ... currently only works for simple logistic
    ## regression (i.e., only one predictor) ##
    se.cmpn <- x[c("dispersion", "cov.unscaled")]
    ## ^- "cmpn" = "components" abbreviated ##
    ## see "`r names(summary(<SOME GLM OBJECT>))`" ##
    se.a <- se.cmpn[[1]]
    se.b <- se.cmpn[[2]]
    se.ab <- se.a*se.b ## "covmat" ##
    se.c <- diag(se.ab) ## "var.cf" ##
    se.x <- sqrt(se.c) ## "s.err" ##
    z.a <- c(x$coefficients[[1]], x$coefficients[[2]])
    z.x <- z.a / se.x
    p.x <- 2 * pnorm(-abs(z.x))
    x.df <- data.frame(Estimate = c(x$coefficients[[1]],
                                    x$coefficients[[2]]),
                       SE = se.x,
                       Z = z.x,
                       p.value = p.x)
    return(x.df)
}

#'
#' ## Rmarkdown Rendering Shortcuts
#'
#' _Note that the following functions will render properly based on specifications set for each output format in the **'`_output.yaml`'** file in the same working directory as the file being rendered._
#'
#'
#+ rendering

RtftB <- function(x, ...) {rmarkdown::render(x, output_format = "tufte::tufte_book", ...); print(format(Sys.time(), '%n[%d %b. %I:%M%p]'))}
RtftH <- function(x, ...) {rmarkdown::render(x, output_format = "tufte::tufte_handout", ...); print(format(Sys.time(), '%n[%d %b. %I:%M%p]'))}
RtftHt <- function(x, ...) {rmarkdown::render(x, output_format = "tufte::tufte_html", ...); print(format(Sys.time(), '%n[%d %b. %I:%M%p]'))}
RtftHt2 <- function(x, ...) {rmarkdown::render(x, output_format = "bookdown::tufte_html2", ...); print(format(Sys.time(), '%n[%d %b. %I:%M%p]'))}
Rbeam <- function(x, ...) {rmarkdown::render(x, output_format = "beamer_presentation", ...); print(format(Sys.time(), '%n[%d %b. %I:%M%p]'))}
Rword <- function(x, ...) {rmarkdown::render(x, output_format = "word_document", ...); print(format(Sys.time(), '%n[%d %b. %I:%M%p]'))}
Rpdf <- function(x, ...) {rmarkdown::render(x, output_format = "pdf_document", ...); print(format(Sys.time(), '%n[%d %b. %I:%M%p]'))}
Rnb <- function(x, ...) {rmarkdown::render(x, output_format = "html_notebook", ...); print(format(Sys.time(), '%n[%d %b. %I:%M%p]'))}
Rhtml <- function(x, ...) {rmarkdown::render(x, output_format = "html_document", ...); print(format(Sys.time(), '%n[%d %b. %I:%M%p]'))}
Rhtml2 <- function(x, ...) {rmarkdown::render(x, output_format = "bookdown::html_document2", ...); print(format(Sys.time(), '%n[%d %b. %I:%M%p]'))}
RhMat <- function(x, ...) {rmarkdown::render(x, output_format = "rmdformats::material", ...); print(format(Sys.time(), '%n[%d %b. %I:%M%p]'))}
Repub <- function(x, ...) { ## Rendering epubs requires a second step with the .epub output to generate the kindle .mobi format, which can be achieved in this shortcut by uncommenting the kindlegen args in this function ##
	rmarkdown::render(x, output_format = "bookdown::epub_book", ...)
#  p <- gsub("\\.Rmd", "\\.epub", x)
#  bookdown::kindlegen(p)
#  print(format(Sys.time(), '%n[%d %b. %I:%M%p]'))
	}
#'
#'
#' -----
#'
#' # References
#'
