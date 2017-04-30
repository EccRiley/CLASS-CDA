Rtbl = table(mydata$ind, mydata$response) tbl
#install.packages("vcd")
library(vcd)
#categorical figures
#uses vcd library called above
#transform variable to factor
mydata$response <- factor(mydata$response)
#table of counts created as tbl above is needed for mosaic #dimnames assigns labels for categories
dimnames(tbl) = list(ind = c("Party","Independent"),
response = c("Trump","Clinton"))
#mosaic(tbl) is sufficient, main gives the title, and gp=shading_hcl and gp_args changes the cutoffs for #different shades, because in our case, the full table would be gray given the higher default cutoffs for colors mosaic(tbl,main = "Reuters Poll Data", gp = shading_max)
