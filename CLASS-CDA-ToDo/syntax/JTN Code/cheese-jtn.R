#### the data file cheese.dat is needed for this R program
#### see the course web site

cheese <- read.table( "cheese.dat", col.names=c("Cheese", "Response", "N") )

#### see if Response is a factor

is.factor( cheese$Response )

#### make it into an ordered factor

cheese$Response <- factor( cheese$Response, ordered=T )

#### load the MASS package

library(MASS)

#### fit the proportional-odds logistic regression model

result <- polr( Response ~ Cheese, weights=N, data=cheese )
summary(result)