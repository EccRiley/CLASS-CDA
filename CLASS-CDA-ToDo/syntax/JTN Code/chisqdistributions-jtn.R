#### Function for plotting a chi-squared distribution with various DFs

#### Create a vector of values from 1 to 50

x=seq(1:50)

#### plot the first density chi-square function with df=1, 
#### using dchisq() function, 
#### type="l" denotes a line with default color black

plot(dchisq(x,1), type="l", xlab="Value of X^2", ylab="Probability Density", main="Examples of chi-squared distributions")


#### To the same plot, add more density lines for various DFs 

lines(dchisq(x,5), col="red")
lines(dchisq(x,10), col="blue")
lines(dchisq(x,20), col="green")
lines(dchisq(x,40), col="purple")

#### adding a legend

legend(40,0.2, c("df=1", "df=5", "df=10", "df=20", "df=40"), text.col=c("black", "red","blue", "green","purple"))
