###Laura Thomson 2008, Splus manual for CAD, Agresti(2002)
###A function for computing Goodman and Kruskal's gamma adapted 
##########################################################

Gamma.f<-function(x)
{
# x is a matrix of counts. You can use output of crosstabs or xtabs.
n<-nrow(x)
m<-ncol(x)
res<-numeric((n-1)*(m-1))
for(i in 1:(n-1)) {
for(j in 1:(m-1)) res[j+(m-1)*(i-1)]<-x[i,j]*sum(x[(i+1):n,(j+1):m])
}
C<-sum(res)
res<-numeric((n-1)*(m-1))
iter<-0
for(i in 1:(n-1))
for(j in 2:m) {
iter<-iter+1; res[iter]<-x[i,j]*sum(x[(i+1):n,1:(j-1)])
}
D<-sum(res)
gamma<-(C-D)/(C+D)
list( gamma=gamma, C=C, D=D)
}