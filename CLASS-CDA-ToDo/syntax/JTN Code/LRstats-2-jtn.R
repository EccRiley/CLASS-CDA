#### Likelihood-Ratio Test Statistic for IxJ tables

LRstats=function(data)
{
	G2=2*sum(data*log(data/chisq.test(data)$expected))
 	G2pvalue=1-pchisq(G2,df=(dim(data)[1]-1)*(dim(data)[2]-1))
 	ans=c(G2,G2pvalue)
 	ans
 }