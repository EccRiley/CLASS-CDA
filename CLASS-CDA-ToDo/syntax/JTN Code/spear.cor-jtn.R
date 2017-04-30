#A function for computing Spearman correlation for IxJ tables & MH statistic
#Spearman correlation for IxJ tables 
#table = IxJ table or a matrix 
#rscore=vector of midrank row scores 
#cscore=vector of midrank column scores 
spear.cor=function(table)
{ 
	table=as.table(table)
	rscore=array(data=NA, dim=dim(table)[1])
	cscore=array(data=NA, dim=dim(table)[2])
	
		for( i in 1:dim(table)[1]){
		if (i==1) rscore[i]=(margin.table(table,1)[i]+1)/2
		else 
		rscore[i]=sum(sum(margin.table(table,1)[1:(i-1)])+(margin.table(table,1)[i]+1)/2)
		}
		rscore
	    ri=rscore-sum(table)/2
	    ri=as.vector(ri)
	
		for (j in 1:dim(table)[2]){
			if (j==1) cscore[j]=(margin.table(table,2)[j]+1)/2
			else
			cscore[j]=sum(sum(margin.table(table,2)[1:(j-1)])+(margin.table(table,2)[j]+1)/2)
		}
		cscore
		ci=cscore-sum(table)/2
		ci=as.vector(ci)
				
	v=sum(t(table*ri)*ci)
	rowd=sum(table)^3-sum(margin.table(table,1)^3)
	cold=sum(table)^3-sum(margin.table(table,2)^3)
	w=(1/12)*sqrt(rowd*cold)
	
	scor=v/w
	scor
	
	M2=(sum(table)-1)*scor^2
	M2
	result=c(scor, M2)
	result
	}
	