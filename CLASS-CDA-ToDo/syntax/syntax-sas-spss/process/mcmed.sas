* This macro generates a Monte Carlo confidence interval;
* for the indirect effect in statistical mediation analysis;
* See Appendix B in http://www.guilford.com/p/hayes3;
* for instructions on its use;
* Written by Andrew F. Hayes;
* www.afhayes.com;




%macro mcmed (a=0,sea=1,b=0,seb=1,samples=1000,conf=95,covab=0);
proc iml;
r=((&sea*&sea)||(&covab))//((&covab)||(&seb*&seb));
errchk=0;
if (det(r)<=0) then;do;errchk=2;end;
tmp=det(r);
if ((&sea<=0) | (&seb<=0)) then;do;errchk=1;end;
cilow=((100-&conf)/200);cihigh=1-cilow;
cilow=floor(&samples*cilow);cihigh=floor((&samples*cihigh)+0.999)+1;
if ((cilow < 1) | (cihigh > &samples)) then;do;errchk=3;end;
pars=&a//&sea//&b//&seb//&covab//&samples//&conf;
rwnm="a:"//"SE(a):"//"b:"//"SE(b):"//"COV(ab):"//"Samples:"//"Conf:";
print pars [label="*** Input Data ***" rowname=rwnm];
create mcvals from pars;append from pars;close mcvals;
if (errchk=0) then;do;
mns=j(&samples,1,&a)||j(&samples,1,&b);
x1=rannor(j(&samples,2,0));x1=(x1*root(r))+mns;
ab=x1[,1]#x1[,2];x1=x1||ab;
create mcvals from x1[colname={'a' 'b' 'ab'}];append from x1;
abtmp=ab;abtmp[rank(abtmp)]=ab;ab=abtmp;
mc=(&a*&b)||ab[cilow,1]||ab[cihigh,1];
clnm="ab"||"LLCI"||"ULCI";
print mc [label="*** Monte Carlo Confidence Interval ***" colname=clnm
 format=12.4];
end;
if (errchk=1) then;do;print "ERROR: Standard errors must be positive";end;
if (errchk=2) then;do;print "ERROR: Entered covariance is not compatible with the standard errors of a and b";end;
if (errchk=3) then;do;print "ERROR: Number of samples is too small for this level of confidence";end;
quit;
proc univariate data=mcvals noprint;var ab;histogram;run;
%mend;
