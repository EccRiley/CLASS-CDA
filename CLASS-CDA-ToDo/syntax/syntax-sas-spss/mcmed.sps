/* This macro generates a Monte Carlo confidence interval */.
/* for the indirect effect in statistical mediation analysis */.
/* See Appendix B in http://www.guilford.com/p/hayes3 */.
/* for instructions on its use */.
/* Written by Andrew F Hayes */.
/* www.afhayes.com */.

define mcmed (a=!charend ('/') !default(0)/b=!charend ('/') !default(0)
 /sea=!charend('/') !default(1)/seb=!charend('/') !default(1)
 /samples=!charend ('/') !default(10000)/covab=!charend ('/')
 !default(0)/conf=!charend ('/') !default(95)).
preserve.
set printback=off.
matrix.
compute r={(!sea)*(!sea),!covab;!covab,(!seb)*(!seb)}.
compute errchk=0.
do if (det(r) <= 0).
compute errchk=2.
end if.
do if (!seb <= 0 or !sea <=0).
compute errchk=1.
end if.
compute cilow=((100-!conf)/200).
compute cihigh=1-cilow.
compute cilow=trunc(!samples*cilow).
compute cihigh=trunc((!samples*cihigh)+.999)+1.
do if (cilow < 1 or cihigh > !samples).
compute errchk=3.
end if.
compute pars={!a;!sea;!b;!seb;!covab;!samples;!conf}.
print pars/title="*** Input Data ***"/rlabels="a:", "SE(a):", "b:","SE(b):", "COV(ab):", "Samples:", "Conf:"/format = F8.4.
do if (errchk=0).
compute mns={make(!samples,1,!a),make(!samples, 1, !b)}.
compute x1=sqrt(-2*ln(uniform(!samples,2)))&*cos((2*3.14159265358979)*
 uniform(!samples,2)).
compute x1=(x1*chol(r))+mns.
compute ab=x1(:,1)&*x1(:,2).
compute x1={x1,ab}.
compute abtmp=ab.
compute abtmp(GRADE(ab))=ab.
compute ab=abtmp.
save x1/outfile=*/variables=a b ab.
compute mc={(!a*!b), ab(cilow,1), ab(cihigh,1)}.
print mc/title="**** Monte Carlo Confidence Interval ****"/clabels="ab", "LLCI", "ULCI"/format = F8.4.
end if.
do if (errchk=1).
print/title="ERROR: Standard errors must be positive".
else if (errchk=2).
print/title="ERROR: Entered covariance is not compatible with the standard errors of a and b".
else if (errchk=3).
print/title="ERROR: Number of samples is too small for this level of confidence".
end if.
end matrix.
restore.
!enddefine.

