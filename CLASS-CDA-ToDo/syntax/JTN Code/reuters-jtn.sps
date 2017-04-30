* Encoding: windows-1252.
output close *.

*replace with your own file path.
GET FILE='c:\jason\spsswin\cdaclass\reuters.sav'.


*note you will get a crazy graphic instead of output by default, Under Edit>Options select the Output tab. Under Output Display and then Pivot tables.

* single-sample z-propotions test.
*temporary.
*select if response eq 0 or response eq 1.
*nptests   /onesample test (response) binomial (testvalue=.5 successcategorical=list(1) likelihood ).
*For binomial (z-proportion) test, successcategorical=list(1) chooses the value of 1 (Clinton) as the comparison  proportion
*testvalue=.5 gives the null proportion (default and can be omitted)
*likelihood gives CIs based on the sample SE estimate (Wald) rather than the null value SE estimate.
*The z-value printed uses a continuity correction (and will not match other programs unless the continuity correction is requested.

*temporary.
*select if response eq 0 or response eq 1.
*nptests    /onesample test (response) chisquare.

temporary.
select if response eq 0 or response eq 1.
crosstabs /tables=ind by response
  /cells=count row column total expected
  /statistics=chisq phi risk cc lambda btau ctau
