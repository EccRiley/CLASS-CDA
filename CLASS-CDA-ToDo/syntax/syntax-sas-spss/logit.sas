dm "output;clear;log;clear";
*ods results off; ods listing;  *comment out to get figure;
options ps=60;

*Import Procedure*;
proc import datafile="/folders/myfolders/CLASS-CDA/data/cnnpoll.sav" out=one dbms = sav replace;
run;

proc logistic data=one order=data descending;
model response=sex ind sex*ind;
run;
