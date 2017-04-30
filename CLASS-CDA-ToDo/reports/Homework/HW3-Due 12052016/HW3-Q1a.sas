** NOTE: Comments surrounded by two or more astericks (i.e., "** <COMMENT TEXT> **") are notes, while comments beginning with a single asterick (i.e., "*<COMMENT TEXT>") are commented out syntax fragments. ** ;

** ----------------------------------------------------------------------- **
** Bootstrapped Mediation Model using A.Hayes' PROCESS Macro **
** ----------------------------------------------------------------------- **

dm "output;clear;log;clear";
ods results off; ods listing;  *comment out to get figure;
options ps=60;

title 'Logistic Regression - Simple Mediation';
*Data Import Procedure*;
proc import datafile="/folders/myfolders/CLASS-CDA/data/child2.sav" out=one dbms = sav replace;
run;

*Process Macro*;
include"/folders/myfolders/CLASS-CDA/process.sas";
process(data=one, vars=abuse white neglect,y=abuse,x=white,m=neglect,model=4); run;


** ----------------------------------------------------------------------- **
** Without Bootstrapping **
** ----------------------------------------------------------------------- **

** Miscellaneous Setup Options **;
dm "output;clear;log;clear";

** Uncomment below to suppress figures **;
ods results off; *ods listing;

options ps=60;

title 'Homework-3, Q1: Logistic Mediation Analysis
(dichotomous predictor, continuous mediator, and dichotomous outcome(s))';

** Data Import Procedure **;
proc import datafile="/folders/myfolders/CLASS-CDA/data/child2.sav" out=one dbms = sav replace;
run;

** Regression Procedure **;
    *** data ***;
proc reg data = one;
    *** M ~ X ***;
model neglect = white;
    *** Y ~ X + M ***;
model abuse = white neglect;
run;

** ----------------------------------------------------------------------- **
** Miscellanea **
** ----------------------------------------------------------------------- **

** Regression Procedure **;
    *** data ***;
proc reg data = one;
    *** M ~ X ***;
model neglect = white;
    *** Y ~ X + M ***;
model abuse = white neglect;
    *** Y ~ X ***;
model abuse = white;
run;

** Regression Procedure (2)**;
    *** data ***;
proc reg data = one;
    *** M ~ X ***;
model neglect = white;
    *** Y ~ X + M ***;
model abuse = white neglect;
run;

** Import Procedure (again) **;
proc import datafile="/folders/myfolders/CLASS-CDA/data/child2-med-nums.csv" out = two dbms = csv replace; getnames = yes;
run;
** NOTE: In `R`, I created a dataframe of a subset of the variables (in numeric form) used in the
 mediation analysis (i.e., "abuse", "white", & "boyfriend"), renamed the variables to their
  statistical model names (i.e., "Y", "X", & "M", respectively), removed instances of NAs, and
   saved the resulting dataframe as a *.csv (I did the same thing with the factored variables in
    string form, but have found that the numeric data is much much easier to work with) **;

** Logistic Procedure **;
proc logistic data = two; *order = data descending;
model M = X M;
run;
