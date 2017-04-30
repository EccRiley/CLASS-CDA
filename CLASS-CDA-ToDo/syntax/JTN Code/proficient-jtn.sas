dm "output;clear;log;clear";
*ods results off; ods listing;  *comment out to get figure;
options ps=60;

title 'Azen & Walker proficiency data from Chapter 3';

proc import datafile="c:\jason\spsswin\cdaclass\proficient.sav" out=one dbms = sav replace;
run;

data two; set one;
level = level - 1;

proc format;
 value level
	0="minimal"
    1="basic"
	2="proficient"
	3="advanced";
 run;

proc freq data=two; tables level;
	format  level level.;
run;

proc univariate data=two noprint;
      histogram level / midpoints=0 1 2 3 ctext = blue;
	  format level level.;
run;
