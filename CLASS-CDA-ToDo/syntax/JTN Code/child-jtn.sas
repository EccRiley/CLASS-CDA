dm "output;clear;log;clear";
ods results off; ods listing;  *comment out to get figure;
options ps=60;

title 'EHS Child Maltreatment Data';

*replace with your own file path;
proc import datafile="c:\jason\spsswin\cdaclass\child.sav" out=one dbms = sav replace;
run;
