dm "output;clear;log;clear";
ods results off; ods listing;  *comment out to get figure;
options ps=60;

title '2014 Portland Police Racial Profiling Data';

*replace with your own file path;
proc import datafile="c:\jason\spsswin\cdaclass\race.sav" out=one dbms = sav replace;
run;
