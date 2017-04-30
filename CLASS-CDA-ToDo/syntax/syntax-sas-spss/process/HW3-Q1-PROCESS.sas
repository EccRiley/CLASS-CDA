** Import Procedure (again) **;
proc import datafile="/folders/myfolders/CLASS-CDA/data/child2-med-nums.csv" out = one dbms = csv replace; getnames = yes;
run;

*PROCESS Macro*;
OPTIONS MSTORED SASMSTORE=macros;
%include"/folders/myfolders/CLASS-CDA/process.sas";
%process(data = one, vars = Y X M, y = Y, x = X, m = M, model = 1);
* See "Model 1" in the "templates.pdf" document provided with A. Hayes' PROCESS macro;
run;
