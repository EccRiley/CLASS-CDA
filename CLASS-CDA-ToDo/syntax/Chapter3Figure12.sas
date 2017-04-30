data ch3example2;
 input prof $ count;
 datalines;
  advanced 18644
  proficient 32269
  basic 10039
  minimal 10757
 ;
proc freq order=data;
 weight count;
 tables prof / testp=(0.15 0.40 0.30 0.15);
run;
