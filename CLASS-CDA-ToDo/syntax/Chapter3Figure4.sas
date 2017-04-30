data ch3example1;
 input prof $ count;
 datalines;
 yes 7
 no 3
 ;
proc freq order=data;
 weight count;
 tables prof /binomial (p=0.8) alpha=.05;
 exact binomial;
run; 
