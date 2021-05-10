/***************************** testing and positivity rates ******************************/

DATA aaa;
SET A107_SRC.ORDCOVID_CaseDetail
(KEEP=PatientICN PatientSID CaseDefinition CaseDateTime PatientCategory sta6a);
Date = DATEPART(CaseDateTime);
if year(Date) = 2021 and month(Date) = 1 then Month=13;
else if year(Date) = 2021 and month(Date) = 2 then Month=14;
else if year(Date) = 2021 and month(Date) = 3 then Month=15;
else if year(Date) = 2021 and month(Date) = 4 then Month=16;
else Month = month(Date);
station = sta6a;
FORMAT Date MMDDYY10.;
if CaseDefinition = "VA Confirmed Positive" then pos = 1;
else if CaseDefinition = "VA Presumptive Positive" then pos = 1;
else if CaseDefinition = "VA Negative" then pos = 0;
else if CaseDefinition = "VA Pending" then pos = 0;
else if CaseDefinition = "VA Respiratory Illness Seen" then pos = 0;
RUN; *n=533,079;

proc sort data=aaa;
by patientICN;
run;

PROC SQL; SELECT COUNT(UNIQUE(PatientICN)) FROM aaa; QUIT; *39,020;

proc freq data=aaa;
table CaseDefinition station month;
run;

data bbb;
set aaa;
station = substr(station,1,3);
sta3n = input(station,3.);
drop CaseDateTime Sta6a;
run;

proc sql;
create table ccc as
select a.*, b.state
from bbb as a
left join char.station as b
on a.sta3n=b.sta3n;
run;

data ccc;
set ccc;
state = strip(state);
run;

proc freq data=ccc ;
table state*month / out=rates;
run;

proc sql;
create table ddd as
select a.*, b.count as test_num
from ccc as a
left join rates as b
on a.state=b.state and a.month=b.month
where a.state ne "";
run;

proc freq data=ddd ;
table state*month / out=rates2;
where pos=1;
run;

proc sql;
create table eee as
select a.*, b.count as pos_num
from ddd as a
left join rates2 as b
on a.state=b.state and a.month=b.month;
run;

data eee;
set eee;
pos_rate = pos_num / test_num;
run;

proc sort data=eee out=x;
by sta3n month;
run;

data y;
set x;
by sta3n month;
if first.month;
run;

proc sql;
create table char.deathfinal as
select a.*, b.test_num, b.pos_rate
from char.deathfinal as a
left join y as b
on a.monthz=b.month and a.sta3n=b.sta3n;
run;

proc univariate data=char.deathfinal;
var test_num pos_rate;
run;
*test_num median = 3256;
*pos_rate median = 0.2403;

data test;
set char.deathfinal;
if test_num =. or pos_rate =.;
run;
*all from sta3n = 740 (no state associated with this...Puerto Rico?);
*n=2,015;

*median imputation (1.0% missing);
data char.deathfinal;
set char.deathfinal;
if test_num =. then test_num=3256;
if pos_rate =. then pos_rate=0.2403;
run;

proc univariate data=char.deathfinal;
var test_num pos_rate;
run; 

proc univariate data=char.deathfinal;
var test_num pos_rate;
histogram;
class catpm;
run; 

*spline new vars;
data char.deathfinal;
set char.deathfinal;

test_nums=((test_num-1879)**3)*(test_num>1879) 
     -((test_num-3256)**3)*(test_num>3256) 
     *(6379-1879)/(6379-3256) 
     +((test_num-6379)**3)*(test_num>6379) 
     *(3256-1879)/(6379-3256); 

pos_rates=((pos_rate-0.1499)**3)*(pos_rate>0.1499) 
     -((pos_rate-0.2403)**3)*(pos_rate>0.2403) 
     *(0.3188-0.1499)/(0.3188-0.2403) 
     +((pos_rate-0.3188)**3)*(pos_rate>0.3188) 
     *(0.2403-0.1499)/(0.3188-0.2403);

run;