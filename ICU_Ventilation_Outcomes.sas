
libname char '/data/dart/2014/ORD_AlAly_201403107D/Andrew/Projects/PM_COVID_death/Data';


/**************************************     ICU      *************************************************/

DATA test;
SET A107_SRC.ORDCOVID_PostIndexProcedures(KEEP=PatientICN ICU60d IndexICUAdmitDate);
	WHERE ICU60d=1 and IndexICUAdmitDate <="20APR2021"d;
DROP ICU60d;
RUN;
PROC SORT DATA=test;BY PatientICN;RUN;

* Link ScrSSN using CohortCrosswalk;
PROC SQL;
CREATE TABLE test1 AS 
	SELECT L.*, R.ScrSSN
		FROM test L
	LEFT JOIN A107_SRC.CohortCrosswalk R
		ON L.PatientICN=R.PatientICN;
QUIT;

PROC SORT DATA=test1;BY ScrSSN;RUN;
data test2;
set test1;
by ScrSSN;
if first.ScrSSN;
run;

PROC SQL;
CREATE TABLE test3 AS 
	SELECT L.*, R.IndexICUAdmitDate
		FROM char.deathfinal L
	LEFT JOIN test2 R
		ON L.ScrSSN=R.ScrSSN;
QUIT;

data test4;
set test3;
if IndexICUAdmitDate^=. and -7<=IndexICUAdmitDate-posdate<=30 then icu30d2=1;
else icu30d2=0;
if icu30d2^=0 then icu2time=IndexICUAdmitDate-posdate;
else icu2time="20APR2021"d-posdate;
if icu2time>30 then icu2time=30;
if icu2time<=0 then icu2time=0.001;
run;


/**************************************     MV      *************************************************/

DATA a;
SET A107_SRC.ORDCOVID_PostIndexProcedures(KEEP=PatientICN Ventilator60d IndexVentilatorStartDate);
	WHERE Ventilator60d=1 and IndexVentilatorStartDate<="20APR2021"d;
DROP Ventilator60d;
RUN;
PROC SORT DATA=a;BY PatientICN;RUN;

* Link ScrSSN using CohortCrosswalk;
PROC SQL;
CREATE TABLE b AS 
	SELECT L.*, R.ScrSSN
		FROM a L
	LEFT JOIN A107_SRC.CohortCrosswalk R
		ON L.PatientICN=R.PatientICN;
QUIT;

PROC SORT DATA=b;BY ScrSSN;RUN;
data c;
set b;
by ScrSSN;
if first.ScrSSN;
run;

PROC SQL;
CREATE TABLE d AS 
	SELECT L.*, R.IndexVentilatorStartDate
		FROM test4 L
	LEFT JOIN c R
		ON L.ScrSSN=R.ScrSSN;
QUIT;

data e;
set d;
if IndexVentilatorStartDate^=. and -7<=IndexVentilatorStartDate-posdate<=30 then mv30d2=1;
else mv30d2=0;
if mv30d2^=0 then mv2time=IndexVentilatorStartDate-posdate;
else mv2time="20APR2021"d-posdate;
if mv2time>30 then mv2time=30;
if mv2time<=0 then mv2time=0.001;
run;

proc freq data=e;
table hos30d*icu30d2 hos30d*mv30d2 hos30d*icu30d hos30d*mv30d ;
run;

data char.deathfinal;
set e;
run;

*remove cases where icu or mv happens without a hospitalization - doesn't make a difference in associations;
data f;
set char.deathfinal;
if hos30d=0 and (icu30d2=1 or mv30d2=1) then delete;
run;

proc freq data=f;
table hos30d*icu30d2 hos30d*mv30d2 ;
run;

/************************    ICU AND MV MODELS WITH NEW DEFINITIONS    *******************************/

*outcome rates are slightly higher but associations don't change significantly;

*outcome rates;
proc freq data=char.deathfinal;
table (icu30d icu30d2 mv30d mv30d2)*catpm ;
run;


proc phreg data=char.deathfinal;
   	class state(ref="36") race sex smoke monthz;
   	model icu2time*icu30d2(0)=  pmiqr state age ages race sex smoke monthz adi adis test_num test_nums pos_rate avg_bedx pos_rates avg_bedxs 
							health_food excess_drink health_foods excess_drinks 
							  
							/rl ; 
	ods select ParameterEstimates;
	ods trace on;
	ods show;
run; 

*pm only = 1.15 (1.13-1.18);
*HR top 2 rows = 1.09 (1.06-1.13);

*exercise = 1.04 (1.01-1.07);
*with ndvi but no ndvi spline = 1.05 (1.02-1.08) AIC=235444.74;

*ndvi + ndvi spline = 0.99 (0.96-1.03) AIC=235393.25;
*per point = 1.00 (0.96-1.03);
*pop density = 1.00 (0.97-1.04);
*all = 0.95 (0.92-0.99);

proc phreg data=char.deathfinal;
   	class state(ref="36") race sex smoke monthz;
   	model mv2time*mv30d2(0)=   pmiqr state age race sex smoke adi ndvi test_num pos_rate avg_bedx monthz 
							ages adis ndvis test_nums pos_rates avg_bedxs 
							health_food exercise_opp excess_drink health_foods exercise_opps excess_drinks
							/rl ; 
	ods select ParameterEstimates;
	ods trace on;
	ods show;
run; 

*HR top row only = 1.14 (1.08-1.20);
*splines = 1.11 (1.05-1.17);
*chr = 1.07 (1.02-1.13);

*per point diff = 1.05 (0.99-1.11);
*pop_density = 1.03 (0.97-1.10);
*all = 1.01 (0.95-1.07);


/********************************    CHECK RACE FOR MV   **********************************/


data black white;
set char.deathfinal;
if race = 'B' then output black;
else if race = 'W' then output white;
run;

proc means data=black p25 median p75 min max;
var pm25;
run;

data black;
set black;
if pm25<=7.1 then catpm=1;
else if pm25<=7.9 then catpm=2;
else if pm25<=9.0 then catpm=3;
else catpm=4;
run;

proc means data=white p25 median p75 min max;
var pm25;
run;

data white;
set white;
if pm25<=6.4 then catpm=1;
else if pm25<=7.2 then catpm=2;
else if pm25<=8.2 then catpm=3;
else catpm=4;
run;

proc phreg data=black;
   	class state(ref="36") race sex smoke monthz;
   	model mv2time*mv30d2(0)=   pmiqr state age race sex smoke adi ndvi test_num pos_rate avg_bedx monthz 
							ages adis ndvis test_nums pos_rates avg_bedxs 
							health_food exercise_opp excess_drink health_foods exercise_opps excess_drinks
							/rl ; 
	ods select ParameterEstimates;
	ods trace on;
	ods show;
run; *1.09 (0.97-1.23);

proc phreg data=white;
   	class state(ref="36") race sex smoke monthz;
   	model mv2time*mv30d2(0)=   pmiqr state age race sex smoke adi ndvi test_num pos_rate avg_bedx monthz 
							ages adis ndvis test_nums pos_rates avg_bedxs 
							health_food exercise_opp excess_drink health_foods exercise_opps excess_drinks
							/rl ; 
	ods select ParameterEstimates;
	ods trace on;
	ods show;
run; *1.07 (1.00-1.14);



/*******************************      15 day outcome definitions     *********************************************/

data g;
set char.deathfinal;

if icu30d2^=0 and -7<=IndexICUAdmitDate-posdate<=15 then icu15d=1;
else icu15d=0;
if icu15d^=0 then icu15time=IndexICUAdmitDate-posdate;
else icu15time="20APR2021"d-posdate;
if icu15time>15 then icu15time=15;
if icu15time<=0 then icu15time=0.001;

if mv30d2^=0 and -7<=IndexVentilatorStartDate-posdate<=15 then mv15d=1;
else mv15d=0;
if mv15d^=0 then mv15time=IndexVentilatorStartDate-posdate;
else mv15time="20APR2021"d-posdate;
if mv15time>15 then mv15time=15;
if mv15time<=0 then mv15time=0.001;
run;

proc freq data=g;
table (icu30d icu30d2 icu15d mv30d mv30d2 mv15d)*catpm ;
run;


proc phreg data=g;
   	class state(ref="36") race sex smoke monthz;
   	model icu15time*icu15d(0)=  pmiqr state age ages race sex smoke monthz adi adis test_num test_nums pos_rate avg_bedx pos_rates avg_bedxs 
							health_food excess_drink health_foods excess_drinks
							  
							/rl ; 
	ods select ParameterEstimates;
	ods trace on;
	ods show;
run; 
 *no changes from 30d outcome;


proc phreg data=g;
   	class state(ref="36") race sex smoke monthz;
   	model mv15time*mv15d(0)=   pmiqr state age race sex smoke adi ndvi test_num pos_rate avg_bedx monthz 
							ages adis ndvis test_nums pos_rates avg_bedxs 
							health_food exercise_opp excess_drink health_foods exercise_opps excess_drinks
							/rl ; 
	ods select ParameterEstimates;
	ods trace on;
	ods show;
run; 
 *no changes from 30d outcome;