
******going to do subgroup and formal interaction here;

proc univariate data=char.deathfinal;
var age adi;
run;

data vvv;
set char.deathfinal;
if age <=50.86 then catage=1;
else if age<=64.51 then catage=2;
else if age<=73.58 then catage=3;
else  catage=4;

if age<=64.51 then catage2=1; else catage2=2;


if adi <=43.35 then catadi=1;
else if adi<=54.50 then catadi=2;
else if adi<=63.35 then catadi=3;
else  catadi=4;

if adi<=54.50 then catadi2=1; else catadi2=2;

if race="B" then brace=1; else brace=0;

run;


/******************************* race **************************************/

/*********** subgroups ******************/

/** black **/

proc phreg data=vvv;
   	class state(ref="36") race sex smoke monthz longtermcare catndvi;
   	model htime*hosdeath(0)= pmiqr state age race sex smoke health_food  excess_drink
							 adi catndvi test_num pos_rate avg_bedx monthz
							ages health_foods  excess_drinks  adis  
							 test_nums pos_rates avg_bedxs avg_icux avg_icuxs
							/rl ; 
	where race="B";
	ods select ParameterEstimates;
	ods trace on;
	ods show;
run; *HR=1.16 (1.02-1.31);

/** other **/

proc phreg data=vvv;
   	class state(ref="36") race sex smoke monthz longtermcare catndvi;
   	model htime*hosdeath(0)= pmiqr state age race sex smoke health_food  excess_drink
							 adi catndvi test_num pos_rate avg_bedx monthz
							ages health_foods  excess_drinks  adis  
							 test_nums pos_rates avg_bedxs avg_icux avg_icuxs
							/rl ; 
	where race="O";
	ods select ParameterEstimates;
	ods trace on;
	ods show;
run; *HR=1.15 (0.96-1.39);

/** white **/

proc phreg data=vvv;
   	class state(ref="36") race sex smoke monthz longtermcare catndvi;
   	model htime*hosdeath(0)= pmiqr state age race sex smoke health_food  excess_drink
							 adi catndvi test_num pos_rate avg_bedx monthz
							ages health_foods  excess_drinks  adis  
							 test_nums pos_rates avg_bedxs avg_icux avg_icuxs
							/rl ; 
	where race="W";
	ods select ParameterEstimates;
	ods trace on;
	ods show;
run; *HR=1.14 (1.07-1.22);


/*********** interactions ******************/


proc phreg data=vvv;
   	class state(ref="36") race sex smoke monthz longtermcare catndvi;
   	model htime*hosdeath(0)= pmiqr pmiqr*race state age race sex smoke health_food  excess_drink
							 adi catndvi test_num pos_rate avg_bedx monthz
							ages health_foods  excess_drinks  adis  
							 test_nums pos_rates avg_bedxs avg_icux avg_icuxs
							/rl ;
	hazardratio pmiqr / diff=ref at(race=all); 
run; 
/* 
B HR=1.12 (1.03-1.22)
O HR=1.17 (1.05-1.31) 
W HR=1.15 (1.09-1.22) 
p=0.7842
*/


/*********** black vs all others ******************/

*same conclusions as black vs others vs white;

proc phreg data=vvv;
   	class state(ref="36") race sex smoke monthz longtermcare catndvi;
   	model htime*hosdeath(0)= pmiqr state age race sex smoke health_food  excess_drink
							 adi catndvi test_num pos_rate avg_bedx monthz
							ages health_foods  excess_drinks  adis  
							 test_nums pos_rates avg_bedxs avg_icux avg_icuxs
							/rl ; 
	where brace=1;
	ods select ParameterEstimates;
	ods trace on;
	ods show;
run; *HR=1.16 (1.02-1.31);

proc phreg data=vvv;
   	class state(ref="36") race sex smoke monthz longtermcare catndvi;
   	model htime*hosdeath(0)= pmiqr state age race sex smoke health_food  excess_drink
							 adi catndvi test_num pos_rate avg_bedx monthz
							ages health_foods  excess_drinks  adis  
							 test_nums pos_rates avg_bedxs avg_icux avg_icuxs
							/rl ; 
	where brace=0;
	ods select ParameterEstimates;
	ods trace on;
	ods show;
run; *HR=1.14 (1.07-1.21);

/*********** interactions ******************/

proc phreg data=vvv ;
   	class state(ref="36") brace sex smoke monthz longtermcare catndvi;
   	model htime*hosdeath(0)= pmiqr pmiqr*brace state age brace sex smoke health_food  excess_drink
							 adi catndvi test_num pos_rate avg_bedx monthz
							ages health_foods  excess_drinks  adis  
							 test_nums pos_rates avg_bedxs avg_icux avg_icuxs
							/rl ; 

	hazardratio pmiqr / diff=ref at(brace=all); 
run; 
*Brace=0 HR=1.16 (1.10-1.23);
*Brace=1 HR=1.12 (1.03-1.21);
*p=0.3751;



/**********************************  AGE  **********************************************************/

/*********** subgroups ******************/

proc phreg data=vvv;
   	class state(ref="36") race sex smoke monthz longtermcare catndvi;
   	model htime*hosdeath(0)= pmiqr state age race sex smoke health_food  excess_drink
							 adi catndvi test_num pos_rate avg_bedx monthz
							ages health_foods  excess_drinks  adis  
							 test_nums pos_rates avg_bedxs avg_icux avg_icuxs
							/rl ; 
	where 50.86 - 10 <= age <= 50.86 + 10;
	ods select ParameterEstimates;
	ods trace on;
	ods show;
run; *HR=1.11 (0.86-1.42);

proc phreg data=vvv;
   	class state(ref="36") race sex smoke monthz longtermcare catndvi;
   	model htime*hosdeath(0)= pmiqr state age race sex smoke health_food  excess_drink
							 adi catndvi test_num pos_rate avg_bedx monthz
							ages health_foods  excess_drinks  adis  
							 test_nums pos_rates avg_bedxs avg_icux avg_icuxs
							/rl ; 
	where 73.58 - 10 <= age <= 73.58 + 10;
	ods select ParameterEstimates;
	ods trace on;
	ods show;
run; *HR=1.13 (1.05-1.20);


/*********** interactions ******************/

proc phreg data=vvv;
   	class state(ref="36") race sex smoke monthz longtermcare catndvi;
   	model htime*hosdeath(0)= pmiqr pmiqr*age state age race sex smoke health_food  excess_drink
							 adi catndvi test_num pos_rate avg_bedx monthz
							ages health_foods  excess_drinks  adis  
							 test_nums pos_rates avg_bedxs avg_icux avg_icuxs
							/rl ; 
	hazardratio pmiqr / diff=ref at(age=50.86 73.58); 
run; 
*young HR=1.23 (1.12-1.35);
*old HR=1.16 (1.10-1.22);
*p=0.0923;



/************************  SEX  ****************************************/

/*********** subgroups ******************/

proc phreg data=vvv;
   	class state(ref="36") race sex smoke monthz longtermcare catndvi;
   	model htime*hosdeath(0)= pmiqr state age race sex smoke health_food  excess_drink
							 adi catndvi test_num pos_rate avg_bedx monthz
							ages health_foods  excess_drinks  adis  
							 test_nums pos_rates avg_bedxs avg_icux avg_icuxs
							/rl ; 
	where sex="M";
	ods select ParameterEstimates;
	ods trace on;
	ods show;
run; *HR=1.15 (1.09-1.21);

proc phreg data=vvv;
   	class state(ref="36") race sex smoke monthz longtermcare catndvi;
   	model htime*hosdeath(0)= pmiqr state age race sex smoke health_food  excess_drink
							 adi catndvi test_num pos_rate avg_bedx monthz
							ages health_foods  excess_drinks  adis  
							 test_nums pos_rates avg_bedxs avg_icux avg_icuxs
							/rl ; 
	where sex="F";
	ods select ParameterEstimates;
	ods trace on;
	ods show;
run; *HR=1.05 (0.69-1.60);


/*********** interactions ******************/

proc phreg data=vvv;
    class state(ref="36") race sex smoke monthz longtermcare catndvi;
   	model htime*hosdeath(0)= pmiqr pmiqr*sex state age race sex smoke health_food  excess_drink
							 adi catndvi test_num pos_rate avg_bedx monthz
							ages health_foods  excess_drinks  adis  
							 test_nums pos_rates avg_bedxs avg_icux avg_icuxs
							/rl ; 
	hazardratio pmiqr / diff=ref at(sex=all); 
run; 
* F = 1.17 (0.91-1.50)
M = 1.15 (1.09-1.21)
p=0.8875;


/****************************  ADI  ********************************************************************/

/*********** subgroups ******************/

proc phreg data=vvv;
   	class state(ref="36") race sex smoke monthz longtermcare catndvi;
   	model htime*hosdeath(0)= pmiqr state age race sex smoke health_food  excess_drink
							  catndvi test_num pos_rate avg_bedx monthz
							ages health_foods  excess_drinks    
							 test_nums pos_rates avg_bedxs avg_icux avg_icuxs
							/rl ; 
	where catadi2=1;
	ods select ParameterEstimates;
	ods trace on;
	ods show;
run; *HR=1.17 (1.08-1.26);

proc phreg data=vvv;
   	class state(ref="36") race sex smoke monthz longtermcare catndvi;
   	model htime*hosdeath(0)= pmiqr state age race sex smoke health_food  excess_drink
							 catndvi test_num pos_rate avg_bedx monthz
							ages health_foods  excess_drinks   
							 test_nums pos_rates avg_bedxs avg_icux avg_icuxs
							/rl ; 
	where catadi2=2;
	ods select ParameterEstimates;
	ods trace on;
	ods show;
run; *HR=1.15 (1.06-1.25);


/*********** interactions ******************/

proc phreg data=vvv;
   	class state(ref="36") race sex smoke monthz longtermcare catndvi;
   	model htime*hosdeath(0)= pmiqr pmiqr*adi state age race sex smoke health_food  excess_drink
							 adi catndvi test_num pos_rate avg_bedx monthz
							ages health_foods  excess_drinks adis
							 test_nums pos_rates avg_bedxs avg_icux avg_icuxs
							/rl ;
	hazardratio pmiqr / diff=ref at(adi=43.35 63.35); 
run; 
*low ADI = 1.17 (1.10-1.23);
*high ADI = 1.12 (1.05-1.19);
*p=0.1031;








/************************************* Testing the race story *********************************/

data vvv;
set vvv;
week = week(posdate);
run;

proc sql;
create table aaa as
select a.*, b.*
from vvv as a
inner join CHR2020_CLEAN as b
on a.FIPS_NUM = b.FIPS;
quit;

proc phreg data=aaa;
   	class state(ref="36") race sex smoke bmi_cat(ref="Norm/Underweight") cvd(ref="0") copd(ref="0") 
	dementia(ref="0") diabetes(ref="0") htn(ref="0") lungcancer(ref="0") pneumonia(ref="0") stroke(ref="0") catage;
   	model time*death30d(0)= pmiqr  state age sex smoke adi ndvi 
	bmi_cat cvd copd dementia diabetes egfr_base htn lungcancer pneumonia stroke/rl ; 
	where race="B" ;
run;

proc phreg data=aaa;
   	class state(ref="36") race sex smoke bmi_cat(ref="Norm/Underweight") cvd(ref="0") copd(ref="0") 
	dementia(ref="0") diabetes(ref="0") htn(ref="0") lungcancer(ref="0") pneumonia(ref="0") stroke(ref="0") segq2;
   	model time*death30d(0)= pmiqr pmiqr*segregation_bw segregation_bw race state age sex smoke adi ndvi
	bmi_cat cvd copd dementia diabetes egfr_base htn lungcancer pneumonia stroke/rl ; 
	hazardratio pmiqr / diff=ref at(segregation_bw=41.7056 58.8840); 
run; 

data aaa;
set aaa;
if ventilator = . then ventilator = 0;
run;

proc freq data=aaa;
table death30d*catage;
where race='B';
run;

proc means data=aaa;
var age ndvi adi;
class segq;
run;

proc corr data=aaa;
var pm25 age;
where race='B';
run;

proc univariate data=aaa;
var segregation_BW;
run;

data aaa;
set aaa;
if segregation_bw=. then segregation_bw = 49.4653;
if segregation_bw <= 41.7056 then segq = 1;
else if segregation_bw <= 49.4953 then segq = 2;
else if segregation_bw <= 58.8840 then segq = 3;
else segq = 4;
if segq = 1 then segq2 = 1;
else segq2 = 2;
run;

proc freq data=aaa;
table segq*death30d;
run;

proc freq data=aaa;
table segq*catpm;
run;






/**/
/*data xxx;*/
/*set char.hos_finalx;*/
/**adi=adi/10;*/
/**age=age/10;*/
/*if 55.715263518>=age>=40.495550992then vage=1;*/
/*if 47.378620565>=adi>=31.546673529 then vadi=1;*/
/**/
/*if 	77.11156742>=age>=	70.116358658 then vage=2;*/
/*if 	66.2740379>=adi>=	58.440685892 then vadi=2;*/
/*				*/
/*				*/
/**/
/*run;*/
/**/
/**/
/*proc logistic data=xxx desc ;*/
/*class state(ref="36") race sex smoke bmi_cat;*/
/*model hos30d=pmiqr adi race sex smoke health_food exercise_opp excess_drink*/
/*pop_density  ndvi bmi_cat copd lungcancer cvd stroke dementia */
/*diabetes htn pneumonia egfr_base state /rl; */
/*where vage=1;*/
/*run;*/
/**/
/**/
/*proc univariate data=char.hos_finalx;*/
/*var age adi;*/
/*output out=uni pctlpre=age adi pctlpts= 15 35 65 85;*/
/*run;*/



