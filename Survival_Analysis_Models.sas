
libname char '/data/dart/2014/ORD_AlAly_201403107D/Andrew/Projects/PM_COVID_death/Data';

*outcome rates;
proc freq data=char.deathfinal ;
table (hos30d icu30d icu30d2 mv30d mv30d2 hosdeath death30d)*catpm / nofreq norow nopercent;
run;

proc univariate data=char.deathfinal;
var los;
run;
proc sort data=char.deathfinal; by catpm;run;
proc univariate data=char.deathfinal;
by catpm;
var los;
run;


/****************************************   hospitalization   **************************************** */

*fully adjusted;
proc phreg data=char.deathfinal;
   	class state(ref="36") race sex smoke monthz longtermcare;
   	model htime*hos30d(0)= pmiqr state age race sex smoke health_food exercise_opp excess_drink
							pop_density adi ndvi per_point_diff test_num pos_rate avg_bedx monthz
							ages health_foods exercise_opps excess_drinks pop_densitys adis ndvis 
							per_point_diffs test_nums pos_rates avg_bedxs avg_icux avg_icuxs longtermcare
							/rl ; 
	ods select ParameterEstimates;
	ods trace on;
	ods show;
run; *HR=1.09 (1.07-1.11);

*final model;
proc phreg data=char.deathfinal;
   	class state(ref="36") race sex smoke monthz longtermcare catndvi;
   	model htime*hos30d(0)= pmiqr state age race sex smoke health_food  excess_drink
							 adi catndvi test_num pos_rate avg_bedx monthz
							ages health_foods  excess_drinks  adis  
							 test_nums pos_rates avg_bedxs avg_icux avg_icuxs 
							/rl ; 
	ods select ParameterEstimates;
	ods trace on;
	ods show;
run; *HR=1.18 (1.16-1.20) - hosp paper has HR=1.08;

/****************************************    icu    **************************************** */

*fully adjusted - Miao definition;
proc phreg data=char.deathfinal;
   	class state(ref="36") race sex smoke monthz longtermcare;
   	model htime*icu30d(0)= pmiqr state age race sex smoke health_food exercise_opp excess_drink
							pop_density adi ndvi per_point_diff test_num pos_rate avg_bedx monthz
							ages health_foods exercise_opps excess_drinks pop_densitys adis ndvis 
							per_point_diffs test_nums pos_rates avg_bedxs avg_icux avg_icuxs longtermcare
							/rl ; 
	ods select ParameterEstimates;
	ods trace on;
	ods show;
run; *HR=0.96 (0.92-1.00);

*final model;
proc phreg data=char.deathfinal;
   	class state(ref="36") race sex smoke monthz longtermcare catndvi;
   	model icu2time*icu30d2(0)= pmiqr state age race sex smoke health_food  excess_drink
							 adi catndvi test_num pos_rate avg_bedx monthz
							ages health_foods  excess_drinks  adis  
							 test_nums pos_rates avg_bedxs avg_icux avg_icuxs 
							/rl ; 
	ods select ParameterEstimates;
	ods trace on;
	ods show;
run; *HR=1.05 (1.02-1.08);


*pm only = 1.15 (1.13-1.18);
*HR top 2 rows = 1.09 (1.05-1.12);
*ndvi = 0.99 (0.96-1.03);
*exercise = 1.02 (0.98-1.05);
*per point = 1.00 (0.96-1.03);
*pop density = 1.00 (0.97-1.04);
*all = 0.95 (0.92-0.99);

/****************************************    mv    **************************************** */

*fully adjusted - Miao definition;
proc phreg data=char.deathfinal;
   	class state(ref="36") race sex smoke monthz longtermcare;
   	model mtime*mv30d(0)=  pmiqr state age race sex smoke health_food exercise_opp excess_drink
							pop_density adi ndvi per_point_diff test_num pos_rate avg_bedx monthz
							ages health_foods exercise_opps excess_drinks pop_densitys adis ndvis 
							per_point_diffs test_nums pos_rates avg_bedxs avg_icux avg_icuxs longtermcare
							/rl ; 
	ods select ParameterEstimates;
	ods trace on;
	ods show;
run; *HR=1.03 (0.97-1.09);

*final model;
proc phreg data=char.deathfinal;
   	class state(ref="36") race sex smoke monthz longtermcare catndvi;
   	model mv2time*mv30d2(0)= pmiqr state age race sex smoke health_food  excess_drink
							 adi catndvi test_num pos_rate avg_bedx monthz
							ages health_foods  excess_drinks  adis  
							 test_nums pos_rates avg_bedxs avg_icux avg_icuxs 
							/rl ; 
	ods select ParameterEstimates;
	ods trace on;
	ods show;
run; *HR=1.11 (1.05-1.16);

*HR top row only = 1.14 (1.08-1.20);
*splines = 1.11 (1.05-1.17);
*chr = 1.07 (1.01-1.13);
*per point diff = 1.05 (0.99-1.11);
*pop_density = 1.03 (0.97-1.10);
*all = 1.01 (0.95-1.07);

/****************************************    los   **************************************** */

data test;
set char.deathfinal;
if hos=0 then los=0;
run;

*fully adjusted ;
proc glm data=test;
   	class state(ref="36") race sex smoke monthz longtermcare;
   	model los= pmiqr state age race sex smoke health_food exercise_opp excess_drink
							pop_density adi ndvi per_point_diff test_num pos_rate avg_bedx monthz
							ages health_foods exercise_opps excess_drinks pop_densitys adis ndvis 
							per_point_diffs test_nums pos_rates avg_bedxs avg_icux avg_icuxs longtermcare
							/solution; 
run; *beta=0.1317 (std err=0.0313, p <0.001);

*final model;
proc glm data=test;
   	class state(ref="36") race sex smoke monthz longtermcare catndvi;
   	model los= pmiqr state age race sex smoke health_food  excess_drink
							 adi catndvi test_num pos_rate avg_bedx monthz
							ages health_foods  excess_drinks  adis  
							 test_nums pos_rates avg_bedxs avg_icux avg_icuxs
							/solution; 
run; *beta=0.2994 (std err=0.0296, p <0.001);

/****************************************    hosdeath   **************************************** */

*fully adjusted;
proc phreg data=char.deathfinal;
   	class state(ref="36") race sex smoke monthz longtermcare;
   	model htime*hosdeath(0)= pmiqr state age race sex smoke health_food exercise_opp excess_drink
							pop_density adi ndvi per_point_diff test_num pos_rate avg_bedx monthz
							ages health_foods exercise_opps excess_drinks pop_densitys adis ndvis 
							per_point_diffs test_nums pos_rates avg_bedxs avg_icux avg_icuxs longtermcare
							/rl ; 
	ods select ParameterEstimates;
	ods trace on;
	ods show;
run; *HR=1.08 (1.02-1.15);

*final model;
proc phreg data=char.deathfinal;
   	class state(ref="36") race sex smoke monthz longtermcare catndvi;
   	model htime*hosdeath(0)= pmiqr state age race sex smoke health_food  excess_drink
							 adi catndvi test_num pos_rate avg_bedx monthz
							ages health_foods  excess_drinks  adis  
							 test_nums pos_rates avg_bedxs avg_icux avg_icuxs
							/rl ; 
	ods select ParameterEstimates;
	ods trace on;
	ods show;
run; *HR=1.15 (1.09-1.21);


/*************************************    ANY DEATH      ********************************************************************/

*final model;
proc phreg data=char.deathfinal;
   	class state(ref="36") race sex smoke monthz longtermcare catndvi;
   	model dtime*death30d(0)= pmiqr state age race sex smoke health_food  excess_drink
							 adi catndvi test_num pos_rate avg_bedx monthz
							ages health_foods  excess_drinks  adis  
							 test_nums pos_rates avg_bedxs avg_icux avg_icuxs
							/rl ; 
	ods select ParameterEstimates;
	ods trace on;
	ods show;
run; *HR=1.04 (1.01-1.07);