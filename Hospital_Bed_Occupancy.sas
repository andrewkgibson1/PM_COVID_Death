
libname char '/data/dart/2014/ORD_AlAly_201403107D/Andrew/Projects/PM_COVID_death/Data';

/***************************   HOSPITAL OCCUPANCY  ************************************************/

* per hospital per week;

data census;
set a107_src.inpat_dailycensus;
where "20APR2021"d >= censusdate >= "01FEB2020"d;
keep bedsoperating bedsauthorized patientsremaining CensusDate sta3n wardlocationsid;
run;

PROC SQL;
CREATE TABLE ICU_Specialty AS
SELECT SpecialtySID, PTFCode, Specialty, 
	CASE WHEN PTFCode IN ('12', '13', '63') 
		AND Specialty IN ('MEDICAL ICU', 'SURGICAL ICU', 'CARDIAC INTENSIVE CARE UNIT')
			THEN 1 ELSE 0 END AS ICU
	FROM DIM_RB03.Specialty;
QUIT;

data ICU_Specialty;
set ICU_Specialty;
where icu=1;
run;

data ward;
set dim_rb03.WardLocation;
run;

proc sql;
create table capacity as
select a.*, b.* 
from census as a left join ward as b 
on a.wardlocationsid=b.wardlocationsid;
quit;

proc sql;
create table capacity1 as
select a.*, b.* 
from capacity as a left join ICU_Specialty as b 
on a.Specialtysid=b.Specialtysid;
quit;

data hoscapacity;
set capacity1;
keep sta3n sta6a bedsoperating censusdate patientsremaining bedsauthorized WardLocationname specialty icu;
run; 
PROC SORT DATA=hoscapacity;
	BY Sta3n CensusDate;
RUN;

data capacity;
set hoscapacity;
where bedsoperating>0;
run;

data capacity1;
set capacity;
by sta3n CensusDate;
retain totbed toticu Bedp icup;
if first.censusdate then do;
totbed=0; toticu=0; bedp=0; icup=0;
end;
totbed=totbed+bedsauthorized;
bedp=bedp+patientsremaining;

if icu=1 then do;
toticu=toticu+bedsauthorized;
icup=icup+patientsremaining;
end;
if last.censusdate then output;
run;

data capacity2;
set capacity1;
year=year(censusdate);
week=week(censusdate);
P_bed=bedp/totbed;
p_icu=icup/toticu;
run;

proc sort data=capacity2;
by sta3n year week;
run;

data capacity3;
set capacity2;
by sta3n year week;
retain avg_bed avg_icu count;
if first.week then do;
count=0;avg_bed=0; avg_icu=0;
end;
count=count+1;
avg_bed=p_bed+avg_bed;
avg_icu=p_icu+avg_icu;
if last.week then do;
avg_bed=avg_bed/count;
avg_icu=avg_icu/count;
output;
end;
run;

data hoscapacity_avg;
set capacity3;
keep sta3n year week censusdate avg_bed avg_icu;
run;

************************;

data test;
set char.deathfinal;
week=week(posdate);
year=year(posdate);
if week(posdate) = 0 then do;
	prev_week = 52;
	prev_year = year(posdate)-1;
	end;
else do;
	prev_week=week(posdate)-1;
	prev_year = year(posdate);
	end;
run;

proc sql;
create table test as
select a.*, b.avg_bed, b.avg_icu 
from test as a 
left join hoscapacity_avg as b 
on a.Sta3n=b.Sta3n and a.prev_week=b.week and a.prev_year=b.year;
quit;

data char.deathfinal;
set test;
avg_bedx = avg_bed * 100;
avg_icux = avg_icu * 100;
run;

proc univariate data=char.deathfinal;
var avg_bedx avg_bed avg_icux avg_icu;
hist;
run;

*4,140 hos missing (2%);
*19,867 ICU missing (9.95%);
* median impute;

data char.deathfinal;
set char.deathfinal;
if avg_bed=. then do;
	avg_bed=0.3833;
	avg_bedx=38.33;
end;
if avg_icu=. then do;
	avg_icu=0.3730;
	avg_icux=37.30;
end;
run;

*spline new var;
data char.deathfinal;
set char.deathfinal;

avg_bedxs=((avg_bedx-30.27)**3)*(avg_bedx>30.27) 
     -((avg_bedx-38.33)**3)*(avg_bedx>38.33) 
     *(45.01-30.27)/(45.01-38.33) 
     +((avg_bedx-45.01)**3)*(avg_bedx>45.01) 
     *(38.33-30.27)/(45.01-38.33); 

avg_beds=((avg_bed-.3027)**3)*(avg_bed>.3027) 
     -((avg_bed-.3833)**3)*(avg_bed>.3833) 
     *(.4501-.3027)/(.4501-.3833) 
     +((avg_bed-.4501)**3)*(avg_bed>.4501) 
     *(.3833-.3027)/(.4501-.3833);

avg_icuxs=((avg_icux-24.29)**3)*(avg_icux>24.29) 
     -((avg_icux-37.30)**3)*(avg_icux>37.30) 
     *(51.26-24.29)/(51.26-37.30) 
     +((avg_icux-51.26)**3)*(avg_icux>51.26) 
     *(37.30-24.29)/(51.26-37.30);

run;