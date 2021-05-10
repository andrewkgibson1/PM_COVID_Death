libname covpm '/data/dart/2014/ORD_AlAly_201403107D/Miao/COVID_PM';

libname char '/data/dart/2014/ORD_AlAly_201403107D/Andrew/Projects/PM_COVID_death/Data';


data aaa;
*set covpm.covidg_final;
set covpm.pat_pos_cmrb_pm042021;
*where pm25>4.4 and pm25<14.1;
state=substr(fips,1,2);

if hosdate^=. and -7<=hosdate-posdate<=30 then hos30d=1;
else hos30d=0;
if deathdatetime ^=. and datepart(deathdatetime) <= datepart(dischargedatetime) then hosdeath = 1;
else hosdeath = 0;
if hosdeath^=0 and -7<=datepart(deathdatetime)-posdate<=30 then hosdeath30d=1;
else hosdeath30d=0;
if MV_date^=. and -7<=MV_date-posdate<=30 then mv30d=1;
else mv30d=0;
if datepart(admitdatetime)^=. and icu=1 and -7<=datepart(admitdatetime)-posdate<=30 then icu30d=1;
else icu30d=0;

if hos30d^=0 then htime=hosdate-posdate;
else htime="20APR2021"d-posdate;
if htime>30 then htime=30;
if htime<=0 then htime=0.001;

if death30d^=0 then dtime=datepart(deathdatetime)-posdate;
else dtime="20APR2021"d-posdate;
if dtime>30 then dtime=30;
if dtime<=0 then dtime=0.001;

if mv30d^=0 then mtime=MV_date-posdate;
else mtime="20APR2021"d-posdate;
if mtime>30 then mtime=30;
if mtime<=0 then mtime=0.001;

if hosdeath^=0 then do;
	hdtime=datepart(deathdatetime)-posdate;
	hd30time=datepart(deathdatetime)-posdate;
end;
else do; 
	hdtime="20APR2021"d-posdate;
	hd30time="20APR2021"d-posdate;
end;
if hdtime>30 then hd30time=30;
if hdtime<=0 then do;
	hdtime=0.001;
	hd30time=0.001;
end;

if pm25 ne .;

monthz=month(posdate);

run; *n=203,960;

data ndvi;
set covpm.ndvi_c;
run;

proc sort data=aaa; by scrssn; run;

data aaa;
set aaa;
merge aaa(in=a) ndvi;
by scrssn;
if a;
run; *n=;

***
before trim
PM 1% at 4.0 & 14.4;
proc univariate data=aaa;
var pm25;
run;

*trim PM at 1%;
data aaa;
set aaa;
where pm25>4.0 and pm25<14.4;
run; *n=199,692; 

*continuous missing;
proc means data=aaa nmiss;
var pm25 age bmi eGFR_base adi ndvi monthz;
run;
*2813 bmi missing (1.4%);
*17528 eGFR_base missing (8.8%);
*3 ADI missing (<1%);
*none missing in others;

*no categorical missing;
proc freq data=aaa;
tables race sex smoke bmi_cat
cvd copd dementia diabetes htn lungcancer pneumonia stroke  
death30d state;
run;
*missing bmi already assigned as normal weight in "bmi_cat";
*others already assigned?;

*after trim - for median imputation;
proc univariate data=aaa;
var pm25 bmi egfr_base adi;
run;

*median imputation
egfr=75.40
adi=54.50
PM quartiles:
4.1-6.5
6.6-7.3
7.4-8.3
8.4-14.3;
data aaa;
set aaa;
if egfr_base=. then egfr_base=77.94;
if adi=. then adi=54.50;
if bmi=. then bmi=30.46186;
pmiqr=pm25/(8.4-6.6);
run;

data aaa;
set aaa;
if pm25<=6.6 then catpm=1;
else if pm25<=7.4 then catpm=2;
else if pm25<=8.4 then catpm=3;
else catpm=4;
run;

*
Table 1
covvars=  
age race sex state smoke bmi_cat 
cvd copd dementia diabetes egfr_base htn lungcancer pneumonia stroke  
adi ndvi
also follow-up time
;

proc freq data=aaa;
tables (race sex smoke bmi_cat
cvd copd dementia diabetes htn lungcancer pneumonia stroke  
death30d)*catpm;
run;

proc univariate data=aaa ;
var pm25 age eGFR_base adi ndvi bmi htime dtime mtime hdtime;
histogram;
run; 

proc univariate data=aaa ;
var pm25 age eGFR_base adi ndvi htime dtime mtime hdtime;
histogram;
class catpm;
run; 

proc univariate data=char.deathfinal ;
var pm25 ;
run; 

data char.aaa;
set aaa;
run; *n=39,020;

************Going to add some splines for all the continuous variables here
But I think I am going to do very simple NCS with knots at quartiles, not too much...;

proc univariate data=aaa;
var age eGFR_base adi ndvi;
run;

*quartiles
age 50.86 64.51 73.58
eGFR_base 63.98 77.94 90.35
adi 43.35 54.50 63.35
ndvi 0.49 0.62 0.70

;

/*
Run whole of "RCS macro for spline cph regression.sas"
Run commented code at end of program
  change cov1 to covariate and knots1 to quartiles
Copy spline definition from "DELETE.sas" to here
*/

*don't spline month, it is linear;

data char.deathfinal;
set aaa;

ages=((age-50.86)**3)*(age>50.86) 
     -((age-64.51)**3)*(age>64.51) 
     *(73.58-50.86)/(73.58-64.51) 
     +((age-73.58)**3)*(age>73.58) 
     *(64.51-50.86)/(73.58-64.51);

eGFR_bases=((eGFR_base-63.98)**3)*(eGFR_base>63.98) 
     -((eGFR_base-77.94)**3)*(eGFR_base>77.94) 
     *(90.35-63.98)/(90.35-77.94) 
     +((eGFR_base-90.35)**3)*(eGFR_base>90.35) 
     *(77.94-63.98)/(90.35-77.94);

adis=((adi-43.35)**3)*(adi>43.35) 
     -((adi-54.50)**3)*(adi>54.50) 
     *(63.35-43.35)/(63.35-54.50) 
     +((adi-63.35)**3)*(adi>63.35) 
     *(54.50-43.35)/(63.35-54.50);  

ndvis=((ndvi-0.49)**3)*(ndvi>0.49) 
     -((ndvi-0.62)**3)*(ndvi>0.62) 
     *(0.70-0.49)/(0.70-0.62) 
     +((ndvi-0.70)**3)*(ndvi>0.70) 
     *(0.62-0.49)/(0.70-0.62);

run;


data test;
set char.deathfinal;
if year(posdate) = 2021 and monthz = 1 then monthz=13;
else if year(posdate) = 2021 and monthz = 2 then monthz=14;
else if year(posdate) = 2021 and monthz = 3 then monthz=15;
else if year(posdate) = 2021 and monthz = 4 then monthz=16;
else monthz=monthz;
run;


proc univariate data=test;
var  monthz;
run; *9 12 13;

data char.deathfinal;
set test;
run;
/***********************************************************/

*testing things here;

proc freq data=char.deathfinal;
table monthz*catpm*death30d / nocol nofreq nopercent nocum;
run;

proc freq data=char.deathfinal;
table monthz*catpm / nocol nofreq nopercent nocum;
run;

proc corr data=char.deathfinal;
var pm25 monthz;
run;

proc freq data=char.deathfinal;
table monthz*race / nocol nofreq nopercent nocum;
run;

proc freq data=char.deathfinal;
table race*hos30d*death30d ;
run;

proc freq data=char.deathfinal;
table catpm*hos30d*death30d ;
where race="B";
run;

proc freq data=char.deathfinal;
table catpm*hos30d*death30d ;
where race="W";
run;

