/* imports rmet_winzor.csv file*/
	/*rmet_winzor.csv file does NOT contain outliers (outliers excluded)- all RMET scores have been WINZORIZED*/
	* contains 3815 observations and 121 variables *;
proc import datafile='/folders/myshortcuts/myfolder/rmet_winzor.csv' 
		out=work.rmet dbms=csv;
run;

* 	RMET = orginal dataframe - character and numeric variables, if exposure=0, but age indicated --> age==. - missing-
	RMET_select = orginal data, minus character variables, no combined variables (3815 obs.)
	RMET2 = data with combined vars, ages and exposures [only differs from complete_cases by rows filtered out] (3815 obs.)
	complete_cases = data filtered by flag (complete demographic), combined variables, ages, exposures (2628 obs.);


/* shows dataset structure (same as str() in R) */;
proc contents data=rmet;
run;

/* convert age variables and demographics from characters to numerics */;
/* original age variable "~AGE" is character "~AGE_n" is numeric*/;
data rmet;
set rmet;
if parentdeathAGE = 'NA' then parentdeathAGE = .;
parentdeathAGE_n= parentdeathAGE * 1;
if parentdivorceAGE = 'NA' then parentdivorceAGE = .;
parentdivorceAGE_n = parentdivorceAGE * 1;
if fosterAGE = 'NA' then fosterAGE = .;
fosterAGE_n = fosterAGE * 1;
if institutionAGE = 'NA' then institutionAGE = .;
institutionAGE_n = institutionAGE * 1;
if suicideAGE = 'NA' then suicideAGE = .;
suicideAGE_n = suicideAGE * 1;
if prisonAGE = 'NA' then prisonAGE = .;
prisonAGE_n = prisonAGE * 1;
if verbalAGE1 = 'NA' then verbalAGE1 = .;
verbalAGE_n = verbalAGE1 * 1;
if fearabuseAGE1 = 'NA' then fearabuseAGE1 = .;
fearabuseAGE_n = fearabuseAGE1 * 1;
if physabuse1AGE1 = 'NA' then physabuse1AGE1 = .;
physabuse1AGE_n = physabuse1AGE1 * 1;
if physabuse2AGE1 = 'NA' then physabuse2AGE1 = .;
physabuse2AGE_n = physabuse2AGE1 * 1;
if physinjuryAGE1 = 'NA' then physinjuryAGE1 = .;
physinjuryAGE_n = physinjuryAGE1 * 1;
if sexabuse1AGE1 = 'NA' then sexabuse1AGE1 = .;
sexabuse1AGE_n = sexabuse1AGE1 * 1;
if sexabuse2AGE1 = 'NA' then sexabuse2AGE1 = .;
sexabuse2AGE_n = sexabuse2AGE1 * 1;
if domesticviolence1AGE1 = 'NA' then domesticviolence1AGE1 = .;
domesticviolence1AGE_n = domesticviolence1AGE1 * 1;
if domesticviolence2AGE1 = 'NA' then domesticviolence2AGE1 = .;
domesticviolence2AGE_n = domesticviolence2AGE1 * 1;
if race_ethnicity = 'NA' then race_ethnicity = .;							*demographic;
race_ethnicity_n = race_ethnicity * 1;
if education = 'NA' then education = .;										*demographic;
education_n = education * 1;
if native_language = 'NA' then native_language = .;							*demographic;
native_language_n = native_language * 1;
if  domesticviolence1_d = 'NA' then domesticviolence1_d =.;
domesticviolence1_d_n = domesticviolence1_d * 1;
if  fearabuse_d = 'NA' then fearabuse_d = .;
fearabuse_d_n = fearabuse_d * 1;
if  physabuse1_d = 'NA' then physabuse1_d = .;
physabuse1_d_n = physabuse1_d * 1;
if sexabuse1 = 'NA' then sexabuse1 = .;
sexabuse1_n = sexabuse1 * 1;
if suicide = 'NA' then suicide = .;
suicide_n = suicide * 1;
drop var1;
run;

						*only RACE_ETHNICITY, EDUCATION, and NATIVE_LANGUAGE demographic variables needed to be changed to numerics;
							*age, birth_year, gender, handedness and parentaled imported as numerics;

*2 procedures to verify variables were re-coded accurately, proc freq (table matching or crosstable -diagonal matching) or proc print; 
			*---------------------------------------*
			  		RE-CODING VERIFICATION
		  				xAGE -> xAGE_n
				top table should match bottom		
			*---------------------------------------*;

proc freq data= rmet;
tables parentdeathAGE parentdeathAGE_n; run;
proc freq data= rmet;
tables parentdivorceAGE parentdivorceAGE_n; run;
proc freq data= rmet;
tables fosterAGE fosterAGE_n; run;
proc freq data= rmet;
tables institutionAGE institutionAGE_n; run;
proc freq data= rmet;
tables suicideAGE suicideAGE_n; run;
proc freq data= rmet;
tables prisonAGE prisonAGE_n; run;
proc freq data= rmet;
tables verbalAGE1 verbalAGE_n; run;
proc freq data= rmet;
tables fearabuseAGE1 fearabuseAGE_n; run;
proc freq data= rmet;
tables physabuse1AGE1 physabuse1AGE_n; run;
proc freq data= rmet;
tables physabuse2AGE1 physabuse2AGE_n; run;
proc freq data= rmet;
tables physinjuryAGE1 physinjuryAGE_n; run;
proc freq data= rmet;
tables sexabuse1AGE1 sexabuse1AGE_n; run;
proc freq data= rmet;
tables sexabuse2AGE1 sexabuse2AGE_n; run;
proc freq data= rmet;
tables domesticviolence1AGE1 domesticviolence1AGE_n; run;
proc freq data= rmet;
tables domesticviolence2AGE1 domesticviolence2AGE_n; run;
								*/ demographics;
proc freq data= rmet;
tables race_ethnicity race_ethnicity_n; run;
proc freq data= rmet;
tables education education_n; run;
proc freq data= rmet;
tables native_language native_language_n; run; 

			*---------------------------------------*
			  		RE-CODING VERIFICATION
			  			xAGE -> xAGE_n
			col. on left should match col. on right 
			*---------------------------------------*;

proc print obs="50" data = rmet;
var parentdeathAGE parentdeathAGE_n;
var parentdivorceAGE parentdivorceAGE_n;
var fosterAGE fosterAGE_n;
var institutionAGE institutionAGE_n;
var suicideAGE suicideAGE_n;
var prisonAGE prisonAGE_n;
var verbalAGE1 verbalAGE_n;
var fearabuseAGE1 fearabuseAGE_n;
var physabuse1AGE1 physabuse1AGE_n;
var physabuse2AGE1 physabuse2AGE_n;
var physinjuryAGE1 physinjuryAGE_n;
var sexabuse1AGE1 sexabuse1AGE_n;
var sexabuse2AGE1 sexabuse2AGE_n;
var domesticviolence1AGE1 domesticviolence1AGE_n;
var domesticviolence2AGE1 domesticviolence2AGE_n;
var race_ethnicity race_ethnicity_n;
var education education_n;
var native_language native_language_n;
var domesticviolence1_d domesticviolence1_d_n;
var fearabuse_d fearabuse_d_n;
var physabuse1_d physabuse1_d_n;
var sexabuse1 sexabuse1_n;
var suicide suicide_n;
run;

			*---------------------------------------*
			  		RE-CODING VERIFICATION	
			  			xAGE -> xAGE_n
				only diagonals should have info		
			*---------------------------------------*;

*compares original ages to recoded ages*; 
proc freq data=rmet;
tables parentdeathAGE * parentdeathage_n; run;
proc freq data=rmet;
tables parentdivorceAGE * parentdivorceAGE_n; run;
proc freq data=rmet;
tables fosterAGE * fosterAGE_n; run;
proc freq data=rmet;
tables institutionAGE * institutionAGE_n; run;
proc freq data=rmet;
tables suicideAGE * suicideAGE_n; run;
proc freq data=rmet;
tables prisonAGE * prisonAGE_n; run;
proc freq data=rmet;
tables verbalAGE1 * verbalAGE_n; run;
proc freq data=rmet;
tables fearabuseAGE1 * fearabuseAGE_n; run;
proc freq data=rmet;
tables physabuse1AGE1 * physabuse1AGE_n; run;
proc freq data=rmet;
tables physabuse2AGE1 * physabuse2AGE_n; run;
proc freq data=rmet;
tables physinjuryAGE1 * physinjuryAGE_n; run;
proc freq data=rmet;
tables sexabuse1AGE1 * sexabuse1AGE_n; run;
proc freq data=rmet;
tables sexabuse2AGE1 * sexabuse2AGE_n; run;
proc freq data=rmet;
tables domesticviolence1AGE1 * domesticviolence1AGE_n; run;
proc freq data=rmet;
tables domesticviolence2AGE1 * domesticviolence2AGE_n; run;
proc freq data=rmet;
tables race_ethnicity * race_ethnicity_n; run;
proc freq data=rmet;
tables education * education_n; run;
proc freq data=rmet;
tables native_language * native_language_n; run;
proc freq data=rmet;
tables domesticviolence1_d * domesticviolence1_d_n; run;
proc freq data=rmet;
tables fearabuse_d * fearabuse_d_n; run;
proc freq data=rmet;
tables physabuse1_d * physabuse1_d_n; run;
proc freq data=rmet;
tables suicide * suicide_n; run;
							
							*---------------------------------------*
							| CHECKING DISTRIBUTION AND SAMPLE SIZE	|
							| sort by exposed/unexposed and ages	|
							*---------------------------------------*;
*ensure those not exposed or missing adversity response do NOT have an associated age - age should be missing
	AND
		re-codes responses which stated they were not exposed but stated an age - these responses are recoded as missing;

proc sort data=rmet; by parentdeath; run;
data rmet;
set rmet;
if (parentdeath = 0) and (parentdeathAGE_n = 0) then parentdeathAGE_n = .; 
else if (parentdeath = 0) and ((parentdeathage_n = 1) or (parentdeathage_n = 10) or (parentdeathage_n = 18)) then parentdeathAGE_n = .;
else if (parentdeath = .) and (parentdeathage_n>=1) then parentdeathAGE_n = .; run;
proc freq data=rmet;
by parentdeath;
tables parentdeath parentdeathAGE_n; run;

proc sort data=rmet; by parentdivorce; run;
data rmet;
set rmet;
if (parentdivorce = 0) and (parentdivorceage_n = 0) then parentdivorceage_n = .; 
else if (parentdivorce = 0) and (parentdivorceage_n > 1) then parentdivorceage_n = .;
else if parentdivorce = . then parentdivorceage_n =.; run;
proc freq data=rmet;
by parentdivorce;
tables parentdivorce parentdivorceAGE_n; run;

proc sort data=rmet; by foster; run;
data rmet;
set rmet;
if (foster = 0) and (fosterage_n = 0) then fosterage_n = .;
else if (foster = 0) and (fosterage_n > 7) then fosterage_n = .;
else if foster = . then fosterage_n = .; run;
proc freq data=rmet;
by foster;
tables foster fosterAGE_n; run;

proc sort data=rmet; by institution; run;
data rmet;
set rmet;
if (institution = 0) and (institutionAGE_n = 0) then institutionAGE_n = .;
else if (institution = 0) and (institutionAGE_n >14) then institutionAGE_n = .;
else if institution = . then institutionAGE_n = .; run;
proc freq data=rmet;
by institution;
tables institution institutionAGE_n; run;

proc sort data=rmet; by suicide_n; run;
data rmet;
set rmet;
if (suicide_n = 0) and (suicideage_n = 0) then suicideage_n = .;
else if (suicide_n = 0) and (suicideage_n > 3) then suicideage_n = .; 
else if suicide_n = . then suicideage_n = .; run;
proc freq data=rmet;
by suicide_n;
tables suicide_n suicideage_n; run;

proc sort data=rmet; by prison; run;
data rmet;
set rmet;
if (prison = 0) and (prisonage_n = 0) then prisonage_n = .;
else if (prison = 0) and (prisonage_n > 4) then prisonage_n = .;
else if prison = . then prisonage_n=.; run;
proc freq data=rmet;
by prison;
tables prison prisonage_n; run;

proc sort data=rmet; by verbalabuse_d; run;
data rmet;
set rmet;
if (verbalabuse_d = 0) and (verbalage_n = 0) then verbalage_n = .;
else if (verbalabuse_d = 0) and (verbalage_n > 4) or (verbalage_n =1) then verbalage_n = .; 
else if verbalabuse_d = . then verbalage_n = .;  run;
proc freq data=rmet;
by verbalabuse_d;
tables verbalabuse_d verbalage_n; run;

proc sort data=rmet; by fearabuse_d_n; run;
data rmet;
set rmet;
if (fearabuse_d_n = 0) and (fearabuseage_n = 0) then fearabuseage_n = .;
else if (fearabuse_d_n = 0) and (fearabuseage_n > 4) then fearabuseage_n = .; 
else if fearabuse_d_n = . then fearabuseage_n = .; run;
proc freq data=rmet;
by fearabuse_d_n;
tables fearabuse_d_n fearabuseage_n; run;

proc sort data=rmet; by physabuse1_d_n; run;
data rmet;
set rmet;
if (physabuse1_d_n = 0) and (physabuse1age_n = 0) then physabuse1age_n = .;
else if (physabuse1_d_n = 0) and (physabuse1age_n > 5) then physabuse1age_n = .;
else if physabuse1_d_n = . then physabuse1age_n = .; run;
proc freq data=rmet;
by physabuse1_d_n;
tables physabuse1_d_n physabuse1age_n; run;

proc sort data=rmet; by physabuse2_d; run;
data rmet;
set rmet;
if (physabuse2_d = 0) and (physabuse2age_n = 0) then physabuse2age_n = .;
else if (physabuse2_d = 0) and (physabuse2age_n > 2) then physabuse2age_n = .; 
else if physabuse2_d = . then physabuse2age_n = .; run;
proc freq data=rmet;
by physabuse2_d;
tables physabuse2_d physabuse2age_n; run;

proc sort data=rmet; by physinjury_d; run;
data rmet;
set rmet;
if (physinjury_d = 0) and (physinjuryAGE_n = 0) then physinjuryAGE_n = .;
else if (physinjury_d = 0) and (physinjuryAGE_n > 13) then physinjuryAGE_n = .;
else if physinjury_d = . then physinjuryAGE_n = .; run;
proc freq data=rmet;
by physinjury_d;
tables physinjury_d physinjuryAGE_n; run;

proc sort data=rmet; by sexabuse1_n; run;
data rmet;
set rmet;
if (sexabuse1_n = 0) and (sexabuse1AGE_n = 0) then sexabuse1AGE_n = .;
else if (sexabuse1_n = 0) and (sexabuse1AGE_n > 3) then sexabuse1AGE_n = .;
else if sexabuse1_n = . then sexabuse1AGE_n = .;  run;
proc freq data=rmet;
by sexabuse1_n;
tables sexabuse1_n sexabuse1AGE_n; run;

proc sort data=rmet; by sexabuse2; run;
data rmet;
set rmet;
if (sexabuse2 = 0) and (sexabuse2AGE_n = 0) then sexabuse2AGE_n = .;
else if (sexabuse2 = 0) and (sexabuse2AGE_n > 6) then sexabuse2AGE_n = .;
else if sexabuse2 = . then sexabuse2AGE_n = .;  run;
proc freq data=rmet;
by sexabuse2;
tables sexabuse2 sexabuse2AGE_n; run;

proc sort data=rmet; by domesticviolence1_d_n; run;
data rmet;
set rmet;
if (domesticviolence1_d_n = 0) and (domesticviolence1age_n = 0) then domesticviolence1age_n = .;
else if (domesticviolence1_d_n = 0) and (domesticviolence1age_n > 2) then domesticviolence1age_n = .
else if domesticviolence1_d_n = . then domesticviolence1age_n = .; run;
proc freq data=rmet;
by domesticviolence1_d_n;
tables domesticviolence1_d_n domesticviolence1age_n; run;

proc sort data=rmet; by domesticviolence2_d; run;
data rmet;
set rmet;
if (domesticviolence2_d = 0) and (domesticviolence2age_n = 0) then domesticviolence2age_n = .;
else if domesticviolence2_d = . then domesticviolence2age_n = .; run;
proc freq data=rmet;
by domesticviolence2_d;
tables domesticviolence2_d domesticviolence2age_n; run;

* all RAW age var end with AGE_n 
	- script below drops any character variables;
* original imported df with all vars still exists - rmet
	rmet_select = new df with fewer variables and easier to work with (drops age group variables which were created in R);
data rmet_select;
set rmet;
drop domesticviolence1_d domesticviolence1AGE1 domesticviolence1AGE1_AG1 domesticviolence1AGE1_AG2 domesticviolence1AGE1_AG3 domesticviolence1AGE1_AG4
	  domesticviolence2AGE1 domesticviolence2AGE1_AG1 domesticviolence2AGE1_AG2 domesticviolence2AGE1_AG3 domesticviolence2AGE1_AG4
		fearabuse_d fearabuseAGE1_AG1 fearabuseAGE1_AG2 fearabuseAGE1_AG3 fearabuseAGE1_AG4  fearabuseAGE1
		 foster_AG1 foster_AG2 foster_AG3 foster_AG4 fosterAGE 
		 institution_AG1 institution_AG2 institution_AG3 institution_AG4 institutionAGE
		  parentdeath_AG1 parentdeath_AG2 parentdeath_AG3 parentdeath_AG4 parentdeathAGE
		   parentdivorce_AG1 parentdivorce_AG2 parentdivorce_AG3 parentdivorce_AG4 parentdivorceAGE
		    physabuse1_d physabuse1AGE1 physabuse1AGE1_AG1 physabuse1AGE1_AG2 physabuse1AGE1_AG3 physabuse1AGE1_AG4
		     physabuse2AGE1 physabuse2AGE1_AG1 physabuse2AGE1_AG2 physabuse2AGE1_AG3 physabuse2AGE1_AG4
		      physinjuryAGE1 physinjuryAGE1_AG1 physinjuryAGE1_AG2 physinjuryAGE1_AG3 physinjuryAGE1_AG4
		       prison_AG1 prison_AG2 prison_AG3 prison_AG4 prisonAGE
		        sexabuse1 sexabuse1AGE1 sexabuse1AGE1_AG1 sexabuse1AGE1_AG2 sexabuse1AGE1_AG3 sexabuse1AGE1_AG4
		         sexabuse2AGE1 sexabuse2AGE1_AG1 sexabuse2AGE1_AG2 sexabuse2AGE1_AG3 sexabuse2AGE1_AG4 
		         suicide suicide_AG1 suicide_AG2 suicide_AG3 suicide_AG4 suicideAGE 
		         	verbalAGE1 verbalAGE1_AG1 verbalAGE1_AG2 verbalAGE1_AG3 verbalAGE1_AG4;
run;

							*---------------------------------------*
							| 	 DEMOGRAPHIC MISSING PATTERNS		|
							| 										|
							*---------------------------------------*;

* count missing demographics;
	*outputs total number of each demographic var response and number of missing response;
proc means data=rmet_select nolabel N NMISS;
	var ID gender handedness education_n parentaled native_language_n race_ethnicity_n; 
	title "Missing Demographic Counts";
run;

* demographic missing data pattern;
	* outputs freq, percent and group means for vars in demographics;
		* 7 DEMOGRAPHIC VARIABLES;
ods select MissPattern;
proc mi data = rmet_select nimpute=0;
	var ID age gender handedness education_n parentaled native_language_n race_ethnicity_n;
	title "Missing Demographic Patterns";
run;

							*---------------------------------------*
							| 		 ADVERSITY EXPOSURE				|
							| 			MISSING PATTERNS			|
							*---------------------------------------*;
*counts missing exposed/unexposed for each adveristy
	NMISS = adversities which do not have 1 OR 0 --> participant responded with "Decline to Answer" or "I Don't Know"; 
proc means data=rmet_select nolabel N NMISS;
	var parentdeath parentdivorce foster institution suicide_n prison verbalabuse_d 
		fearabuse_d_n physabuse1_d_n physabuse2_d physinjury_d sexabuse1_n sexabuse2 domesticviolence1_d_n domesticviolence2_d;
	title  "Adversity Exposure Counts";
run;

* PROC MI not executed on exposed/unexposed vars - due to all values being either 1 or 0 - all variables have values

							*---------------------------------------*
							| 			 ADVERSITY AGE	 			|
							| 			MISSING PATTERNS			|
							*---------------------------------------*;

* count missing values in age dataset;
	* outputs each vars number of responses and missing responses
		N = number of raw age responses;
proc means data=rmet_select nolabel N NMISS;
	var parentdeathAGE_n parentdivorceAGE_n fosterAGE_n institutionAGE_n suicideAGE_n 
		prisonAGE_n verbalAGE_n fearabuseAGE_n physabuse1AGE_n physabuse2AGE_n physinjuryAGE_n 
		sexabuse1AGE_n sexabuse2AGE_n domesticviolence1AGE_n domesticviolence2AGE_n;
	title "Missing Age Counts";
run;
* missing data pattern for age dataset;
	* outputs freq, percent and group means for vars in demographics;
		* LESS USEFUL SINCE NOT NECESSARY SINCE WE ARE NOT TRYING TO HAVE COMPLETE CASES FOR EACH ADVERSITY*;
ods select MissPattern;
proc mi data=rmet_select nimpute=0;
	var parentdeathAGE_n parentdivorceAGE_n fosterAGE_n institutionAGE_n suicideAGE_n 
		prisonAGE_n verbalAGE_n fearabuseAGE_n physabuse1AGE_n physabuse2AGE_n physinjuryAGE_n 
		sexabuse1AGE_n sexabuse2AGE_n domesticviolence1AGE_n domesticviolence2AGE_n;
	title "Missing Age Patterns";
run;

							*---------------------------------------*
							| CHECKING CORRELATION B/T SIMILAR VARS |
							|										|
							*---------------------------------------*;
	*useful to identify variables which can be combined;
proc corr data=rmet_select; 
var parentdeath parentdivorce foster institution suicide_n prison verbalabuse_d fearabuse_d_n physabuse1_d_n physabuse2_d physinjury_d sexabuse1_n sexabuse2 domesticviolence1_d_n domesticviolence2_d;
title 'Correlated Adversity Variables';
run;

							*---------------------------------------*
							| 		CREATING (8) NEW CORRELATED	 	|
							|				VARIABLES				|
							*---------------------------------------*;

*institution & foster - child_care;
data rmet_select; set rmet_select;
child_care = .;
if institution = 1 or foster = 1 then child_care = 1;
	else if institution = 0 and foster = 0 then child_care = 0;
child_care_age_new = .;
child_care_age_new = min(of institutionage_n fosterage_n);
run;
*physabuse1 & physabuse2 & physinjury- PHYSABUSE_new; 
data rmet_select; set rmet_select;
physabuse_new = .;
if physabuse1_d_n = 1 or physabuse2_d = 1 or physinjury_d = 1 then physabuse_new = 1;
	else if physabuse1_d_n = 0 and physabuse2_d = 0 or physinjury_d = 0 then physabuse_new = 0;
physabuse_age_new =.;
physabuse_age_new = min(of physabuse1AGE_n physabuse2age_n physinjuryAGE_n);
run;
*sexabuse1 & sexabuse2  - SEXABUSE_new;
data rmet_select; set rmet_select;
sexabuse_new = .;
if sexabuse1_n = 1 or sexabuse2 = 1 then sexabuse_new = 1;
	else if sexabuse1_n = 0 and sexabuse2 = 0 then sexabuse_new = 0;
sexabuse_age_new = .;
sexabuse_age_new = min(of sexabuse1age_n sexabuse2age_n);
run; 
*domesticviolence1 & domesticviolence2 - DOMESTICVIOLENCE_new;
data rmet_select; set rmet_select;
domesticviolence_new = .;
if domesticviolence1_d_n = 1 or domesticviolence2_d = 1 then domesticviolence_new = 1;
	else if domesticviolence1_d_n = 0 and domesticviolence2_d = 0 then domesticviolence_new = 0;
domesticviolence_age_new = .;
domesticviolence_age_new = min(of domesticviolence1age_n domesticviolence2age_n);
run; 

			*---------------------------------------*
			 		RE-CODING VERIFICATION 		 	
					x1_d and x2_d -> x_new
				x1AGE_n and x2AGE_n -> x_age_new
			*---------------------------------------*;

*CHILD_CARE;
proc print data=rmet_select;
var institution foster child_care /* left are exposure*/  institutionAGE_n fosterAGE_n child_care_age_new;
title 'Combined Institution and Foster Variable';
run;
* PHYSABUSE_new - phys1/2 and physinjury;
proc print data=rmet_select;
var physabuse1_d_n physabuse2_d physinjury_d physabuse_new /* left are exposure*/  physabuse1age_n physabuse2age_n physinjuryAGE_n physabuse_age_new;
title 'Combined Physical Abuse Variable';
run;
*SEXABUSE_new - sexabuse1 and sexabuse2;
proc print data=rmet_select;
var sexabuse1_n sexabuse2 sexabuse_new /* left are exposure*/ sexabuse1age_n sexabuse2age_n sexabuse_age_new;
title 'Combined Sex Abuse Variable';
run;
*DOMESTICVIOLENCE_new - domesticviolence1 and domesticviolence2;
proc print data=rmet_select;
var domesticviolence1_d_n domesticviolence2_d domesticviolence_new /* left are exposure*/ domesticviolence1age_n domesticviolence2age_n domesticviolence_age_new;
title ' Combined Domestic Violence Variable';
run;

			*---------------------------------------*
			 		RE-CODING VERIFICATION 		 	
					x1_d and x2_d -> x_new
				x1AGE_n and x2AGE_n -> x_age_new
			*---------------------------------------*;

proc sort data=rmet_select; by child_care; run;
proc freq data=rmet_select;
by child_care;
tables (institution foster)* child_care/crosslist norow nocol;
title 'child_care verification';
run;
proc sort data=rmet_select; by physabuse_new; run;
proc freq data=rmet_select;
by physabuse_new;
tables (physabuse1_d_n physabuse2_d physinjury_d)*physabuse_new/crosslist norow nocol;
title 'physabuse_new verification';
run;
proc sort data=rmet_select; by sexabuse_new; run;
proc freq data=rmet_select;
by sexabuse_new;
tables (sexabuse1_n sexabuse2)*sexabuse_new/crosslist norow nocol;
title 'sexabuse_new verification';
run;
proc sort data=rmet_select; by domesticviolence_new; run;
proc freq data=rmet_select;
by domesticviolence_new;
tables (domesticviolence1_d_n domesticviolence2_d)*domesticviolence_new/crosslist norow nocol;
title 'domesticviolence_new verification';
run;

							*---------------------------------------*
							|RE-CHECKING CORRELATION B/T SIMILAR VAR|
							|										|
							*---------------------------------------*;
* 10 TOTAL VARS - foster/institution} child_care, physabuse1/physabuse2/physinjury} physabuse_new, sexabuse1/sexabuse2} sexabuse_new, domesticviolence1/domesticviolence2} domesticviolence_new; 
proc corr data=rmet_select; 
var parentdeath parentdivorce child_care suicide_n prison verbalabuse_d fearabuse_d_n physabuse_new sexabuse_new domesticviolence_new;
title 'Correlated Adversity Variables';
run;

							*---------------------------------------*
							| 	CHECKING AGE CORR B/T COMBINED VAR  |
							|										|
							*---------------------------------------*;
proc corr data= rmet_select;
VAR physabuse1AGE_n  physabuse2AGE_n  physinjuryAGE_n  sexabuse1AGE_n sexabuse2AGE_n  domesticviolence1AGE_n domesticviolence2AGE_n;
title 'Age Correlation for Similar Adversity Variables';
run;
					*necessary to ensure correlation of exposure age for similar vars, as well as exposure correlation;
							
							*---------------------------------------*
							| 	COMPLETE DEMOGRAPHIC FLAG VAR	 	|
							|	complete demographics -> flag = 1	|
							*---------------------------------------*;
*creates dummy "flag" variable for responses which have complete demographic information;
data rmet_select; set rmet_select;
flag = .;
if (education_n ~= .) and (gender ~= .) and (native_language_n ~= .) and (parentaled ~= .) and (race_ethnicity_n ~= .) then flag = 1;
	else if (education_n =.) or (gender =.) or (native_language_n =.) or (parentaled =.) or (race_ethnicity_n =.) then flag = 0;
run;

			*---------------------------------------*
			 		RE-CODING VERIFICATION 		 	
				complete demographics -> flag = 1
			*---------------------------------------*;

proc sort data=rmet_select; by flag;run;
proc freq data=rmet_select;
by flag;
tables education_n gender native_language_n parentaled race_ethnicity_n flag/ norow nocol;
title 'Verification of Complete Demographic Flag Variable';
run;
							*flag var was recoded correctly b/c the frequency of flag = 1 is 2628.
							this matches the frequency of complete cases when PROC MI is executed;
							*flag = 0 freq + flag=1 freq should be 3815 (total sample);

							*---------------------------------------*
							| 		TIMING VARIABLE CREATION	 	|
							|				DO LOOP					|
							*---------------------------------------*;
data rmet2;
set rmet_select;
array old {10} parentdeathAGE_n parentdivorceAGE_n child_care_age_new suicideAGE_n 
	prisonAGE_n verbalAGE_n fearabuseAGE_n physabuse_age_new sexabuse_age_new domesticviolence_age_new;
array new {10} parentdeathAGE_n_r parentdivorceAGE_n_r child_care_age_new_r suicideAGE_n_r 
	prisonAGE_n_r verbalAGE_n_r fearabuseAGE_n_r physabuse_age_new_r sexabuse_age_new_r domesticviolence_age_new_r;
do i = 1 to 10;
if old{i} = . then new{i} = .;
    else if  0 le old{i} le 5 then new{i} = 1;
    else if 6 le old {i} le 10 then new{i} = 2;
    else if 11 le old{i} le 17 then new{i} = 3;
    else if old{i} ge 18 then new{i} = 4;
end;
drop i;
run;

			*---------------------------------------*
			 		RE-CODING VERIFICATION 		 	
			timing var -> 0-5 = 1, 6-10 = 2 11-17 = 3 18+ = 4
			*---------------------------------------*;

proc freq data=rmet2;
tables parentdeathAGE_n*parentdeathAGE_n_r 
		parentdivorceAGE_n*parentdivorceAGE_n_r
		child_care_age_new*child_care_age_new_r
		suicideAGE_n*suicideAGE_n_r
		prisonAGE_n*prisonAGE_n_r
		verbalAGE_n*verbalAGE_n_r
		fearabuseAGE_n*fearabuseAGE_n_r
		physabuse_age_new*physabuse_age_new_r
		physinjuryAGE_n*physinjuryAGE_n_r
		sexabuse_age_new*sexabuse_age_new_r
		domesticviolence_age_new*domesticviolence_age_new_r/list missing; 
run;

*create subset of rmet2 of only completed demographics;
data complete_cases;
   set rmet2;
   where flag = 1;
run;

							*---------------------------------------*
							| 			PROC MI VARIABLE	 		|
							|	identified new age timing vars		|
							*---------------------------------------*;

	*MISSING PATTERN OF EXPOSURES ONLY FOR COMPLETE DEMOGRAPHIC CASES (2628);
ods select MissPattern;
proc mi data=complete_cases nimpute=0;
	var parentdeath parentdivorce prison verbalabuse_d suicide_n 
		child_care physabuse_new sexabuse_new domesticviolence_new;
	title "Missing exposure Patterns";
run;

* missing ages are coded as "." ;
							*age group frequency distribution for all adversities;
proc freq data = rmet2;
tables parentdeathAGE_n_r parentdivorceAGE_n_r child_care_age_new_r suicideAGE_n_r 
	prisonAGE_n_r verbalAGE_n_r fearabuseAGE_n_r physabuse_age_new_r 
	sexabuse_age_new_r domesticviolence_age_new_r/norow nocol;
title "Age Group Frequency Across Adversities";run;

	* AGE GROUP FREQUENCY DISTRIBUTION FOR ONLY COMPLETE DEMOGRAPHIC CASES (2628);
proc freq data = complete_cases;
tables parentdeathAGE_n_r parentdivorceAGE_n_r child_care_age_new_r suicideAGE_n_r 
	prisonAGE_n_r verbalAGE_n_r fearabuseAGE_n_r physabuse_age_new_r 
	sexabuse_age_new_r domesticviolence_age_new_r/norow nocol;
title "Age Group Frequency Across Adversities for Completed Demographics";run;


* exports SAS data to files within selected path ;
*rmet2 - contains timing var, adversity exposure, adversity age - only numerics----- will need to filter for complete demographic cases;
proc export data=work.rmet2 DBMS=csv
   outfile="/folders/myshortcuts/myfolder/rmet2_SAS_export.csv";
run;


* 	RMET = orginal dataframe - character and numeric variables, if exposure=0, but age indicated --> age==. - missing-
	RMET_select = orginal data, minus character variables (droped age groups created in R), NO combined variables, only numerics (3815 obs.)
	RMET2 = data with combined vars, ages and exposures, only numerics [only differs from complete_cases by rows filtered out] (3815 obs.)
	complete_cases = data filtered by flag (complete demographic), combined variables, ages, exposures (2628 obs.);



*-------------------------------------------------------------------------------------------------------;
*-------------------------------------------------------------------------------------------------------;
*-------------------------------------------------------------------------------------------------------;
*-------------------------------------------------------------------------------------------------------;
*misc. test code below;
*-------------------------------------------------------------------------------------------------------;
* age distribution of all variables;
proc freq data=rmet;
tables parentdeathAGE_n parentdivorceAGE_n fosterAGE_n institutionAGE_n suicideAGE_n
		prisonAGE_n verbalAGE_n fearabuseAGE_n physabuse_age_new physinjuryAGE_n 
		sexabuse_age_new domesticviolence_age_new;run;

*creates resulting table counted by individual age values;
proc freq data = rmet;
tables physabuse_age_new;run;

*creates new datatable counted by individual age values;
proc freq data = rmet noprint;
tables sexabuse_age_new/norow nocol out = sexagedistribution;run;

proc freq data=rmet noprint;
tables domesticviolence_age_new/norow nocol out=domesticviolenceagedistribution;run;

***continue from here ensure everything copied correctly;
proc freq data = rmet;
by physabuse2_d physabuse1_d ;
tables physabuse1_d physabuse2_d phys_new;
title "new physical abuse variables";
run;

proc means data = rmet; 
var parentdeath parentdivorce foster institution suicide prison verbalabuse_d fearabuse_d
physabuse2_d physabuse1_d physinjury_d sexabuse1 sexabuse2 domesticviolence1_d domesticviolence2_d;
run;						

*generates age frequency distribution for parentdeath as histogram;
PROC SQL;
create table parentdeathage as
SELECT parentdeathage_n FROM rmet;
RUN;
proc SGPLOT data=work.parentdeathage;
vbar parentdeathage_n ;
title 'age distribution';
run;
quit;
