* ****************************************** *
* PROCESSING TOMATO Southern blight grafting *
* Experiment 3: Graft - Greenhouse 		     *
* ****************************************** *;

* NOTE: adapted from Karen Xu's consulting analysis;
 
options ls=90 nonumber formdlim=' ' pagesize=52 center;

* ********* *
* A. Import
* ********* *;

** import data;
proc import 
		datafile="C:\Users\Alex Putman\GoogleDrive\_VSPLab_UCR\08_Tomato_rolfsii\SAS_graft\2_data\graft-rolfsii_3_graft-greenhouse_severity_final.csv"
		dbms=dlm replace out=graft_gh;
	delimiter=",";
	getnames=YES;
run;

** sort dataset;
proc sort data=graft_gh;
	by exp_rep cultivar graft inoculum block days_after_plant;
run;

** convert columns to character; * -L makes anything left-justified within 2 character space;
data graft_gh;
	set graft_gh;
	cult_new = put(cultivar, 4.);
	exprep_new = put(exp_rep, 4.);
	inoc_new = put(inoculum, 2. -L);
	drop cultivar exp_rep inoculum;
	rename cult_new=cultivar exprep_new=exp_rep inoc_new=inoculum;
run;
	
** check dataset;
*	proc print data=graft_gh;
*		title 'graft greenhouse full review';
*	run;


* ************************************************ *
* B. Analyze - Main Effects - No Repeated Measures *
* ************************************************ *;
	
*** Step 1 ***;
	** OBJ: analyze main effects;
	** see code in Appendix B below;
	** RESULTS: P > F = 1 or near 1 for all effects despite obvious effect of inoculum level
	** CONCLUSION: remove rating dates with little data

*** Step 2 ***;
	** OBJ: analyze main effects: with rating dates removed, day_ excluded from interaction;
	** see code in Appendix B below;
	** RESULTS: P > F = 1 or near 1 for all effects despite obvious effect of inoculum level 
		* various models tried, see Appendix for details
	** CONCLUSION: try removing inoculum = 0 (see Appendix for details)

*** Step 3 ***;
	** OBJ: analyze main effects with: rating dates removed, inoculum = 0 removed;
		* possible that all 0s in inoculum = 0 causing problems in full dataset;
		
	** create dataset: 2018 - remove first 3 dates, 2019 - remove first 2 dates;
	data graft_gh_d2;
		set graft_gh;
		if inoculum in ('0') then delete;
		if days_after_plant in (18,99,105) AND exp_rep='2018' then delete;
		if days_after_plant in (8,14) AND exp_rep='2019' then delete;
	run;

	** check dataset;
*	proc print data=graft_gh_d2;
*		title 'graft greenhouse 2018 d2 review';
*	run;

	proc glimmix data=graft_gh_d2 method=laplace;
		class exp_rep cultivar graft block days_after_plant;
		by exp_rep;
		model disease_severity = cultivar|graft days_after_plant / dist=multinomial link=cumlogit;
		random intercept / subject=block;
		covtest / wald;
		title 'graft greenhouse B step 3 d2 - main effects';
	run;
	
	** RESULTS: most sensical results
		* no red flags in output tables
		* however, differences between cultivars in 2018 not detected despite being obvious in graph; 
	** CONCLUSION: proceed with mean separation
	
* *************************************************** *
* C. Analyze - Mean Separation - No Repeated Measures *
* *************************************************** *;
	
*** Step 1 ***;
	** OBJ: separate means with: rating dates removed, inoculum = 0 removed;
	
	proc glimmix data=graft_gh_d2 method=laplace;
		class exp_rep cultivar graft block days_after_plant;
		by exp_rep;
		model disease_severity = cultivar|graft days_after_plant / dist=multinomial link=cumlogit oddsratio(diff=all);
		random intercept / subject=block;
		covtest / wald;
		title 'graft greenhouse C step 1 d2 - mean separation';
	run;

	** RESULTS: graft odds ratios make sense in light of graph
		* however, nonsensical estimates for standard graft in 2019: because all pots = 0 all dates

* ********** *
* APPENDIX B *
* ********** *;
*** Step 1 ***;
*	** modified from original analysis by Karen;
*		* retain only needed class variables
*		* examine all interactions;
*		* random statement: subject changed to block;
*	proc glimmix data=graft_gh method=laplace;
*		class exp_rep cultivar graft inoculum block days_after_plant;
*		by exp_rep;
*		model disease_severity = cultivar|graft|inoculum|days_after_plant / dist=multinomial link=cumlogit;
*		random intercept / subject=block;
*		covtest / wald;
*		title 'graft greenhouse B (modified analysis) - step 1 main effects';
*	run;

*** Step 2 ***;
*	** remove rating dates: 2018, all but last 4 rating dates - 2019, first two rating dates;
*	data graft_gh_d1;
*		set graft_gh;
*		if days_after_plant notin (120,127,133,154) AND exp_rep='2018' then delete;
*		if days_after_plant in (8,14) AND exp_rep='2019' then delete;
*	run;
*
*	** check dataset;
*	proc print data=graft_gh_d1;
*		title 'graft greenhouse d1 review';
*	run;
*
*	** analyze;
*	proc glimmix data=graft_gh_d1 method=laplace;
*		class exp_rep cultivar graft inoculum block days_after_plant;
*		by exp_rep;
*		model disease_severity = cultivar|graft|inoculum days_after_plant / dist=multinomial link=cumlogit;
*		covtest / wald;
*		random intercept / subject=block;
*		title 'graft greenhouse B step 2 d1 - main effects';
*	run;
*
*		** ATTEMPTS: various models tried
*			* days_after_plant as interaction term or separate main effect term
*			* last rating date or last 3 rating dates
*			* with or without random block effect
*		** OUTCOME: 
*			* most models result in non-sensical results
*				-block covariance estimates
*				-F Value = infinity
*				-slicing cultivar*graft*inoculum shows strongly significant differences among graft within inoculum=0
*			* removing block as random effect produced sensical results
*				* however no main or interaction effects were significant
*		** CONCLUSION: no differences can be detected
*			* unclear why difference between cultivars was not detected
*			* low disease probably contributed




*********************
*** previous analyses;
*
*PROC IMPORT OUT=graft DATAFILE= "C:\Xu Huaying\Collaboratory\Natalie Solares\Grafting trial\graft-rolfsii_exp-2_DRating_10_edit (1).xlsx" 
*            DBMS=xlsx REPLACE;
*     SHEET="SAS"; 
*     GETNAMES=YES;
*RUN;
*
*proc print data=graft;
*run;
*
*
*proc glimmix data=graft method=laplace;
*	class date exp_unt trt plot block cultivar inoculum;
**	model disease_score = block trt|cultivar|inoculum cultivar|inoculum date /dist=multinomial 
*                      link=cumlogit solution;
**	model disease_score = block trt|cultivar|inoculum trt|cultivar|date trt|inoculum|date cultivar|inoculum|date/dist=multinomial 
*                      link=cumlogit;
**	model disease_score = block trt|cultivar trt|inoculum trt|date cultivar|inoculum cultivar|date inoculum|date/dist=multinomial 
*                      link=cumlogit;
**	model disease_score = block trt cultivar   inoculum  date/dist=multinomial 
*                      link=cumlogit ODDSRATIO (DIFF=ALL);
*	model disease_score = cultivar   inoculum  date/dist=multinomial 
*                      link=cumlogit ODDSRATIO (DIFF=ALL);
*	random intercept / subject=exp_unt ;
*	covtest  /wald;
*run;
*
*
*
*/* Generate data file for survial analysis */
*proc sort data=graft;
*	by exp_unt day;
*run;
*
*proc transpose data=graft out=graft_wide prefix=Day;
*	by exp_unt trt plot block cultivar graft inoculum;
*	id day;
*	var disease_score;
*run;
*
*PROC EXPORT DATA=graft_wide OUTFILE = "C:\Xu Huaying\Collaboratory\Natalie Solares\Grafting trial\graft_wide.XLSX"
*  DBMS = xlsx REPLACE;
*  SHEET = "graft";
*RUN;
*
*
*
*PROC IMPORT OUT=graft_wide DATAFILE= "C:\Xu Huaying\Collaboratory\Natalie Solares\Grafting trial\graft_wide_edit.xlsx" 
*            DBMS=xlsx REPLACE;
*     SHEET="SAS"; 
*     GETNAMES=YES;
*RUN;
*
*proc print;
*run;
*
*proc freq;
*	table trt*symptom inoculum*symptom trt*death  inoculum*death graft*symptom graft*death cultivar*symptom cultivar*death;
*run;
*
*proc freq;
*	table block*symptom ;
*run;
*
*
*proc freq data=graft_wide;
*	table inoculum*death /fisher;
*run;
*
*
*proc phreg data=graft_wide plot(overlay)=survival;
*	class trt cultivar inoculum /PARAM=GLM;
*   model Day_to_sympton*c_sympton(0)= block trt|cultivar|inoculum;
**   model Day_to_sympton*c_sympton(0)= trt cultivar inoculum;
*   lsmeans cultivar inoculum /adjust=tukey lines;
*run;
*
*proc phreg data=graft_wide plot(overlay)=survival;
*	class trt cultivar inoculum /PARAM=GLM;
*   model Day_to_death*c_death (0)= trt cultivar ;
**   model Day_to_death*c_death (0)= trt cultivar inoculum;
**   model Day_to_sympton*c_sympton(0)= trt cultivar inoculum;
**   lsmeans cultivar inoculum /adjust=tukey lines;
*run;
*
*
