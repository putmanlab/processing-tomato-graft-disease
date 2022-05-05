* ****************************************** *
* PROCESSING TOMATO Southern blight grafting *
* Experiment 4: Grafting - Field	         *
* Data - Curly Top (CT)						 *
* ****************************************** *;

%LET results_path=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\08_Tomato_rolfsii\SAS_graft\4_results\;
%LET results_path_img=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\08_Tomato_rolfsii\SAS_graft\4_results\html_images;
%LET name_base=rolfsii_graft-field-ct_;

** log;
proc printto new log="&results_path.&name_base.A_sas-log.log"; run;

options ls=90 nonumber formdlim=' ' pagesize=52 center;

* ********* *
* A. Import *
* ********* *;

*** Step 1 ***
	** original, full dataset;

** import data;
proc import 
	datafile="C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\08_Tomato_rolfsii\SAS_graft\2_data\graft-rolfsii_4_field_incidence_by-plot_final.csv"
	dbms=dlm replace out=field;
	delimiter=",";
	getnames=YES;
run;


** convert cultivar to character;
	* create new variable - put function always returns character so no $ needed;
data field;
	set field;
	exprep_new = put(exp_rep, 4.);
	cult_new = put(cultivar, 4.);
	drop exp_rep cultivar;
	rename exprep_new=exp_rep cult_new=cultivar;
run;

** sort dataset
	* by rating for 'by group' processing in proc glimmix;
	* good practice to sort dataset in same order that variables appear in class statement
	* by days_after_plant to improve processing time (?);
proc sort data=field;
	by rating exp_rep cultivar graft block days_after_plant;
run;

** create data sets, no 2019 rating dates were removed
	* CT_T only;
	* d0: no 2018 rating dates removed
	* d7: last 2018 rating date removed due to unknown cultural/disease problem;
	* d17: first 2018 date removed due to all 0s;

	data field_d0;
		set field;
		if rating not in ('CT_T') then delete;
	run;

	data field_d7;
		set field_d0;
		if date in ('10Aug2018'd) then delete;
	run;

	data field_d17;
		set field_d0;
		if date in ('11May2018'd,'10Aug2018'd) then delete;

ods html path="&results_path" body="&name_base.A_sas-output.html";
** check dataset;
proc print data=field_d17;
	title "&name_base. full review";
run;
ods html close; * saves html;
proc printto; run; * direct log back to SAS Log window;

** check variable types
*proc contents data=field_d17;
*run;
		
	
* **************************** *
* B. Analyze - Evaluate Models *
* **************************** *;

*** Step 1 ***;
	** OBJ: get fit statistics, check for overdispersion;

	%LET name_step=B_step-1; %LET title_step=" B step 1 ";

	ods html body="&name_base.&name_step._sas-output.html" (title=&title_step) path="&results_path" gpath="&results_path_img" (url="html_images/"); ods graphics on / reset imagename="&name_base.&name_step";
	proc printto new log="&results_path.&name_base.&name_step._sas-log.log"; run;
	
	** analyze main effects;
	proc glimmix data=field_d17 plot=residualpanel method=laplace;
		class exp_rep cultivar graft block days_after_plant;
		by exp_rep;
		model rating_sum / n_plants = graft|cultivar|days_after_plant / dist=binomial link=logit htype=3;
		random intercept graft*cultivar / subject=block;
		covtest / wald;
		title "field CT &title_step. d17, all blocks - get fit statistics ";
	run;

	ods graphics off; ods html close; proc printto; run; * saves html, direct log back to SAS Log window;
	
	** RESULTS: 
		* 2018: converged, no evidence for overdispersion (0.44), sensible
		* 2019: converged, no evidence for overdispersion (0.34), sensible

	** CONCLUSION: analyze main effects


* ************************* *
* C. Analyze - Main Effects *
* ************************* *;

*** Step 1 ***;
	** OBJ: analyze main effects;

	%LET name_step=C_step-1; %LET title_step=" C step 1 ";

	ods html body="&name_base.&name_step._sas-output.html" (title=&title_step) path="&results_path" gpath="&results_path_img" (url="html_images/"); ods graphics on / reset imagename="&name_base.&name_step";
	proc printto new log="&results_path.&name_base.&name_step._sas-log.log"; run;
	
	** analyze main effects;
	proc glimmix data=field_d17 method=laplace;
		class exp_rep cultivar graft block days_after_plant;
		by exp_rep;
		model rating_sum / n_plants = graft|cultivar|days_after_plant / dist=binomial link=logit htype=3;
		random intercept graft*cultivar / subject=block;
		title "field CT &title_step. d17, all blocks - analyze main effects ";
	run;

	ods graphics off; ods html close; proc printto; run; * saves html, direct log back to SAS Log window;
	
	** RESULTS: 
		* 2018: effect of graft, cultivar
		* 2019: effect of graft
	** CONCLUSION: proceed to mean separation


* **************************** *
* D. Analyze - Mean Separation *
* **************************** *;

*** Step 1 ***;
	** OBJ: analyze main effects;

	%LET name_step=D_step-1; %LET title_step=" D step 1 ";

	ods html body="&name_base.&name_step._sas-output.html" (title=&title_step) path="&results_path" gpath="&results_path_img" (url="html_images/"); ods graphics on / reset imagename="&name_base.&name_step";
	proc printto new log="&results_path.&name_base.&name_step._sas-log.log"; run;
	
	** analyze main effects;
	proc glimmix data=field_d17 method=laplace;
		class exp_rep cultivar graft block days_after_plant;
		by exp_rep;
		model rating_sum / n_plants = graft|cultivar|days_after_plant / dist=binomial link=logit htype=3;
		random intercept graft*cultivar / subject=block;
		
		lsmeans graft / lines ilink adjust=tukey adjdfe=row;

		title "field CT &title_step. d17, all blocks - mean separation ";
	run;

	ods graphics off; ods html close; proc printto; run; * saves html, direct log back to SAS Log window;



