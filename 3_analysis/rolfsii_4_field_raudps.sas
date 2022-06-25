* ************************************************** *
* PROCESSING TOMATO Southern blight grafting 		 *
* Experiment 4: Grafting - Field 2018-2019     		 *
* Data - Southern Blight							 *
* relative Area Under Disease Progress Stairs 		 *
* ************************************************** *;

* set variables for automatic saving of output;
%LET results_path=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\08_Tomato_rolfsii\SAS_graft\4_results\;
%LET results_path_img=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\08_Tomato_rolfsii\SAS_graft\4_results\html_images;
%LET name_base=rolfsii_graft-field-sb-audps_;

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
	datafile="C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\08_Tomato_rolfsii\SAS_graft\2_data\rolfsii_4_field_audps_final.csv"
	dbms=dlm replace out=field_audps;
	delimiter=",";
	getnames=YES;
run;


** convert cultivar to character;
	* create new variable - put function always returns character so no $ needed;
data field_audps;
	set field_audps;
	exprep_new = put(exp_rep, 4.);
	cult_new = put(cultivar, 4.);
	drop exp_rep cultivar;
	rename exprep_new=exp_rep cult_new=cultivar;
run;

** sort dataset
	* by rating for 'by group' processing in proc glimmix;
	* good practice to sort dataset in same order that variables appear in class statement
	* by days_after_plant to improve processing time (?);
proc sort data=field_audps;
	by rating exp_rep cultivar graft block;
run;

** filter dataset for combined rating (_T: syptomatic + dead);
	data field_audps_t;
		set field_audps;
		if rating not in ('SB_T','CT_T','O_T','V_T') then delete;
	run;
	
ods html path="&results_path" body="&name_base.A_sas-output.html";
** check dataset;
proc print data=field_audps_t;
	title "&name_base. full review";
run;
ods html close; * saves html;
proc printto; run; * direct log back to SAS Log window;

** check variable types;
proc contents data=field_audps_t;
run;
		
		
* **************************** *
* B. Analyze - Evaluate Models *
* **************************** *;

*** Step 1 ***;
	** NOTE: 
	** OBJ: get fit statistics for random effect

	** Step 1-1 ** 
		* OBJ: evaluate with random effect;

		%LET name_step=B_step-1-1; %LET title_step=" B step 1-1 ";
	
		ods html body="&name_base.&name_step._sas-output.html" (title=&title_step) path="&results_path" gpath="&results_path_img" (url="html_images/"); ods graphics on / reset imagename="&name_base.&name_step";
		proc printto new log="&results_path.&name_base.&name_step._sas-log.log"; run;
		
		** analyze main effects;
		proc glimmix data=field_audps_t plot=residualpanel plot=pearsonpanel method=laplace;
			class rating exp_rep cultivar graft block;
			by rating exp_rep;
			model raudps = graft|cultivar / dist=beta link=logit htype=3;
			random intercept / subject=block;
			covtest / wald;
			title "field rAUDPS _T &title_step. - get fit statistics - complex/stroup random effect";
		run;
	
		ods graphics off; ods html close; proc printto; run; * saves html, direct log back to SAS Log window;
	
		* RESULTS: partially successful 
			* 2018: converged, no overdispersion
			* 2019: estimated G matrix not positive definite
			
	** Step 1-2 ** 
		* OBJ: no random effect;

		%LET name_step=B_step-1-2; %LET title_step=" B step 1-2 ";
	
		ods html body="&name_base.&name_step._sas-output.html" (title=&title_step) path="&results_path" gpath="&results_path_img" (url="html_images/"); ods graphics on / reset imagename="&name_base.&name_step";
		proc printto new log="&results_path.&name_base.&name_step._sas-log.log"; run;
		
		** analyze main effects;
		proc glimmix data=field_audps_t plot=residualpanel plot=pearsonpanel method=laplace;
			class rating exp_rep cultivar graft block;
			by rating exp_rep;
			model raudps = graft|cultivar / dist=beta link=logit htype=3;
			covtest / wald;
			title "field rAUDPS _T &title_step. - get fit statistics - simpler random effect";
		run;
	
		ods graphics off; ods html close; proc printto; run; * saves html, direct log back to SAS Log window;
	
		* NOTE: random effect (just intercept or intercept graft*cultivar resulted in estimated G matrix error for SB_T 2019;
		* RESULTS: converged
			* intercept effect was small when included
	
	** CONCLUSIONS: remove random effect


* ************************* *
* C. Analyze - Main Effects *
* ************************* *;

*** Step 1 ***;
	** OBJ: analyze main effects (remove method=laplace);
	%LET name_step=C_step-1; %LET title_step=" C step 1 ";
	
	ods html body="&name_base.&name_step._sas-output.html" (title=&title_step) path="&results_path" gpath="&results_path_img" (url="html_images/"); ods graphics on / reset imagename="&name_base.&name_step";
	proc printto new log="&results_path.&name_base.&name_step._sas-log.log"; run;
		
	** analyze main effects;
	proc glimmix data=field_audps_t;
		class rating exp_rep cultivar graft block;
		by rating exp_rep;
		model raudps = graft|cultivar / dist=beta link=logit htype=3;
		covtest / wald;
		title "field SB _T &title_step. - analyze main effects";
	run;
	
	ods graphics off; ods html close; proc printto; run; * saves html, direct log back to SAS Log window;

	** RESULTS: significant effect of graft for CT_T and SB_T in both years
	
	** CONCLUSION: separate measn


* **************************** *
* D. Analyze - Mean Separation *
* **************************** *;

*** Step 1 ***;
	** OBJ: separate means;

	%LET name_step=D_step-1; %LET title_step=" D step 1 ";
	
	ods html body="&name_base.&name_step._sas-output.html" (title=&title_step) path="&results_path" gpath="&results_path_img" (url="html_images/"); ods graphics on / reset imagename="&name_base.&name_step";
	proc printto new log="&results_path.&name_base.&name_step._sas-log.log"; run;
		
	** analyze main effects;
	proc glimmix data=field_audps_t;
		class rating exp_rep cultivar graft block;
		by rating exp_rep;
		model raudps = graft|cultivar / dist=beta link=logit htype=3;
		lsmeans graft / lines ilink adjust=tukey adjdfe=row;
		title "field SB _T &title_step. - separate means";
	run;
	
	ods graphics off; ods html close; proc printto; run; * saves html, direct log back to SAS Log window;