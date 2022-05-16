* ************************************************** *
* PROCESSING TOMATO Southern blight grafting 		 *
* Experiment 4: Grafting - Field	         		 *
* Data - relative Area Under Disease Progress Stairs *
* ************************************************** *;

* set variables for automatic saving of output;
%LET results_path=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\08_Tomato_rolfsii\SAS_graft\4_results\;
%LET results_path_img=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\08_Tomato_rolfsii\SAS_graft\4_results\html_images;
%LET name_base=rolfsii_graft-field-sb_;

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
	by rating exp_rep cultivar graft block;
run;

** filter dataset for combined rating (_T: syptomatic + dead);
	data field_t;
		set field;
		if rating not in ('SB_T','CT_T','O_T','V_T') then delete;
	run;
	
ods html path="&results_path" body="&name_base.A_sas-output.html";
** check dataset;
proc print data=field_t;
	title "&name_base. full review";
run;
ods html close; * saves html;
proc printto; run; * direct log back to SAS Log window;

** check variable types;
proc contents data=field_t;
run;
		
		
* **************************** *
* B. Analyze - Evaluate Models *
* **************************** *;

*** Step 1 ***;
	** NOTE: 
	** OBJ: get fit statistics for random effect

	** Step 1-1 ** 
		* OBJ: simple random effect;

		%LET name_step=B_step-1-1; %LET title_step=" B step 1-1 ";
	
		ods html body="&name_base.&name_step._sas-output.html" (title=&title_step) path="&results_path" gpath="&results_path_img" (url="html_images/"); ods graphics on / reset imagename="&name_base.&name_step";
		proc printto new log="&results_path.&name_base.&name_step._sas-log.log"; run;
		
		** analyze main effects;
		proc glimmix data=field_t plot=residualpanel method=laplace;
			class rating exp_rep cultivar graft block;
			by rating exp_rep;
			model raudps = graft|cultivar / dist=beta link=logit htype=3;
			random intercept / subject=block;
			covtest / wald;
			title "field rAUDPS _T &title_step. simple random effect - get fit statistics";
		run;
	
		ods graphics off; ods html close; proc printto; run; * saves html, direct log back to SAS Log window;
	
		* RESULTS: partially successful 
			* 2018: converged, no overdispersion (Pearson/DF 0.57)
			* 2019: estimated G matrix not positive definite
			
	** Step 1-2 ** 
		* OBJ: stroup random effect;

		%LET name_step=B_step-1-2; %LET title_step=" B step 1-2 ";
	
		ods html body="&name_base.&name_step._sas-output.html" (title=&title_step) path="&results_path" gpath="&results_path_img" (url="html_images/"); ods graphics on / reset imagename="&name_base.&name_step";
		proc printto new log="&results_path.&name_base.&name_step._sas-log.log"; run;
		
		** analyze main effects;
		proc glimmix data=field_t plot=residualpanel method=laplace;
			class rating exp_rep cultivar graft block;
			by rating exp_rep;
			model raudps = graft|cultivar / dist=beta link=logit htype=3;
			random intercept graft*cultivar / subject=block;
			covtest / wald;
			title "field rAUDPS _T &title_step. stroup random effect - get fit statistics";
		run;
	
		ods graphics off; ods html close; proc printto; run; * saves html, direct log back to SAS Log window;
	
		* RESULTS: partially unsuccessful 
			* 2018: converged
			* 2019: estimated G matrix not positive definite
	
	** CONCLUSIONS: 
		* separating days_after_plant does not affect ability to fit model
		* try removing graft*cultivar from random effect


*** Step 2 ***;
	** OBJ: get fit statistics for random effect, test for overdispersion
		-try simpler random effects to eliminate g matrix error for 2019
		-to determine if all blocks/dates can be included if days_after_plant removed from interaction term

	** Step 2-1 ** 
		* OBJ: include days_after_plant;

		%LET name_step=B_step-2-1; %LET title_step=" B step 2-1 ";
	
		ods html body="&name_base.&name_step._sas-output.html" (title=&title_step) path="&results_path" gpath="&results_path_img" (url="html_images/"); ods graphics on / reset imagename="&name_base.&name_step";
		proc printto new log="&results_path.&name_base.&name_step._sas-log.log"; run;
		
		** analyze main effects;
		proc glimmix data=field_d127 plot=residualpanel method=laplace;
			class exp_rep cultivar graft block days_after_plant;
			by exp_rep;
			model rating_sum / n_plants = graft|cultivar|days_after_plant / dist=binomial link=logit htype=3;
			random intercept / subject=block;
			covtest / wald;
			title "field SB &title_step. d127, all blocks - simple random effect - get fit statistics, with days after plant";
		run;
	
		ods graphics off; ods html close; proc printto; run; * saves html, direct log back to SAS Log window;
	
		* RESULTS: successful
			* 2018: converged, very mild evidence of overdispersion (1.21), sensible results. cultivar*graft interaction
			* 2019: converged, very mild evidence of overdispersion (1.10), sensible results. graft only

	** Step 2-2 ** 
		* OBJ: separate days_after_plant;

		%LET name_step=B_step-2-2; %LET title_step=" B step 2-2 ";
	
		ods html body="&name_base.&name_step._sas-output.html" (title=&title_step) path="&results_path" gpath="&results_path_img" (url="html_images/"); ods graphics on / reset imagename="&name_base.&name_step";
		proc printto new log="&results_path.&name_base.&name_step._sas-log.log"; run;
		
		** analyze main effects;
		proc glimmix data=field_d127 plot=residualpanel method=laplace;
			class exp_rep cultivar graft block days_after_plant;
			by exp_rep;
			model rating_sum / n_plants = graft|cultivar days_after_plant / dist=binomial link=logit htype=3;
			random intercept / subject=block;
			covtest / wald;
			title "field SB &title_step. d127, all blocks - simple random effect - get fit statistics, separate days after plant";
		run;
	
		ods graphics off; ods html close; proc printto; run; * saves html, direct log back to SAS Log window;
	
		* RESULTS: successful
			* 2018: converged, very mild evidence of overdispersion (1.32), sensible results. cultivar*graft interaction
			* 2019: converged, very mild evidence of overdispersion (1.33), sensible results. graft only
	
	** CONCLUSIONS: 
		* days_after_plant: separating does not affect ability to fit model
		* use laplace, not doing so requires removing 2nd rating date in 2018
		* use simpler random effect for 2019


* ************************* *
* C. Analyze - Main Effects *
* ************************* *;

*** Step 1 ***;
	** OBJ: analyze main effects
		* use simpler random effect term for 2019 to remove G matrix warning;

	** Step 1-1 **
		* OBJ: evaluate main effects for 2018 (with random graft*cultivar);

		** create dataset;
		data field_r18_d127;
			set field_d127;
			if exp_rep = '2019' then delete;
		run;

		%LET name_step=C_step-1-1; %LET title_step=" C step 1-1 ";
	
		ods html body="&name_base.&name_step._sas-output.html" (title=&title_step) path="&results_path" gpath="&results_path_img" (url="html_images/"); ods graphics on / reset imagename="&name_base.&name_step";
		proc printto new log="&results_path.&name_base.&name_step._sas-log.log"; run;
		
		** analyze main effects;
		proc glimmix data=field_r18_d127 method=laplace;
			class cultivar graft block days_after_plant;
			model rating_sum / n_plants = graft|cultivar|days_after_plant / dist=binomial link=logit htype=3;
			random intercept graft*cultivar / subject=block;
			title "field SB &title_step. d127, all blocks - 2018 only - analyze main effects";
		run;
	
		ods graphics off; ods html close; proc printto; run; * saves html, direct log back to SAS Log window;

		* RESULTS: effect of graft and days_after_plant, some evidence for cultivar

	** Step 1-2 **
		* OBJ: evaluate main effects for 2019 (without random graft*cultivar to remove G matrix error);

		** create dataset;
		data field_r19_d0;
			set field_d0;
			if exp_rep = '2018' then delete;
		run;

		%LET name_step=C_step-1-2; %LET title_step=" C step 1-2 ";
	
		ods html body="&name_base.&name_step._sas-output.html" (title=&title_step) path="&results_path" gpath="&results_path_img" (url="html_images/"); ods graphics on / reset imagename="&name_base.&name_step";
		proc printto new log="&results_path.&name_base.&name_step._sas-log.log"; run;
		
		** analyze main effects;
		proc glimmix data=field_r19_d0 method=laplace;
			class cultivar graft block days_after_plant;
			model rating_sum / n_plants = graft|cultivar|days_after_plant / dist=binomial link=logit htype=3;
			random intercept / subject=block;
			title "field SB &title_step. d0, all blocks - 2019 only - analyze main effects";
		run;
	
		ods graphics off; ods html close; proc printto; run; * saves html, direct log back to SAS Log window;

		* RESULTS: effect of graft, some evidence for days_after_plant, very weak evidence for cultivar
	
	** CONCLUSION: separate means


* **************************** *
* D. Analyze - Mean Separation *
* **************************** *;

*** Step 1 ***;
	** OBJ: separate means
		* use simpler random effect term for 2019 to remove G matrix warning;

	** Step 1-1 **
		* OBJ: separate means for 2018 (with random graft*cultivar);

		%LET name_step=D_step-1-1; %LET title_step=" D step 1-1 ";
	
		ods html body="&name_base.&name_step._sas-output.html" (title=&title_step) path="&results_path" gpath="&results_path_img" (url="html_images/"); ods graphics on / reset imagename="&name_base.&name_step";
		proc printto new log="&results_path.&name_base.&name_step._sas-log.log"; run;
		
		proc glimmix data=field_r18_d127 method=laplace;
			class cultivar graft block days_after_plant;
			model rating_sum / n_plants = graft|cultivar|days_after_plant / dist=binomial link=logit htype=3;
			random intercept graft*cultivar / subject=block;

			lsmeans graft / lines ilink adjust=tukey adjdfe=row;

			title "field SB &title_step. d127, all blocks - 2018 only - separate means";
		run;
	
		ods graphics off; ods html close; proc printto; run; * saves html, direct log back to SAS Log window;

		* RESULTS: standard/tall similar to each other, both different from none
		

	** Step 1-2 **
		* OBJ: separate means for 2019 (without random graft*cultivar to remove G matrix error);

		%LET name_step=D_step-1-2; %LET title_step=" D step 1-2 ";
	
		ods html body="&name_base.&name_step._sas-output.html" (title=&title_step) path="&results_path" gpath="&results_path_img" (url="html_images/"); ods graphics on / reset imagename="&name_base.&name_step";
		proc printto new log="&results_path.&name_base.&name_step._sas-log.log"; run;
		
		proc glimmix data=field_r19_d0 method=laplace;
			class cultivar graft block days_after_plant;
			model rating_sum / n_plants = graft|cultivar|days_after_plant / dist=binomial link=logit htype=3;
			random intercept / subject=block;
	
			lsmeans graft / lines ilink adjust=tukey adjdfe=row;

			title "field SB &title_step. d0, all blocks - 2019 only - separate means";
		run;
	
		ods graphics off; ods html close; proc printto; run; * saves html, direct log back to SAS Log window;

		* RESULTS: standard/tall similar to each other, both different from none

	

