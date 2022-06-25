* ****************************************** *
* PROCESSING TOMATO Southern blight grafting *
* Experiment 2: Grafting - Field - 2017      *
* Data - Southern Blight					 *
* Area Under the Disease Progress Stairs	 *
* ****************************************** *;

* set variables for automatic saving of output;
%LET results_path=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\08_Tomato_rolfsii\SAS_graft\4_results\;
%LET results_path_img=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\08_Tomato_rolfsii\SAS_graft\4_results\html_images;
%LET name_base=rolfsii_graft-field-17-sb-audps_;

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
	datafile="C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\08_Tomato_rolfsii\SAS_graft\2_data\rolfsii_2_field-2017_disease-incidence_audps_final.csv"
	dbms=dlm replace out=field_audps_17;
	delimiter=",";
	getnames=YES;
run;


** convert cultivar to character;
	* create new variable - put function always returns character so no $ needed;
data field_audps_17;
	set field_audps_17;
	cult_new = put(cultivar, 4.);
	drop cultivar;
	rename cult_new=cultivar;
run;

** sort dataset
	* by rating for 'by group' processing in proc glimmix;
	* good practice to sort dataset in same order that variables appear in class statement
	* by days_after_plant to improve processing time (?);
proc sort data=field_audps_17;
	by cultivar graft block subplot;
run;
	
ods html path="&results_path" body="&name_base.A_sas-output.html";
** check dataset;
proc print data=field_audps_17;
	title "&name_base. full review";
run;
ods html close; * saves html;
proc printto; run; * direct log back to SAS Log window;

** check variable types
*proc contents data=field_audps_17;
*run;


* **************************** *
* B. Analyze - Evaluate Models *
* **************************** *;

*** Step 1 ***;
	** OBJ: get fit statistics for random effect (and test for overdispersion?);

	** Step 1-1 **;
		* OBJ: evaluate with stroup/complex random effect;
		
		%LET name_step=B_step-1-1; %LET title_step=" B step 1-1 ";

		ods html body="&name_base.&name_step._sas-output.html" (title=&title_step) path="&results_path" gpath="&results_path_img" (url="html_images/"); ods graphics on / reset imagename="&name_base.&name_step";
		proc printto new log="&results_path.&name_base.&name_step._sas-log.log"; run;
	
		** analyze main effects;
		proc glimmix data=field_audps_17 plot=residualpanel plot=pearsonpanel method=laplace;
			class cultivar graft block subplot;
			model raudps = graft|cultivar / dist=beta link=logit htype=3;
			random intercept graft*cultivar graft*cultivar*subplot / subject=block;
			covtest / wald;
			title "field-2017 SB rAUDPS &title_step. - get fit statistics - complex random effect";
		run;

		ods graphics off; ods html close; proc printto; run; * saves html, direct log back to SAS Log window;
	
		* RESULTS: did not run ("the initial estimates did not yield a valid objective function")

	** Step 1-2 **;
		* OBJ: evaluate with simpler random effect;
		
		%LET name_step=B_step-1-2; %LET title_step=" B step 1-2 ";

		ods html body="&name_base.&name_step._sas-output.html" (title=&title_step) path="&results_path" gpath="&results_path_img" (url="html_images/"); ods graphics on / reset imagename="&name_base.&name_step";
		proc printto new log="&results_path.&name_base.&name_step._sas-log.log"; run;
	
		** analyze main effects;
		proc glimmix data=field_audps_17 plot=residualpanel plot=pearsonpanel method=laplace;
			class cultivar graft block subplot;
			model raudps = graft|cultivar / dist=beta link=logit htype=3;
			random intercept / subject=block;
			covtest / wald;
			title "field-2017 SB rAUDPS &title_step. - get fit statistics - simple random effect";
		run;

		ods graphics off; ods html close; proc printto; run; * saves html, direct log back to SAS Log window;
	
		* NOTE: including graft*cultivar in random effect causes estimated G matrix error
		* RESULTS: ran smoothly
 
	** CONCLUSION: use simple random effects


* ************************ *
* C. Analyze - Main Effect *
* ************************ *;

*** Step 1 ***;
	** OBJ: evaluate with simple random effect (remove method=laplace);
		
		%LET name_step=C_step-1; %LET title_step=" C step 1 ";

		ods html body="&name_base.&name_step._sas-output.html" (title=&title_step) path="&results_path" gpath="&results_path_img" (url="html_images/"); ods graphics on / reset imagename="&name_base.&name_step";
		proc printto new log="&results_path.&name_base.&name_step._sas-log.log"; run;
	
		** analyze main effects;
		proc glimmix data=field_audps_17 plot=residualpanel plot=pearsonpanel;
			class cultivar graft block subplot;
			model raudps = graft|cultivar / dist=beta link=logit htype=3;
			random intercept / subject=block;
			covtest / wald;
			title "field-2017 SB rAUDPS &title_step. - analyze";
		run;

		ods graphics off; ods html close; proc printto; run; * saves html, direct log back to SAS Log window;



	