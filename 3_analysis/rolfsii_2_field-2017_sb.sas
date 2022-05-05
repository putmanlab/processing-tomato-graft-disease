* ****************************************** *
* PROCESSING TOMATO Southern blight grafting *
* Experiment 2: Grafting - Field - 2017      *
* Data - Southern Blight					 *
* ****************************************** *;

* set variables for automatic saving of output;
%LET results_path=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\08_Tomato_rolfsii\SAS_graft\4_results\;
%LET results_path_img=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\08_Tomato_rolfsii\SAS_graft\4_results\html_images;
%LET name_base=rolfsii_graft-field-17-sb_;

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
	datafile="C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\08_Tomato_rolfsii\SAS_graft\2_data\rolfsii_2_field-2017_disease-incidence_final.csv"
	dbms=dlm replace out=field_17;
	delimiter=",";
	getnames=YES;
run;


** convert cultivar to character;
	* create new variable - put function always returns character so no $ needed;
data field_17;
	set field_17;
	cult_new = put(cultivar, 4.);
	drop cultivar;
	rename cult_new=cultivar;
run;

** sort dataset
	* by rating for 'by group' processing in proc glimmix;
	* good practice to sort dataset in same order that variables appear in class statement
	* by days_after_plant to improve processing time (?);
proc sort data=field_17;
	by cultivar graft block subplot days_after_plant;
run;
	
ods html path="&results_path" body="&name_base.A_sas-output.html";
** check dataset;
proc print data=field_17;
	title "&name_base. full review";
run;
ods html close; * saves html;
proc printto; run; * direct log back to SAS Log window;

** check variable types
*proc contents data=field_17;
*run;


* **************************** *
* B. Analyze - Evaluate Models *
* **************************** *;

*** Step 1 ***;
	** OBJ: get fit statistics for random effect, test for overdispersion;

	%LET name_step=B_step-1; %LET title_step=" B step 1 ";

	ods html body="&name_base.&name_step._sas-output.html" (title=&title_step) path="&results_path" gpath="&results_path_img" (url="html_images/"); ods graphics on / reset imagename="&name_base.&name_step";
	proc printto new log="&results_path.&name_base.&name_step._sas-log.log"; run;
	
	** analyze main effects;
	proc glimmix data=field_17 plot=residualpanel plot=pearsonpanel method=laplace;
		class cultivar graft block subplot days_after_plant;
		model disease_incidence / n_plants = graft|cultivar|days_after_plant / dist=binomial link=logit htype=3;
		random intercept graft*cultivar graft*cultivar*subplot / subject=block;
		covtest / wald;
		title "field-2017 SB &title_step. - get fit statistics";
	run;

	ods graphics off; ods html close; proc printto; run; * saves html, direct log back to SAS Log window;
	
	** RESULTS: 
		* converged, sensible
		* Pearson/DF = 2.34, possible evidence for overdispersion
	** CONCLUSION: attempt to correct for overdispersion

*** Step 2 ***;
	** OBJ: attempt to correct for overdispersion
	
	** Step 2-1 **
		* OBJ: quasi-likelihood to correct for overdispersion;

		%LET name_step=B_step-2-1; %LET title_step=" B step 2-1 ";
	
		ods html body="&name_base.&name_step._sas-output.html" (title=&title_step) path="&results_path" gpath="&results_path_img" (url="html_images/"); ods graphics on / reset imagename="&name_base.&name_step";
		proc printto new log="&results_path.&name_base.&name_step._sas-log.log"; run;
		
		proc glimmix data=field_17 plot=residualpanel plot=pearsonpanel;
			class cultivar graft block subplot days_after_plant;
			model disease_incidence / n_plants = graft|cultivar|days_after_plant / dist=binomial link=logit htype=3;
			random intercept / subject=block;
			random _residual_;
			covtest / wald;
			title "field-2017 SB &title_step. - correct for overdispersion - quasi-likelihood";
		run;
	
		ods graphics off; ods html close; proc printto; run; * saves html, direct log back to SAS Log window;
		
		* RESULTS: 

	** Step 2-1 **
		* OBJ: standard model (no Laplace = conditional model = pseudo-likelihood?);
		
		%LET name_step=B_step-2-2; %LET title_step=" B step 2-2 ";
	
		ods html body="&name_base.&name_step._sas-output.html" (title=&title_step) path="&results_path" gpath="&results_path_img" (url="html_images/"); ods graphics on / reset imagename="&name_base.&name_step";
		proc printto new log="&results_path.&name_base.&name_step._sas-log.log"; run;
		
		proc glimmix data=field_17 plot=residualpanel plot=pearsonpanel;
			class cultivar graft block subplot days_after_plant;
			model disease_incidence / n_plants = graft|cultivar|days_after_plant / dist=binomial link=logit htype=3;
			random intercept graft*cultivar graft*cultivar*subplot / subject=block;
			covtest / wald;
			title "field-2017 SB &title_step. - correct for overdispersion - standard/conditional model (no laplace)";
		run;
	
		ods graphics off; ods html close; proc printto; run; * saves html, direct log back to SAS Log window;
		
		* RESULTS: 

	** CONCLUSION: unclear if corrected
		* analyze graft*days_after_plant interaction
		

* ************************ *
* C. Analyze - Interaction *
* ************************ *;

*** Step 1 ***;
	** OBJ: analyze interaction;

	%LET name_step=C_step-1; %LET title_step=" C step 1 ";

	ods html body="&name_base.&name_step._sas-output.html" (title=&title_step) path="&results_path" gpath="&results_path_img" (url="html_images/"); ods graphics on / reset imagename="&name_base.&name_step";
	proc printto new log="&results_path.&name_base.&name_step._sas-log.log"; run;
	
	** analyze main effects;
	proc glimmix data=field_17 method=laplace;
		class cultivar graft block subplot days_after_plant;
		model disease_incidence / n_plants = graft|cultivar|days_after_plant / dist=binomial link=logit htype=3;
		random intercept graft*cultivar graft*cultivar*subplot / subject=block;

		slice graft*days_after_plant / sliceBy=days_after_plant;
		slice cultivar*days_after_plant / sliceBy=cultivar;
		slice cultivar*days_after_plant / sliceBy=days_after_plant;

		title "field-2017 SB &title_step. - analyze interactions";
	run;

	ods graphics off; ods html close; proc printto; run; * saves html, direct log back to SAS Log window;
	
	** RESULTS: 
		* graft:
			* effect of graft significant for all dates except date 2
			* incidence is numerically higher in non-grafted plots on all dates
		* cultivar: 
			* when sliced by days_after_plant, effect of cultivar not significant on any date
			* when sliced by cultivar, days_after_plant significant for both cultivars
	** CONCLUSION: 



	