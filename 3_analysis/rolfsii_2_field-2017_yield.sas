* ****************************************** *
* PROCESSING TOMATO Southern blight grafting *
* Experiment 2: Grafting - Field - 2017      *
* Data - Yield					 *
* ****************************************** *;

* set variables for automatic saving of output;
%LET results_path=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\08_Tomato_rolfsii\SAS_graft\4_results\;
%LET results_path_img=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\08_Tomato_rolfsii\SAS_graft\4_results\html_images;
%LET name_base=rolfsii_graft-field-17-yield_;

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
	datafile="C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\08_Tomato_rolfsii\SAS_graft\2_data\rolfsii_2_field-2017_yield_final.csv"
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
	by cultivar graft block;
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
	** OBJ: get fit statistics for random effect;

	%LET name_step=B_step-1; %LET title_step=" B step 1 ";

	ods html body="&name_base.&name_step._sas-output.html" (title=&title_step) path="&results_path" gpath="&results_path_img" (url="html_images/"); ods graphics on / reset imagename="&name_base.&name_step";
	proc printto new log="&results_path.&name_base.&name_step._sas-log.log"; run;
	
	** analyze main effects;
	proc glimmix data=field_17 plot=residualpanel method=laplace;
		class cultivar graft block;
		model yield_mt_ha = graft|cultivar / dist=normal link=identity htype=3;
		random intercept / subject=block;
		covtest / wald;
		title "field-2017 yield &title_step. - get fit statistics";
	run;

	ods graphics off; ods html close; proc printto; run; * saves html, direct log back to SAS Log window;
	
	** RESULTS: fit statistics, covariance estimates don't make sense or are blank
		* applies to raw yield totals
		* when adjusted to per area and converted to metric, no abnormalities noted
		* use lognormal in step 2 below because results are very similar to raw totals

	** CONCLUSION: try lognormal

*** Step 2 ***;
	** OBJ: attempt lognormal to get sensible results;

	%LET name_step=B_step-2; %LET title_step=" B step 2 ";

	ods html body="&name_base.&name_step._sas-output.html" (title=&title_step) path="&results_path" gpath="&results_path_img" (url="html_images/"); ods graphics on / reset imagename="&name_base.&name_step";
	proc printto new log="&results_path.&name_base.&name_step._sas-log.log"; run;
	
	** analyze main effects;
	proc glimmix data=field_17 plot=residualpanel method=laplace;
		class cultivar graft block;
		model yield_mt_ha = graft|cultivar / dist=lognormal link=identity htype=3;
		random intercept / subject=block;
		covtest / wald;
		title "field-2017 yield &title_step. - get fit statistics - lognormal";
	run;

	ods graphics off; ods html close; proc printto; run; * saves html, direct log back to SAS Log window;
	
	** RESULTS: results are sensible
		* main effect of graft
		
	** CONCLUSION: no further analysis needed