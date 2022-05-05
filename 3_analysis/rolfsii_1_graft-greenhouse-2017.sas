* ****************************************** *
* PROCESSING TOMATO Southern blight grafting *
* Experiment 1: Graft - Greenhouse - 2017    *
* ****************************************** *;

* NOTE: adapted from Karen Xu's consulting analysis;

%LET results_path=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\08_Tomato_rolfsii\SAS_graft\4_results\;
%LET results_path_img=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\08_Tomato_rolfsii\SAS_graft\4_results\html_images;
%LET name_base=rolfsii_graft-gh-17_;

** log;
proc printto new log="&results_path.&name_base.A_sas-log.log"; run;

options ls=90 nonumber formdlim=' ' pagesize=52 center;

* ********* *
* A. Import
* ********* *;

** import data;
proc import 
		datafile="C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\08_Tomato_rolfsii\SAS_graft\2_data\rolfsii_1_graft-greenhouse-2017_disease-serverity_final.csv"
		dbms=dlm replace out=graft_gh;
	delimiter=",";
	getnames=YES;
run;

** sort dataset;
proc sort data=graft_gh;
	by cultivar graft inoculum block days_after_plant;
run;

** convert columns to character; * -L makes anything left-justified within 2 character space;
data graft_gh;
	set graft_gh;
	inoc_new = put(inoculum, 2. -L);
	drop inoculum;
	rename inoc_new=inoculum;
run;
	
ods html path="&results_path" body="&name_base.A_sas-output.html";
** check dataset;
proc print data=graft_gh;
	title "&name_base. full review";
run;
ods html close; * saves html;
proc printto; run; * direct log back to SAS Log window;


* ************************************************ *
* B. Analyze - Main Effects - No Repeated Measures *
* ************************************************ *;
	
*** Step 1 ***;
	** OBJ: analyze main effects;
	%LET name_step=B_step-1; %LET title_step=" B step 1 ";

	ods html body="&name_base.&name_step._sas-output.html" (title=&title_step) path="&results_path" gpath="&results_path_img" (url="html_images/"); ods graphics on / reset imagename="&name_step";
	proc printto new log="&results_path.&name_base.&name_step._sas-log.log"; run;

	proc glimmix data=graft_gh method=laplace;
		class cultivar graft inoculum block days_after_plant;
		model disease_severity = cultivar|graft|inoculum days_after_plant / dist=multinomial link=cumlogit;
		random intercept / subject=block;
		covtest / wald;
		title "graft gh 2017 &title_step. - main effects";
	run;

	ods graphics off; ods html close; proc printto; run; * saves html, direct log back to SAS Log window;

	** RESULTS: appear sensible, 3-way interaction
	** CONCLUSION: analyze interactions


* ******************************************************* *
* C. Analyze - Interaction Effects - No Repeated Measures *
* ******************************************************* *;
	
*** Step 1 ***;
	** OBJ: analyze interaction effects;
	%LET name_step=C_step-1; %LET title_step=" C step 1 ";

	ods html body="&name_base.&name_step._sas-output.html" (title=&title_step) path="&results_path" gpath="&results_path_img" (url="html_images/"); ods graphics on / reset imagename="&name_step";
	proc printto new log="&results_path.&name_base.&name_step._sas-log.log"; run;

	proc glimmix data=graft_gh method=laplace;
		class cultivar graft inoculum block days_after_plant;
		model disease_severity = cultivar|graft|inoculum days_after_plant / dist=multinomial link=cumlogit;
		random intercept / subject=block;
		covtest / wald;
		
		slice cultivar*graft*inoculum / sliceBy=cultivar*inoculum;
		
		title "graft gh 2017 &title_step. - interaction effects";
	run;

	ods graphics off; ods html close; proc printto; run; * saves html, direct log back to SAS Log window;

	** RESULTS: evidence for influence of grafting for 5608 - all inoculum rates, 8504 - inoculum 20 only
	** CONCLUSION: 
