##############################################
# PROCESSING TOMATO Southern blight grafting #
# Experiment 4: Grafting - Field 	         #
##############################################

## built on Docker putmanlab/exploratory-analysis:0.1.1

library(ggplot2)
library(lubridate)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(cowplot)

directory="/home/tomato_graft_rolfsii"
setwd(directory)

####################
# A. Import - 2018 #
####################

### import file
	## import
	e4r1.in = read_csv(file="./2_data/Boswell_graft_rating_8.10.18_edit4_cleaned.csv", col_names=T, na="")
	
	## rename plot
	e4r1.in = e4r1.in %>% rename("plot_num"=plot)
	
	## convert to long format
	e4r1.in = e4r1.in %>% gather(key="date", value="rating", -plant, -block, -plot_num)

### curate
	## check rating column
#	e4r1.in %>% group_by(rating) %>% summarize(count=n())
	
	## replace blank cells with healthy rating
	e4r1.in.t = e4r1.in %>% mutate(rating=replace(rating, is.na(rating), "H_H"))
	
	## replace - with NA
	e4r1.in.t = e4r1.in.t %>% mutate(rating=replace(rating, rating == "-", NA))

	## check rating column
#	e4r1.in.t %>% group_by(rating) %>% summarize(count=n())
	
	## assign treatment info
		# import
		e4r1.trt = read_csv(file="/home/tomato_graft_rolfsii/2_data/worksheets/graft-rolfsii_4_plot-treatment-assignments.csv", col_names=T, na="")
		
		# change cultivar to character
		e4r1.trt = e4r1.trt %>% mutate(cultivar=as.character(cultivar))
		
		# join treatment info to rating data
		e4r1.t = e4r1.in.t %>% left_join(e4r1.trt, by=c("block" = "block", "plot_num" = "plot"))
	
	## change to actual dates
		# make tibble of dates
		e4r1.dates = tibble(date_let=c("A","B","C","D","E","F","G"), date_act=as_date(c("2018-05-11","2018-06-05","2018-06-15","2018-06-28","2018-07-13","2018-07-24","2018-08-10")))
		
		# join with data
		e4r1.dat = e4r1.t %>% left_join(e4r1.dates, by=c("date" = "date_let"))
		
		# remove old date column, reorder columns, rename old date column
		e4r1.dat = e4r1.dat %>% select(cultivar, graft, treatment, block, plot_num, plant, date_act, rating) %>% rename("date"=date_act)

####################
# B. Import - 2019 #
####################

### import file
	e4r2.in = read_csv(file="./2_data/Boswell_rating4_8.8.19_cleaned.csv", col_names=T, na="-")

### clean up
	## remove gap rows in between blocks
	e4r2.in = e4r2.in %>% filter(!is.na(block))

	## check plant number columns and remove extra
		# check equality
		e4r2.in %>% filter(Plant_L != Plant_R)
		
		# remove extra column and rename
		e4r2.in = e4r2.in %>% select(-Plant_R) %>% rename("plant"=Plant_L)
		
	## convert "" (empty) columns to "H_H" (healthy)
	e4r2.in = e4r2.in %>% mutate_at(vars(-block, -plant), funs( replace(., . == "", "H_H") ) )
	
### curate ratings - preparation 
	## convert to wide by rating date
		# gather all plots and dates
		e4r2.in.l = e4r2.in %>% gather(key="plot_date", value="rating", -block, -plant)
	
		# separate plot_date
		e4r2.in.l = e4r2.in.l %>% separate(plot_date, into=c("plot_num","date"), sep="_")
		
		# remove "Plot" from plot
		e4r2.in.l = e4r2.in.l %>% mutate(plot_num=str_replace(plot_num, "Plot", ""))
		
		# replace ? in a few cells
		e4r2.in.l = e4r2.in.l %>% mutate(rating=str_replace(rating, "\\?", ""))

		# remove unused rating date E
		e4r2.in.l = e4r2.in.l %>% filter(date != "E")
		
		# spread by date
		e4r2.in.w = e4r2.in.l %>% spread(key=date, value=rating)
	
	## set up
		# create edited flag column
		e4r2.in.w = e4r2.in.w %>% mutate(flag_edited=as.integer(NA))
		
		# create duplicate of rating date columns; could not figure out how to do mutate multiple columns based on conditions of all the same columns
		e4r2.t = e4r2.in.w %>% mutate(A_=A, B_=B, C_=C, D_=D)

		# view
#		e4r2.t %>% filter(is.na(flag_edited) & !(A_ == "H_H" & B_ == "H_H" & C_ == "H_H" & D_ == "H_H")) %>% arrange(A,B,C,D) %>% print(n=Inf)
		
### curate ratings - convert ratings that are constant through rating dates 
	## "CT" to "CT_S" (curly top symptoms)
		# all 4 rating dates
		e4r2.t = e4r2.t %>% mutate_at(vars(A,B,C,D), funs( replace(., A_ == "CT" & B_ == "CT" & C_ == "CT" & (D_ == "CT" | D_ == "H_H"), "CT_S") ) ) %>% mutate(flag_edited=replace(flag_edited, A_ == "CT" & B_ == "CT" & C_ == "CT" & (D_ == "CT" | D_ == "H_H"), 1))

		# rating dates B,C,D
		e4r2.t = e4r2.t %>% mutate_at(vars(B,C,D), funs( replace(., A_ == "H_H" & B_ == "CT" & C_ == "CT" & D_ == "CT", "CT_S") ) ) %>% mutate(flag_edited=replace(flag_edited, A_ == "H_H" & B_ == "CT" & C_ == "CT" & D_ == "CT", 2))
		
		# rating dates C,D
		e4r2.t = e4r2.t %>% mutate_at(vars(C,D), funs( replace(., A_ == "H_H" & B_ == "H_H" & C_ == "CT" & D_ == "CT", "CT_S") ) ) %>% mutate(flag_edited=replace(flag_edited, A_ == "H_H" & B_ == "H_H" & C_ == "CT" & D_ == "CT", 3))
					
	## "V" to "V_S" (unknown virus symptoms)
		# all 4 rating dates
		e4r2.t = e4r2.t %>% mutate_at(vars(A,B,C,D), funs( replace(., A_ == "V" & B_ == "V" & C_ == "V" & D_ == "V", "V_S") ) ) %>% mutate(flag_edited=replace(flag_edited, A_ == "V" & B_ == "V" & C_ == "V" & D_ == "V", 9))

		# rating dates B,C,D
		e4r2.t = e4r2.t %>% mutate_at(vars(B,C,D), funs( replace(., A_ == "H_H" & B_ == "V" & C_ == "V" & D_ == "V", "V_S") ) ) %>% mutate(flag_edited=replace(flag_edited, A_ == "H_H" & B_ == "V" & C_ == "V" & D_ == "V", 10))
		
		# rating dates C,D
		e4r2.t = e4r2.t %>% mutate_at(vars(C,D), funs( replace(., A_ == "H_H" & B_ == "H_H" & C_ == "V" & D_ == "V", "V_S") ) ) %>% mutate(flag_edited=replace(flag_edited, A_ == "H_H" & B_ == "H_H" & C_ == "V" & D_ == "V", 11))

		# rating date D
		e4r2.t = e4r2.t %>% mutate_at(vars(D), funs( replace(., A_ == "H_H" & B_ == "H_H" & C_ == "H_H" & D_ == "V", "V_S") ) ) %>% mutate(flag_edited=replace(flag_edited, A_ == "H_H" & B_ == "H_H" & C_ == "H_H" & D_ == "V", 12))

	## "WE" to "O_S" (weak to other symptoms)		
		# rating dates C,D
		e4r2.t = e4r2.t %>% mutate_at(vars(C,D), funs( replace(., A_ == "H_H" & B_ == "H_H" & C_ == "WE" & D_ == "WE", "O_S") ) ) %>% mutate(flag_edited=replace(flag_edited, A_ == "H_H" & B_ == "H_H" & C_ == "WE" & D_ == "WE", 14))

		# rating date D
		e4r2.t = e4r2.t %>% mutate_at(vars(D), funs( replace(., A_ == "H_H" & B_ == "H_H" & C_ == "H_H" & D_ == "WE", "O_S") ) ) %>% mutate(flag_edited=replace(flag_edited, A_ == "H_H" & B_ == "H_H" & C_ == "H_H" & D_ == "WE", 15))
	
	## "D" to "O_D" (dead on first date classified as death from other)
		# all 4 rating dates
		e4r2.t = e4r2.t %>% mutate_at(vars(A,B,C,D), funs( replace(., A_ %in% c("D","DB","B") & B_ == "D" & C_ == "D" & D_ == "D", "O_D") ) ) %>% mutate(flag_edited=replace(flag_edited, A_ %in% c("D","DB","B") & B_ == "D" & C_ == "D" & D_ == "D", 17))
			
	## "SB" to "SB_S" (southern blight symptoms)		
		# rating dates C,D
		e4r2.t = e4r2.t %>% mutate_at(vars(C,D), funs( replace(., A_ == "H_H" & B_ == "H_H" & C_ == "SB" & D_ == "SB", "SB_S") ) ) %>% mutate(flag_edited=replace(flag_edited, A_ == "H_H" & B_ == "H_H" & C_ == "SB" & D_ == "SB", 20))

		# rating date D
		e4r2.t = e4r2.t %>% mutate_at(vars(D), funs( replace(., A_ == "H_H" & B_ == "H_H" & C_ == "H_H" & D_ == "SB", "SB_S") ) ) %>% mutate(flag_edited=replace(flag_edited, A_ == "H_H" & B_ == "H_H" & C_ == "H_H" & D_ == "SB", 21))		
	
### assign mortality cause
	## SB
		# A=SB, B,C,D=D; replace D to SB_D, then SB to SB_S
		e4r2.t = e4r2.t %>% mutate_at(vars(B,C,D), funs( replace(., A_ == "SB" & B_ == "D" & C_ == "D" & D_ == "D", "SB_D") ) )
		e4r2.t = e4r2.t %>% mutate_at(vars(A), funs( replace(., A_ == "SB" & B_ == "D" & C_ == "D" & D_ == "D", "SB_S") ) ) %>% mutate(flag_edited=replace(flag_edited, A_ == "SB" & B_ == "D" & C_ == "D" & D_ == "D", 30))

		# A,B=SB, C,D=D; replace D to SB_D, then SB to SB_S
		e4r2.t = e4r2.t %>% mutate_at(vars(C,D), funs( replace(., A_ == "SB" & B_ == "SB" & C_ == "D" & D_ == "D", "SB_D") ) )
		e4r2.t = e4r2.t %>% mutate_at(vars(A,B), funs( replace(., A_ == "SB" & B_ == "SB" & C_ == "D" & D_ == "D", "SB_S") ) ) %>% mutate(flag_edited=replace(flag_edited, A_ == "SB" & B_ == "SB" & C_ == "D" & D_ == "D", 31))

		# A,B,C=SB, D=D; replace D to SB_D, then SB to SB_S
		e4r2.t = e4r2.t %>% mutate_at(vars(D), funs( replace(., A_ == "SB" & B_ == "SB" & C_ == "SB" & D_ == "D", "SB_D") ) )
		e4r2.t = e4r2.t %>% mutate_at(vars(A,B,C), funs( replace(., A_ == "SB" & B_ == "SB" & C_ == "SB" & D_ == "D", "SB_S") ) ) %>% mutate(flag_edited=replace(flag_edited, A_ == "SB" & B_ == "SB" & C_ == "SB" & D_ == "D", 32))

		# A=NA, B=SB, C,D=D; replace D to SB_D, then SB to SB_S
		e4r2.t = e4r2.t %>% mutate_at(vars(C,D), funs( replace(., A_ == "H_H" & B_ == "SB" & C_ == "D" & D_ == "D", "SB_D") ) )
		e4r2.t = e4r2.t %>% mutate_at(vars(B), funs( replace(., A_ == "H_H" & B_ == "SB" & C_ == "D" & D_ == "D", "SB_S") ) ) %>% mutate(flag_edited=replace(flag_edited, A_ == "H_H" & B_ == "SB" & C_ == "D" & D_ == "D", 33))

		# A=NA, B,C=SB, C=D; replace D to SB_D, then SB to SB_S
		e4r2.t = e4r2.t %>% mutate_at(vars(D), funs( replace(., A_ == "H_H" & B_ == "SB" & C_ == "SB" & D_ == "D", "SB_D") ) )
		e4r2.t = e4r2.t %>% mutate_at(vars(B,C), funs( replace(., A_ == "H_H" & B_ == "SB" & C_ == "SB" & D_ == "D", "SB_S") ) ) %>% mutate(flag_edited=replace(flag_edited, A_ == "H_H" & B_ == "SB" & C_ == "SB" & D_ == "D", 34))

		# A,B=NA, C=SB, D=D; replace D to SB_D, then SB to SB_S
		e4r2.t = e4r2.t %>% mutate_at(vars(D), funs( replace(., A_ == "H_H" & B_ == "H_H" & C_ == "SB" & D_ == "D", "SB_D") ) )
		e4r2.t = e4r2.t %>% mutate_at(vars(C), funs( replace(., A_ == "H_H" & B_ == "H_H" & C_ == "SB" & D_ == "D", "SB_S") ) ) %>% mutate(flag_edited=replace(flag_edited, A_ == "H_H" & B_ == "H_H" & C_ == "SB" & D_ == "D", 35))

		# A=CT, B=SB, C,D=D
		e4r2.t = e4r2.t %>% mutate_at(vars(C,D), funs( replace(., A_ == "CT" & B_ == "SB" & C_ == "D" & D_ == "D", "SB_D") ) )
		e4r2.t = e4r2.t %>% mutate_at(vars(A,B), funs( replace(., A_ == "CT" & B_ == "SB" & C_ == "D" & D_ == "D", "SB_S") ) ) %>% mutate(flag_edited=replace(flag_edited, A_ == "CT" & B_ == "SB" & C_ == "D" & D_ == "D", 39))

	## WE to SB
		# A=NA, B=WE, C=SB, D=D;
		e4r2.t = e4r2.t %>% mutate_at(vars(D), funs( replace(., A_ == "H_H" & B_ == "WE" & C_ == "SB" & D_ == "D", "SB_D") ) )
		e4r2.t = e4r2.t %>% mutate_at(vars(B,C), funs( replace(., A_ == "H_H" & B_ == "WE" & C_ == "SB" & D_ == "D", "SB_S") ) ) %>% mutate(flag_edited=replace(flag_edited, A_ == "H_H" & B_ == "WE" & C_ == "SB" & D_ == "D", 36))

	## CT
		# A,B,C=CT, D=D;
		e4r2.t = e4r2.t %>% mutate_at(vars(D), funs( replace(., A_ == "CT" & B_ == "CT" & C_ == "CT" & D_ == "D", "CT_D") ) )
		e4r2.t = e4r2.t %>% mutate_at(vars(A,B,C), funs( replace(., A_ == "CT" & B_ == "CT" & C_ == "CT" & D_ == "D", "CT_S") ) ) %>% mutate(flag_edited=replace(flag_edited, A_ == "CT" & B_ == "CT" & C_ == "CT" & D_ == "D", 37))

### split datasets: modified and unmodified
	## split
	e4r2.t.unmod = e4r2.t %>% filter(!is.na(flag_edited) | (A_ == "H_H" & B_ == "H_H" & C_ == "H_H" & D_ == "H_H") | (is.na(A_) & is.na(B_) & is.na(C_) & is.na(D_) ) )
	e4r2.t.mod.out = e4r2.t %>% filter(is.na(flag_edited) & !(A_ == "H_H" & B_ == "H_H" & C_ == "H_H" & D_ == "H_H")) %>% arrange(A,B,C,D)
		
	## view
#	e4r2.t.mod.out %>% print(n=Inf)
#	e4r2.t.unmod %>% group_by(flag_edited) %>% summarize(ct=n()) %>% print(n=Inf)

### manually curate rest of plants
	## export
	write_csv(e4r2.t.mod.out, path="./2_data/worksheets/rolfsii_4_field-2018_manually-curate-plant-ratings_output.csv", append=F, col_names=T, na="")

	## import modified ratings
	e4r2.t.mod.in = read_csv(file="./2_data/worksheets/rolfsii_4_field-2018_manually-curate-plant-ratings_input.csv", col_names=T, na="")

		# convert plot_num to character
		e4r2.t.mod.in = e4r2.t.mod.in %>% mutate(plot_num=as.character(plot_num))
		
	## bind
	e4r2.aft = bind_rows(e4r2.t.unmod, e4r2.t.mod.in)
	
### check datasets before and after modification
	## convert to long
	e4r2.aft.l = e4r2.aft %>% select(-flag_edited, -A_, -B_, -C_, -D_) %>% gather(key="date", value="rating", -block, -plant, -plot_num)
	
	## summarize
	chk.in = e4r2.in.l %>% group_by(block, plot_num) %>% summarize(ct=n(), ct_na=sum(is.na(rating)), ct_sb=sum(rating == "SB", na.rm=T), ct_ct=sum(rating == "CT", na.rm=T), ct_hh=sum(rating == "H_H", na.rm=T), ct_oth=sum(!rating %in% c("SB","CT","H_H")))
	chk.aft = e4r2.aft.l %>% group_by(block, plot_num) %>% summarize(ct_f=n(), ct_na_f=sum(is.na(rating)), ct_sb_f=sum(rating == "SB_S", na.rm=T), ct_ct_f=sum(rating == "CT_S", na.rm=T), ct_hh_f=sum(rating == "H_H", na.rm=T), ct_oth_f=sum(!rating %in% c("SB_S","CT_S","H_H")))
	
#	chk.in %>% left_join(chk.aft, by=c("block" = "block", "plot_num" = "plot_num")) %>% print(n=Inf)

### add treatment info
	## import randomization
	e4r2.rand = read_csv(file="./1_methods/2019 field trial r randomization results.csv", col_names=T)
	
	## get plot number (1,2,...)
		# convert to character
		e4r2.rand = e4r2.rand %>% mutate(plots=as.character(plots))
		
		# replace
		e4r2.rand = e4r2.rand %>% mutate(plots=str_replace(plots, "10", ""))
		e4r2.rand = e4r2.rand %>% mutate(plots=str_replace(plots, "20", ""))
		e4r2.rand = e4r2.rand %>% mutate(plots=str_replace(plots, "30", ""))
		e4r2.rand = e4r2.rand %>% mutate(plots=str_replace(plots, "40", ""))
		e4r2.rand = e4r2.rand %>% mutate(plots=str_replace(plots, "50", ""))
		e4r2.rand = e4r2.rand %>% mutate(plots=str_replace(plots, "60", ""))
		
		# rename column; change cultivar to character
		e4r2.rand = e4r2.rand %>% rename(plot_num=plots) %>% mutate(cultivar=as.character(cultivar))
	
	## join
	e4r2.f = e4r2.aft.l %>% left_join(e4r2.rand, by=c("block" = "block", "plot_num" = "plot_num"))

### change to actual dates
	## make tibble of dates
	e4r2.dates = tibble(date_let=c("A","B","C","D"), date_act=as_date(c("2019-06-26","2019-07-10","2019-07-24","2019-08-08")))
		
	## join with data
	e4r2.f = e4r2.f %>% left_join(e4r2.dates, by=c("date" = "date_let"))
		
	## remove old date column, rename old date column
	e4r2.f = e4r2.f %>% select(-date) %>% rename("date"=date_act)

		
########################
# C. Organize - Plants #
########################

### bind
	## add identifier columns
	e4r1.dat = e4r1.dat %>% mutate(exp_rep=as.character(2018))
	e4r2.f = e4r2.f %>% mutate(exp_rep=as.character(2019))
	
	## convert to character
	e4r1.dat = e4r1.dat %>% mutate(plot_num=as.character(plot_num))
	
	## bind
	data.e4 = bind_rows(e4r1.dat, e4r2.f)

### add days after planting column
	## create column
	data.e4 = data.e4 %>% mutate(days_after_plant=as.integer(NA))
	
	## fill
	data.e4 = data.e4 %>% mutate(days_after_plant=replace(days_after_plant, exp_rep == "2018", as.integer( date[exp_rep == "2018"] - as_date("2018-04-24") ) ))
	data.e4 = data.e4 %>% mutate(days_after_plant=replace(days_after_plant, exp_rep == "2019", as.integer( date[exp_rep == "2019"] - as_date("2019-05-21") ) ))
	
### order columns, remove treatment column
	data.e4 = data.e4 %>% select(exp_rep, cultivar, graft, block, plot_num, plant, date, days_after_plant, rating)

### checks
#	data.e4 %>% group_by(exp_rep, rating) %>% summarize(ct=n())
#	data.e4 %>% group_by(exp_rep, cultivar, graft) %>% summarize(ct=n(), ct_na=sum(is.na(rating)))

### create dataset with NAs removed for SAS analysis
#	data.e4.narm = data.e4 %>% filter(!is.na(rating))

### export
	write_csv(data.e4, path="./2_data_curated/rolfsii_4_field_incidence_by-plant_final.csv", col_names=T, append=F, na="NA")
#	write_csv(data.e4.narm, path="./2_data_curated/graft-rolfsii_4_field_incidence_for-SAS_missing-removed.csv", col_names=T, append=F, na="NA")

	
#######################
# D. Organize - Plots #
#######################
	
### percent incidence
	## sum total for each treatment, block, date
	e4.summ.t = data.e4 %>% group_by(exp_rep, cultivar, graft, date, days_after_plant, rating, block, plot_num) %>% summarize(rating_sum=n()) %>% ungroup()
	
	## add symptomatic + dead plants together for sensical numbers (in other words, symptom incidence doesnt go down)
		# convert to wide
		e4.summ.t = e4.summ.t %>% spread(key=rating, value=rating_sum)
		
		# rename NA column
		e4.summ.t = e4.summ.t %>% rename("z_ct_na"=`<NA>`)
		
		# replace NA with 0
		e4.summ.t = e4.summ.t %>% mutate_at(vars(CT_D:z_ct_na), funs( replace(., is.na(.), 0) ) )
		
		# add symptomatic + dead; calculate total number of plants (does not include NAs)
		e4.summ.t = e4.summ.t %>% mutate(SB_T=SB_D+SB_S, CT_T=CT_D+CT_S, V_T=V_D+V_S, O_T=O_D+O_S, n_plants=H_H+SB_D+SB_S+CT_D+CT_S+V_D+V_S+O_D+O_S)
		
		# convert to integer
		e4.summ.t = e4.summ.t %>% mutate_at(vars(CT_D:n_plants), funs( as.integer(.) ))

	## split n_plants from rating sums
		e4.summ.rat.w = e4.summ.t %>% select(-n_plants)
		e4.summ.n.plant = e4.summ.t %>% select(exp_rep, cultivar, graft, date, days_after_plant, block, plot_num, n_plants)

	## convert ratings back to long
		e4.summ.rat = e4.summ.rat.w %>% gather(key="rating", value="rating_sum", -exp_rep, -cultivar, -graft, -date, -days_after_plant, -block, -plot_num)
		
	## calculate percent incidence for each plot based on number of plants at first rating date
		# filter n_plants for only first rating date; remove date columns
		e4.summ.n.plant = e4.summ.n.plant %>% filter(date %in% as_date(c("2018-05-11","2019-06-26"))) %>% select(-date, -days_after_plant)
		
		# join to rating count totals
		e4.summ.t2 = e4.summ.rat %>% left_join(e4.summ.n.plant, by=c("exp_rep" = "exp_rep", "cultivar" = "cultivar", "graft" = "graft", "block" = "block", "plot_num" = "plot_num"))

		# calculate percent incidence
		e4.summ.plot = e4.summ.t2 %>% mutate(perc_incid=round(((rating_sum/n_plants)*100), digits=1))

### export final data	
	## convert perc_incid to character so that NAs use "."; in this version of readr, NaN are ignored by the na option
		# convert column to character
		e4.summ.plot.export = e4.summ.plot %>% mutate(perc_incid=as.character(perc_incid))
	
		# replace NaN
		e4.summ.plot.export = e4.summ.plot.export %>% mutate(perc_incid=replace(perc_incid, perc_incid == "NaN", NA))

		# replace Inf for 5608-std-block2
		e4.summ.plot.export = e4.summ.plot.export %>% mutate(perc_incid=replace(perc_incid, perc_incid == "Inf", NA))		
		
		# export
		write_csv(e4.summ.plot.export, path="./2_data_curated/rolfsii_4_field_incidence_by-plot_final.csv", na=".", append=F, col_names=T)
		
	
#########################
# E. Summarize - Tables #
#########################

### examine to determine how to calculate mean
	# verify that number of plants in each plot is same across rating dates
#	e4.summ.plot %>% 	filter(rating == "SB_T") %>% 
#						select(-days_after_plant, -rating_sum, -perc_incid) %>% 
#						spread(key=date, value=n_plants) %>%
#						rowwise() %>% 
#						mutate(ct_mean_18=mean(c(`2018-05-11`,`2018-06-05`,`2018-06-15`,`2018-06-28`,`2018-07-13`,`2018-07-24`,`2018-08-10`), na.rm=T), ct_mean_19=mean(c(`2019-06-26`,`2019-07-10`,`2019-07-24`,`2019-08-08`), na.rm=T)) %>% 
#						arrange(ct_mean_18) %>% 
#						print(n=Inf)

### SB_T, CT_T - plots 
	# summarize
	e4.summ.plot.dates = e4.summ.plot %>% group_by(exp_rep, cultivar, graft, date, rating) %>% summarize(incid_min=min(perc_incid, na.rm=T), incid_max=max(perc_incid, na.rm=T)) %>% ungroup()
	
	# filter and arrange
	e4.summ.plot.dates = e4.summ.plot.dates %>% filter(rating %in% c("SB_T","CT_T")) %>% arrange(rating, exp_rep, date, cultivar, graft)
	
	# exclude last rating date
	e4.summ.plot.dates = e4.summ.plot.dates %>% filter(!date %in% as_date("2018-08-10"))
	
	# export
	write_csv(e4.summ.plot.dates, path="./4_results/rolfsii_4_field_incidence_summ-table_SB-CT_cultivar-graft_min-max.csv", na="NA", append=F, col_names=T)

### summarize by treatment, each date; ignore NA;
	# calculate overall mean by summing counts then calculating % in second step; mean of means is not accurate due to different sample sizes
	e4.summ.trt = e4.summ.plot %>% group_by(exp_rep, cultivar, graft, date, rating) %>% summarize(rating_total=sum(rating_sum), plants_sum=sum(n_plants))
	
	e4.summ.trt = e4.summ.trt %>% mutate(incid_avg=round( (rating_total / plants_sum)*100, digits=1)) %>% ungroup()

	## each rating date	
		# disease totals
			# spread
			e4.summ.trt.perc = e4.summ.trt %>% select(-rating_total, -plants_sum, -exp_rep) %>% spread(key=date, value=incid_avg)
		
			# look at southern blight, curly top, virus
			e4.summ.trt.perc = e4.summ.trt.perc %>% filter(rating %in% c("SB_T","CT_T","V_T")) %>% arrange(rating, cultivar, graft)

		# export
		write_csv(e4.summ.trt.perc, path="./4_results/rolfsii_4_field_incidence_summ-table_SB-CT-V_cultivar-graft_trt-dates.csv", na="NA", append=F, col_names=T)
	
### graft only - overall mean (FOR PAPER)
	# mean calculated by summing total number of plants affected and in plot across all blocks and cultivars
		# cannot do mean of means (mean of plot or treatment means) because sample size varies due to slightly different number of plants in each plot and some missing plots for some treatments
	# SB - d127; 2018, rating dates 1,2,7 removed; 2019, none removed
		# filter for desired ratings, dates
		e4.summ.sb.graft.d127 = e4.summ.plot %>% filter(rating == "SB_T") %>% filter(!date %in% as_date(c("2018-05-11","2018-06-05","2018-08-10")))

		# sum counts
		e4.summ.sb.graft.d127 = e4.summ.sb.graft.d127 %>% group_by(exp_rep, graft) %>% summarize(rating_total=sum(rating_sum), plants_sum=sum(n_plants), dates_ct=n_distinct(date))
		
		# calculate average, give counts as average over dates
		e4.summ.sb.graft.d127 = e4.summ.sb.graft.d127 %>% mutate(rating_avg_dates=round(rating_total/dates_ct, digits=1), plants_avg_dates=(plants_sum/dates_ct), incid_avg=round( (rating_total / plants_sum)*100, digits=1)) %>% ungroup()
	
	# CT - d17; 2018, rating dates 1,7 removed; 2019, none removed
		# filter for desired rating, dates
		e4.summ.ct.graft.d17 = e4.summ.plot %>% filter(rating == "CT_T") %>% filter(!date %in% as_date(c("2018-05-11","2018-08-10")))
		
		# summarize
		e4.summ.ct.graft.d17 = e4.summ.ct.graft.d17 %>% group_by(exp_rep, graft) %>% summarize(rating_total=sum(rating_sum), plants_sum=sum(n_plants), dates_ct=n_distinct(date))
		
		# calculate average
		e4.summ.ct.graft.d17 = e4.summ.ct.graft.d17 %>% mutate(rating_avg_dates=round(rating_total/dates_ct, digits=1), plants_avg_dates=(plants_sum/dates_ct), incid_avg=round( (rating_total / plants_sum)*100, digits=1)) %>% ungroup()
	
	## export
	write_csv(e4.summ.sb.graft.d127, path="./4_results/rolfsii_4_field_incidence_summ-table_SB_graft_d127_overall-mean.csv", na="NA", append=F, col_names=T)
	write_csv(e4.summ.ct.graft.d17, path="./4_results/rolfsii_4_field_incidence_summ-table_CT_graft_d17_overall-mean.csv", na="NA", append=F, col_names=T)
	

################
# F. Visualize #
################	

### all ratings; exclude NA "rating", "H_H"; exclude last rating date for 2018
	plot.e4.incid.perc.r1 = e4.summ.plot %>% filter(exp_rep == "2018" & !rating %in% c("H_H","z_ct_na") & date != as_date("2018-08-10")) %>% {
		ggplot(., aes(x=date, y=perc_incid, color=graft, linetype=cultivar, group=interaction(block, graft, cultivar))) +
			geom_line(size=0.3) +
			facet_wrap(~rating, ncol=3) +
			scale_y_continuous(limits=c(0,69)) +
			scale_x_date(limits=as_date(c("2018-05-11","2018-08-08")), date_breaks="1 month", date_labels="%b %d") +
			theme_bw() +
			theme(axis.text=element_text(size=12), legend.text=element_text(size=12)) +
			theme(legend.position="none") +
			theme(plot.margin=margin(5.5,0,5.5,5.5), panel.grid=element_blank(), panel.grid.major.y=element_line(color="light grey", size=0.15), panel.grid.major.x=element_line(color="light grey", size=0.15)) +
			labs(x="Date", y="Incidence (%)", color="Graft", linetype="Cultivar")
		}
	plot.e4.incid.perc.r2 = e4.summ.plot %>% filter(exp_rep == "2019" & !rating %in% c("H_H","z_ct_na")) %>% {
		ggplot(., aes(x=date, y=perc_incid, color=graft, linetype=cultivar, group=interaction(block, graft, cultivar))) +
			geom_line(size=0.3) +
			facet_wrap(~rating, ncol=3) +
			scale_y_continuous(limits=c(0,69)) +
			scale_x_date(limits=as_date(c("2019-05-11","2019-08-08")), date_breaks="1 month", date_labels="%b %d") +
			theme_bw() +
			theme(axis.text=element_text(size=12), legend.text=element_text(size=12)) +
			theme(axis.title.y=element_blank()) +
			theme(plot.margin=margin(5.5,5.5,5.5,0), panel.grid=element_blank(), panel.grid.major.y=element_line(color="light grey", size=0.15), panel.grid.major.x=element_line(color="light grey", size=0.15)) +
			labs(x="Date", color="Graft", linetype="Cultivar")
		}
	
	plot.e4.incid.perc = plot_grid(plot.e4.incid.perc.r1, plot.e4.incid.perc.r2, ncol=2, align="v", axis="lr")
	
	cowplot::ggsave(file="./4_results/rolfsii_4_field_incidence_all-ratings.png", device="png", plot=plot.e4.incid.perc, width=16, height=8, units="in")

### SB_T only - examine variation among blocks
	## group plots
		# create column
		e4.summ.plot.wet = e4.summ.plot %>% mutate(wet_rows=as.character(NA))
		
		# fill
		e4.summ.plot.wet = e4.summ.plot.wet %>% mutate(wet_rows=replace(wet_rows, exp_rep == "2019" & block %in% c(4,5,6) & plot_num %in% c(4,5,6), "Row Wet"))
		e4.summ.plot.wet = e4.summ.plot.wet %>% mutate(wet_rows=replace(wet_rows, exp_rep == "2018" | (exp_rep == "2019" & block %in% c(1,2,3) | (block %in% c(4,5,6) & plot_num %in% c(1,2,3))), "Row Dry"))		

	plot.e4.incid.perc.sbt.blk = e4.summ.plot.wet %>% filter(rating %in% c("SB_T","O_T") & date != as_date("2018-08-10")) %>% {
		ggplot(., aes(x=days_after_plant, y=perc_incid, color=graft, linetype=cultivar, group=interaction(block, cultivar, graft))) +
			geom_line(size=0.3) +
			facet_grid(exp_rep ~ rating + wet_rows) +
			theme_bw() +
			theme(axis.text=element_text(size=12), legend.text=element_text(size=12), strip.text=element_text(size=12)) +
			theme(legend.position="bottom") +
			theme(panel.grid=element_blank(), panel.grid.major.y=element_line(color="light grey", size=0.15), panel.grid.major.x=element_line(color="light grey", size=0.15)) +
			labs(x="Days After Planting", y="Incidence (%)", color="Graft", linetype="Cultivar")
		}

	ggplot2::ggsave(file="./4_results/rolfsii_4_field_incidence_sb_symp-plus-dead_only_wet-rows.png", device="png", plot=plot.e4.incid.perc.sbt.blk, width=8, height=6, units="in")

### SB_T (FOR PAPER)
	plot.e4.incid.sb = e4.summ.plot %>% filter(rating == "SB_T" & date != as_date("2018-08-10")) %>% {
		ggplot(., aes(x=days_after_plant, y=perc_incid, color=graft, linetype=graft, group=interaction(block, graft, cultivar))) +
			geom_line(size=0.3) +
			facet_grid(exp_rep ~ cultivar, labeller=labeller(cultivar=c("5608"="HZ 5608", "8504"="HZ 8504"))) +
			theme_bw() +
			theme(axis.text=element_text(size=12), legend.text=element_text(size=12), strip.text=element_text(size=12)) +
			theme(legend.position="bottom") +
			theme(panel.grid=element_blank(), panel.grid.major.y=element_line(color="light grey", size=0.15), panel.grid.major.x=element_line(color="light grey", size=0.15)) +
			labs(x="Days After Planting", y="Southern blight incidence (%)", color="Graft", linetype="Graft")
		}

	ggplot2::ggsave(file="./4_results/rolfsii_4_field_incidence_sb.png", device="png", plot=plot.e4.incid.sb, width=6, height=6, units="in")
	

### CT_T (FOR PAPER)
	plot.e4.incid.ct = e4.summ.plot %>% filter(rating == "CT_T" & date != as_date("2018-08-10")) %>% {
		ggplot(., aes(x=days_after_plant, y=perc_incid, color=graft, linetype=graft, group=interaction(block, graft, cultivar))) +
			geom_line(size=0.3) +
			facet_grid(exp_rep ~ cultivar, labeller=labeller(cultivar=c("5608"="HZ 5608", "8504"="HZ 8504"))) +
			theme_bw() +
			theme(axis.text=element_text(size=12), legend.text=element_text(size=12), strip.text=element_text(size=12)) +
			theme(legend.position="bottom") +
			theme(panel.grid=element_blank(), panel.grid.major.y=element_line(color="light grey", size=0.15), panel.grid.major.x=element_line(color="light grey", size=0.15)) +
			labs(x="Days After Planting", y="Curly Top incidence (%)", color="Graft", linetype="Graft")
		}

	ggplot2::ggsave(file="./4_results/rolfsii_4_field_incidence_ct.png", device="png", plot=plot.e4.incid.ct, width=6, height=6, units="in")
	
### O_T, V_T (FOR PAPER)
	plot.e4.incid.ov = e4.summ.plot %>% filter(rating %in% c("O_T","V_T") & date != as_date("2018-08-10")) %>% {
		ggplot(., aes(x=days_after_plant, y=perc_incid, color=graft, linetype=graft, group=interaction(block, graft, cultivar))) +
			geom_line(size=0.3) +
			facet_grid(exp_rep ~ rating + cultivar, labeller=labeller(rating=c(O_T="Other", V_T="Unknown Virus"), cultivar=c("5608"="HZ 5608", "8504"="HZ 8504"))) +
			theme_bw() +
			theme(axis.text=element_text(size=12), legend.text=element_text(size=12), strip.text=element_text(size=12)) +
			theme(legend.position="bottom") +
			theme(panel.grid=element_blank(), panel.grid.major.y=element_line(color="light grey", size=0.15), panel.grid.major.x=element_line(color="light grey", size=0.15)) +
			labs(x="Days After Planting", y="Incidence (%)", color="Graft", linetype="Graft")
		}

	ggplot2::ggsave(file="./4_results/rolfsii_4_field_incidence_oth-vir.png", device="png", plot=plot.e4.incid.ov, width=8, height=6, units="in")

