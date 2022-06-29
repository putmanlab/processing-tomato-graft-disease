##############################################
# PROCESSING TOMATO Southern blight grafting #
# Experiment 3: Grafting - Greenhouse 		 #
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

#############
# A. Import #
#############

### 2018
	## import
	in.e3r1 = read_csv(file=paste(directory, "/2_data/graft-rolfsii_exp-2_DRating_10_edit.csv", sep=""), col_names=T)

	## organize	
		# remove unneeded rootstock ("graft") and notes columns
		in.e3r1 = in.e3r1 %>% select(-graft, -notes)
		
		# rename columns
		in.e3r1 = in.e3r1 %>% rename("exp_unit"=exp_unt, "graft"=trt, "disease_severity"=`disease score`)
		
		# reformat date column
		in.e3r1 = in.e3r1 %>% mutate(date=mdy(date))
		
		# change cultivar to character
		in.e3r1 = in.e3r1 %>% mutate(cultivar=as.character(cultivar))
	
		# reorder columns; remove unneeded plot, exp_unit columns
		in.e3r1 = in.e3r1 %>% select(cultivar, graft, inoculum, block, date, disease_severity)
		
	## curate
		# remove ratings (change to 0) for plants that were later found to be affected by a different disease or problem per notes in datasheet
			# 0 inoculum - 5608 - tall - block 6
			in.e3r1 = in.e3r1 %>% mutate(disease_severity=replace(disease_severity, inoculum == 0 & cultivar == "5608" & graft == "tall" & block == 6, 0))
	
			# 0 inoculum - 8504 - none - block 4
			in.e3r1 = in.e3r1 %>% mutate(disease_severity=replace(disease_severity, inoculum == 0 & cultivar == "8504" & graft == "none" & block == 4, 0))
		
		# 10 inoculum - 5608 - tall - block 5 - 2018-08-28: change to value of previous rating date, severity reverted to 0 for one rating date then returned to 6
		in.e3r1 = in.e3r1 %>% mutate(disease_severity=replace(disease_severity, inoculum == 10 & cultivar == "5608" & graft == "tall" & block == 5 & date == as_date("2018-08-28"), 4))
		
### 2019
	## import
	in.e3r2 = read_csv(file="./2_data/2019_graft_gh_rating8_8.14.19.csv", col_names=T, na="")
	
	## organize
		# remove notes column
		in.e3r2 = in.e3r2 %>% select(-notes)
		
		# rename columns
		in.e3r2 = in.e3r2 %>% rename("exp_unit"='pot ID', "disease_severity"='disease rating')
		
		# reformat date, cultivar columns
		in.e3r2 = in.e3r2 %>% mutate(date=mdy(date), cultivar=as.character(cultivar))
		
		# reorder columns 
		in.e3r2 = in.e3r2 %>% select(cultivar, graft, inoculum, block, date, disease_severity)

### bind
	## add identifier row
	in.e3r1 = in.e3r1 %>% mutate(exp_rep=as.character("2018"))
	in.e3r2 = in.e3r2 %>% mutate(exp_rep=as.character("2019"))
	
	## bind
	data.e3 = bind_rows(in.e3r1, in.e3r2)
	
### add days after planting column to provide time between ratings for spatial correlation structure for repeated measures in SAS; reorder columns
	## create column
	data.e3 = data.e3 %>% mutate(days_after_plant=as.integer(NA))
	
	## fill column
	data.e3 = data.e3 %>% mutate(days_after_plant=replace(days_after_plant, exp_rep == "2018", date[exp_rep == "2018"] - as_date("2018-04-30") ))
	data.e3 = data.e3 %>% mutate(days_after_plant=replace(days_after_plant, exp_rep == "2019", date[exp_rep == "2019"] - as_date("2019-05-23") ))
	
	## reorder columns
	data.e3 = data.e3 %>% select(exp_rep, cultivar, graft, inoculum, block, date, days_after_plant, disease_severity)
	
### export final data
	write_csv(data.e3, path="./2_data_curated/graft-rolfsii_3_graft-greenhouse_severity_final.csv", na="", append=F, col_names=T)


################
# B. Visualize #
################

### number of plants with each rating - stacked bar (FOR PAPER)
	plot.e3.bar.r1 = data.e3 %>% filter(exp_rep == "2018") %>% {
	ggplot(., aes(x=as.character(days_after_plant))) +
		geom_bar(aes(fill=as.character(disease_severity)), stat="count") +
		facet_grid(graft ~ cultivar + inoculum, labeller=labeller(cultivar=c("5608"="HZ 5608", "8504"="HZ 8504"))) +
		scale_x_discrete(limits=c("18","99","105","113","120","127","133","154")) +
		scale_y_continuous(limits=c(0,8)) +
		scale_fill_brewer(palette="YlOrRd") +
		theme_bw() +
		theme(axis.title.y=element_text(color=NA, margin=margin(0,5,0,0)), axis.text.y=element_text(size=10), strip.text=element_text(size=10)) +
		theme(axis.title.x=element_blank(), axis.text.x=element_text(size=9, angle=90, vjust=0.5), legend.position="none") +
		theme(aspect.ratio=0.6, plot.margin=margin(10,5.5,0,5.5), panel.grid=element_blank(), panel.grid.major.y=element_line(color="light grey", size=0.15), panel.grid.major.x=element_line(color="light grey", size=0.15)) +	
		labs(x="Days after transplant", y="Number of replicate plants", fill="Southern blight severity (0–7)")
	}
	
	plot.e3.bar.r2 = data.e3 %>% filter(exp_rep == "2019") %>% {
	ggplot(., aes(x=as.character(days_after_plant))) +
		geom_bar(aes(fill=as.character(disease_severity)), stat="count") +
		facet_grid(graft ~ cultivar + inoculum, labeller=labeller(cultivar=c("5608"="HZ 5608", "8504"="HZ 8504"))) +
		scale_x_discrete(limits=c("8","14","49","55","63","70","76","83")) +
		scale_fill_brewer(palette="YlOrRd") +
		theme_bw() +
		theme(axis.text.y=element_text(size=10), axis.text.x=element_text(size=9, angle=90, vjust=0.5), axis.title=element_text(size=11), strip.text.y=element_text(size=10)) +
		theme(axis.title.y=element_text(margin=margin(0,5,0,0), hjust=2.25), axis.title.x=element_text(margin=margin(5,0,0,0))) +
		theme(legend.text=element_text(size=11), legend.title=element_text(size=10), legend.position="bottom", legend.margin=margin(-5,0,0,0)) +
		theme(strip.text.x=element_blank()) +
		guides(fill=guide_legend(nrow=1)) +
		theme(aspect.ratio=0.6, plot.margin=margin(2.5,5.5,0,5.5), panel.grid=element_blank(), panel.grid.major.y=element_line(color="light grey", size=0.15), panel.grid.major.x=element_line(color="light grey", size=0.15)) +	
		labs(x="Days after transplant", y="Number of replicate plants", fill="Southern blight severity (0–7)")
	}

	plot.e3.bar = plot_grid(plot.e3.bar.r1, plot.e3.bar.r2, ncol=1, labels=c("2018","2019"), vjust=c(1.5,1), hjust=-1)
		
	cowplot::ggsave(file="./4_results/rolfsii_3_graft-gh_severity_bar.png", device="png", plot=plot.e3.bar, width=6.5, height=7.5, units="in")
	

##################
# C. Logger Data #
##################

### import
	## each
	in.gh.logger.18 = read_csv(file=paste(directory, "/2_data/greenhouse graft Hobo T&RH data 2018.csv", sep=""), col_names=T)
	in.gh.logger.19 = read_csv(file=paste(directory, "/2_data/greenhouse graft 2019 logger data.csv", sep=""), col_names=T)

	## bind
	in.gh.logger = bind_rows(in.gh.logger.18, in.gh.logger.19)
	
	## organize
		# rename columns
		in.gh.logger = in.gh.logger %>% rename(row_id=`#`, datetime=`Date Time, GMT -0700`, temp_instant=`Temp, °F`, temp_avg=`Temp - Avg, °F`, rh_instant=`RH, %`, rh_avg=`RH - Avg, %`, dewpt=`DewPt, °F`)
		
		# convert to datetime; NOTE: timezone is UTC-7
		in.gh.logger = in.gh.logger %>% mutate(datetime=ymd_hms(datetime))
				
		# remove unneeded columns
		in.gh.logger = in.gh.logger %>% select(datetime, temp_avg, rh_avg)

		# convert to C
		data.env = in.gh.logger %>% mutate(temp_avg=round(((temp_avg-32)*(5/9)), digits=1))
		
### remove data outside experiment/logging period; in 2019 logging was stopped exactly at end of trial
	# 2018 start date
	data.env = data.env %>% filter(date(datetime) >= as_date("2018-06-12"))
	
	# 2018 end date
	data.env = data.env %>% filter(date(datetime) <= as_date("2018-10-01") | date(datetime) >= as_date("2019-01-01"))
	
	# 2019 start date
	data.env = data.env %>% filter(date(datetime) < as_date("2019-01-01") | date(datetime) >= as_date("2019-06-04"))
	
### summarize
	## summarize by hour
	data.env.hr = data.env %>% group_by(datetime=ceiling_date(datetime, "hour")) %>% summarize(temp_hr=mean(temp_avg, na.rm=T), rh_hr=mean(rh_avg, na.rm=T))

	## mean overall
	data.env.hr %>% group_by(year=year(datetime)) %>% summarize(temp_mean_yr=mean(temp_hr, na.rm=T), rh_mean_yr=mean(rh_hr, na.rm=T))
	
	## daily min/max yearly summary
		# get daily stats
		summ.env.day = data.env.hr %>% 
			group_by(date=ceiling_date(datetime, "day")) %>% 
			summarize(
				temp_max=max(temp_hr, na.rm=T),
				temp_avg=mean(temp_hr, na.rm=T), 
				temp_min=min(temp_hr, na.rm=T),
				rh_max=max(rh_hr, na.rm=T),
				rh_avg=mean(rh_hr, na.rm=T),
				rh_min=min(rh_hr, na.rm=T) ) %>%
			ungroup()
		
		# gather
		summ.env.day = summ.env.day %>% gather(key="variable", value="value", -date)
		
		# calculate yearly summary
		summ.env.day.yr = summ.env.day %>% group_by(year=year(date), variable) %>% summarize(min_yr=min(value), avg_yr=mean(value), max_yr=max(value)) %>% ungroup()
		