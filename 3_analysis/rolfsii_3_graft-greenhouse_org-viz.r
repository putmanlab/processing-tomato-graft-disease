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
		