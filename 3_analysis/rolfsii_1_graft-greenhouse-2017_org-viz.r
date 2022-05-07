##############################################
# PROCESSING TOMATO Southern blight grafting #
# Experiment 1: Greenhouse - 2017            #
##############################################

## built on Docker putmanlab/exploratory-analysis:0.1.1

library(ggplot2)
library(lubridate)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)

setwd("/home/tomato_graft_rolfsii")

######################
# A. Import and Plot #
######################

### import
	## get data
	in.e1r1.dis = read_csv(file="./2_data/graft-rolfsii_exp-1_DRating_6_2017-10-24.csv", col_names=T)

	## sort
	e1r1.dis.t = in.e1r1.dis %>% arrange(date, trt_let, inoculum)

	## change columns; cultivar to character from number; rename to disease_severity
	e1r1.dis.t = e1r1.dis.t %>% mutate(cultivar=as.character(cultivar), disease_severity=disease_score)

### add days after planting column to provide time between ratings for spatial correlation structure for repeated measures in SAS; reorder columns
	## create column
	e1r1.dis.t = e1r1.dis.t %>% mutate(days_after_plant = as.integer( date - as_date("2017-06-05")) )

	## reorder columns
	e1r1.dis = e1r1.dis.t %>% select(cultivar, graft, inoculum, block, date, days_after_plant, disease_severity)

### export final QC'd data	
	write_csv(e1r1.dis, path="./2_data_curated/rolfsii_1_graft-greenhouse-2017_disease-serverity_final.csv", na=".", col_names=T, append=F)


###########
# B. Plot #
###########

### graph raw data
	## number of plants with each rating - stacked bar
	plot.severity.stack = e1r1.dis %>% filter(!is.na(disease_severity)) %>% {
	ggplot(., aes(x=as.character(days_after_plant))) +
		geom_bar(aes(fill=as.character(disease_severity)), stat="count") +
		facet_grid(inoculum ~ cultivar + graft, labeller=labeller(cultivar=c("5608"="HZ 5608", "8504"="HZ 8504"), graft=c("Maxi"="Maxifort", "none"="none"))) +
		scale_x_discrete(limits=c("18","37","51","67","79","101")) +
		scale_fill_brewer(palette="YlOrRd") +
		theme_bw() +
		theme(panel.grid=element_blank(), panel.grid.major.y=element_line(color="light grey", size=0.15), panel.grid.major.x=element_line(color="light grey", size=0.15)) +
		theme(axis.title=element_text(size=14), axis.text=element_text(size=11), strip.text=element_text(size=14)) +
		theme(legend.position="bottom", legend.title=element_text(size=14), legend.text=element_text(size=12)) +
		guides(fill=guide_legend(nrow=1)) +
		labs(x="Days after transplant", y="Number of replicate plants", fill="Southern blight severity (0–8)")
	}
	ggplot2::ggsave(file="./4_results/rolfsii_1_graft-gh-2017_severity_bar.png", device="png", plot=plot.severity.stack, width=6.5, height=6.5, units="in")