##############################################
# PROCESSING TOMATO Southern blight grafting #
# Experiment 2: Field - 2017                 #
##############################################

## built on Docker rocker/tidyverse:4.2.0

if (!requireNamespace("conflicted", quietly=TRUE)) {
  install.packages("conflicted")
}
library(conflicted)

library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)
library(stringr)
library(tidyr)

devtools::install_version("gridExtra", version="2.3", repos="https://cran.r-project.org/", dependencies=c("Depends","Imports"), upgrade="never")
devtools::install_version("egg", version="0.4.5", repos="https://cran.r-project.org/", dependencies=c("Depends","Imports"), upgrade="never")
library(egg)

conflict_prefer("date", "lubridate")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("spread", "tidyr")

setwd("/home/tomato_graft_rolfsii")


##################################
# A. Import and Organize - Yield #
##################################

## get data
in.e2.yld = read_csv(file="./2_data/graft-rolfsii_LB Southern Blight Harvest Data 2017.csv", col_names=T)

## clean up df
	# rename columns
	e2.yld.t = in.e2.yld %>% rename(yield_lbs_plot=`Harvested Lbs`, block=Rep) 
	
	# add columns for treatment factors and fill in
	e2.yld.t = e2.yld.t %>% mutate(cultivar=as.character(NA), graft=as.character(NA))
	e2.yld.t = e2.yld.t %>% mutate(cultivar=replace(cultivar, Treatment %in% c("8504", "8504 on Maxifort"), "8504"))
	e2.yld.t = e2.yld.t %>% mutate(cultivar=replace(cultivar, Treatment %in% c("5608", "5608 on Maxifort"), "5608"))
	e2.yld.t = e2.yld.t %>% mutate(graft=replace(graft, Treatment %in% c("8504 on Maxifort","5608 on Maxifort"), "Maxi"))
	e2.yld.t = e2.yld.t %>% mutate(graft=replace(graft, Treatment %in% c("8504","5608"), "none"))
	
	# remove unneeded columns and blank row
	e2.yld.t = e2.yld.t %>% select(cultivar, graft, block, yield_lbs_plot) %>% filter(!is.na(cultivar) & !is.na(graft))
	# add combined factor for graphing and combined factor for labelling
	e2.yld.t = e2.yld.t %>% mutate(plant_factor=paste(cultivar, graft, sep="_"), plant_label=paste(cultivar, graft, sep="\n"))
	# order
	e2.yld = e2.yld.t %>% arrange(cultivar, graft, block)
	
## convert yield
	# to tons/acre; (x lbs / 1 plot) * (1 ton/2000 lbs) * (1 plot/3000 ft2) * (43560 ft2/1 acre); plot, 5 ft wide x 600 ft long
	e2.yld = e2.yld %>% mutate(yield_ton_ac=round(( (yield_lbs_plot/1)*(1/2000)*(1/3000)*(43560/1) ), digits=1))
	
	# to metric ton/hectare (metric ton = tonne = megagram)
	e2.yld = e2.yld %>% mutate(yield_mt_ha=round(( (yield_ton_ac/1)*(0.9072/1)*(1/0.4047) ), digits=1))
	
## calculate means and add combined plant label
	e2.yld.avg = e2.yld %>% group_by(graft) %>% summarize(yield_ton_mean=round(mean(yield_ton_ac), digits=1), yield_mt_mean=round(mean(yield_mt_ha), digits=1))

## export
	# remove labels
	e2.yld.exp = e2.yld %>% select(-plant_factor, -plant_label)

	# export		
	write_csv(e2.yld.exp, file="./2_data_curated/rolfsii_2_field-2017_yield_final.csv", na=".", col_names=T, append=F)


######################################
# B. Import and Organize - Incidence #
######################################

## incidence data
	# get data
	in.e2.incid = read_csv(file="./2_data/graft-rolfsii_LB grafted trial 2017.csv", col_names=T)
	# convert date column
	e2.incid.t = in.e2.incid %>% mutate(date=mdy(date_)) %>% select(-date_)
	# convert to long format
	e2.incid.t = e2.incid.t %>% gather(key="bed", value="disease_incidence", -date, -subplot)
	# convert bed to integer
	e2.incid.t = e2.incid.t %>% mutate(bed=as.integer(bed))
	# remove NAs
	e2.incid.t = e2.incid.t %>% filter(!is.na(date) & !is.na(disease_incidence))
	
## plot plan
	# import
	in.e2.map = read_csv(file="./1_methods/graft-rolfsii_LB grafted trial 2017_plot-map.csv", col_names=T)
	in.e2.blocks = read_csv(file="./1_methods/graft-rolfsii_LB grafted trial 2017_blocks.csv", col_names=T)

	# rename columns and remove NAs rows and columns
	e2.map.t = in.e2.map %>% filter(!is.na(half)) %>% select(-x1, -x2, -x3, -x4)
	# convert to long format and remove NAs
	e2.map.t = e2.map.t %>% gather(key="bed", value="trt_letter", -half) %>% filter(!is.na(trt_letter))
	# convert bed to integer
	e2.map.t = e2.map.t %>% mutate(bed=as.integer(bed))
	# create df of bed numbers
	e2.map.beds.half = c(1,1,1,1,2,2,2,2)
	e2.map.beds.subplot = c(1,2,3,4,5,6,7,8)
	e2.map.bed = data.frame('half'=as.integer(e2.map.beds.half), 'subplot'=as.integer(e2.map.beds.subplot))
	
## add treatment info
	e2.map.t = e2.map.t %>% mutate(
		cultivar=case_when(
			trt_letter %in% c("W","P") ~ "5608", 
			trt_letter %in% c("O", "Y") ~ "8504"), 
		graft=case_when(
			trt_letter %in% c("Y","W") ~ "Maxi", 
			trt_letter %in% c("O","P") ~ "none"))

	# add block info
	e2.map.t = e2.map.t %>% full_join(in.e2.blocks)

	# expand e2.map to subplots
	e2.map = e2.map.t %>% full_join(e2.map.bed, by=c("half" = "half"))
			
	# merge data with treatment info
	e2.incid.f = e2.incid.t %>% full_join(e2.map, by=c("bed" = "bed", "subplot" = "subplot"))

## homogenize subplots; subplots in block 1-4 were numbered 5-8, whereas subplots in block 5-7 were numbered 1-4
	e2.incid.f = e2.incid.f %>% mutate(subplot=replace(subplot, subplot == 5, 1))
	e2.incid.f = e2.incid.f %>% mutate(subplot=replace(subplot, subplot == 6, 2))
	e2.incid.f = e2.incid.f %>% mutate(subplot=replace(subplot, subplot == 7, 3))
	e2.incid.f = e2.incid.f %>% mutate(subplot=replace(subplot, subplot == 8, 4))

## add columns and reorganize
	# add days_after_plant column
	e2.incid.f = e2.incid.f %>% mutate(days_after_plant= as.integer( date - as_date("2017-05-15") ))

	# add number of plants in each plot for SAS analysis (assume same for all)
	e2.incid.f = e2.incid.f %>% mutate(n_plants=as.integer(50))

	# reorder and select columns
	e2.incid.f = e2.incid.f %>% select(cultivar, graft, block, subplot, date, days_after_plant, disease_incidence, n_plants)
	
## summarize to ensure no treatments have 0 incidence (all-zero problem; causes problems in analysis of binomial data in GLMM)
	e2.incid.f %>% group_by(cultivar, graft, date) %>% summarize(dis_incid_sum=sum(disease_incidence))		

## calculate perc_incid
	e2.incid.f = e2.incid.f %>% mutate(perc_incid=round( ((disease_incidence / n_plants)*100), digits=1))
	
## export curated data object
	write_csv(e2.incid.f, file="./2_data_curated/rolfsii_2_field-2017_disease-incidence_final.csv", na=".", col_names=T, append=F)


#########################
# C. Summarize - Tables #
#########################

### graft - rating date
	## summarize
	e2.summ.graft = e2.incid.f %>% group_by(graft, date) %>% summarize(incid_mean=round(mean(perc_incid, na.rm=T), digits=1))

	## spread
	e2.summ.graft = e2.summ.graft %>% spread(key=date, value=incid_mean)

	## export
	write_csv(e2.summ.graft, file="./4_results/rolfsii_2_field-2017_incidence_summ-table_SB_graft-date.csv", na="NA", col_names=T, append=F)

### subplots - rating date 
	# summarize
	e2.summ.subplot = e2.incid.f %>% group_by(cultivar, graft, date) %>% summarize(incid_min=min(perc_incid, na.rm=T), incid_max=max(perc_incid, na.rm=T)) %>% ungroup()
	
	# filter and arrange
	e2.summ.subplot = e2.summ.subplot %>% arrange(date, cultivar, graft)
	
	# export
	write_csv(e2.summ.subplot, file="./4_results/rolfsii_2_field-2017_incidence_summ-table_SB_cultivar-graft_min-max.csv", na="NA", append=F, col_names=T)

### graft-cultivar - rating date (for figure)
	## summarize
	e2.summ.incid.fig = e2.incid.f %>% group_by(cultivar, graft, date) %>% summarize(incid_mean=round(mean(perc_incid, na.rm=T), digits=1)) %>% ungroup()


########################
# D. Summarize - AUDPS #
########################

### load audps functions
	## AUDPS function copied from epifitter package as workaround to problem installing epifitter
		# compile error with one of dependencies of an epifitter dependency
	source("./3_analysis/epifitter_functions.r")

### calculate AUDPS
	## arrange and group by
	e2.incid.s = e2.incid.f %>% arrange(cultivar, graft, block, subplot, date) %>% group_by(cultivar, graft, block, subplot)

	## calculate
	e2.incid.s = e2.incid.s %>% summarize(raudps=AUDPS(y=perc_incid, time=days_after_plant, type="relative", y_proportion=FALSE)) %>% ungroup()
	
	## convert and round
	e2.incid.s = e2.incid.s %>% mutate(raudps=round((raudps * 100), digits=1))
	
### summarize graft-cultivar (for figure)
	## summarize
	e2.summ.audps.fig = e2.incid.s %>% group_by(cultivar, graft) %>% summarize(raudps_mean=round(mean(raudps, na.rm=T), digits=1)) %>% ungroup()

### export curated data object
	## convert to proportion
	e2.incid.s.out = e2.incid.s %>% mutate(raudps=raudps/100)

	write_csv(e2.incid.s.out, file="./2_data_curated/rolfsii_2_field-2017_disease-incidence_audps_final.csv", na=".", col_names=T, append=F)
	
	
###########
# D. Plot #
###########

### disease - incidence + audps combined
	## incidence - cultivar + graft
	plot.e2.incid = ggplot(e2.incid.f, aes(color=graft, linetype=graft)) +
		geom_line(aes(y=disease_incidence, x=date, group=interaction(block, subplot, graft)), size=0.3, alpha=0.5, position=position_jitter(w=0.2, h=0.2)) +
		geom_line(data=e2.summ.incid.fig, aes(x=date, y=incid_mean), size=1.75) +
		facet_grid(cultivar ~ .) +
		scale_color_discrete(labels=c("Maxifort","none")) + 
		scale_linetype_discrete(labels=c("Maxifort","none")) + 
		theme_bw() +
		theme(panel.grid=element_blank(), panel.grid.major.y=element_line(color="light grey", size=0.15), panel.grid.major.x=element_line(color="light grey", size=0.15)) +
		theme(axis.title=element_text(size=12), axis.text=element_text(size=10)) +
		theme(strip.text=element_blank(), strip.background=element_blank()) +
		theme(legend.position=c(0.25, 0.92), legend.background=element_rect(fill=NA) ) +
		guides(color=guide_legend(nrow=1), linetype=guide_legend(nrow=1)) +
		labs(y="Southern blight strikes (%)", x="Date", color="Graft", linetype="Graft")

	## audps - cultivar + graft
	plot.e2.audps = ggplot(e2.incid.s, aes(y=raudps, x=graft)) +
		geom_dotplot(binaxis="y", binwidth=2, dotsize=0.75, stackdir="center", stackratio=1.25) +
		geom_text(data=e2.summ.audps.fig, aes(x=graft, y=raudps_mean, label=raudps_mean), hjust=-0.75) +
		facet_grid(cultivar ~ ., labeller=labeller(cultivar=c("5608"="HZ 5608", "8504"="HZ 8504"))) +
		scale_x_discrete(labels=c("Maxifort","none")) + 
		stat_summary(fun=mean, fun.min=mean, fun.max=mean, geom="crossbar", size=0.3, width=0.4, color="red") +
		theme_bw() +
		theme(axis.title=element_text(size=12), axis.text=element_text(size=10)) +
		labs(y="Relative area under disease progress stairs (% strikes)", x="Graft")

	plot.e2.comb = ggarrange(plot.e2.incid, plot.e2.audps, widths=c(3.5,1.5))
	
	ggplot2::ggsave(file="./4_results/rolfsii_2_field-2017_disease_incid-audps.png", device="png", plot=plot.e2.comb, width=6.5, height=5, units="in")
	ggplot2::ggsave(file="./4_results/Solares_tomato-grafting_PlantDis_fig-2.tiff", device="tiff", plot=plot.e2.comb, width=6.5, height=5, units="in", dpi=500, compression="lzw")
	

### yield - graft (FOR PAPER)
	plot.e2.yld.dot = ggplot(e2.yld, aes(y=yield_mt_ha, x=graft)) +
		geom_dotplot(binaxis="y", binwidth=0.25, dotsize=2, stackdir="center") +
#		geom_line(aes(group=interaction(cultivar,block))) +
		geom_text(data=e2.yld.avg, aes(x=graft, y=yield_mt_mean, label=yield_mt_mean), hjust=-0.75) +
		scale_x_discrete(labels=c("Maxifort","none")) +
		stat_summary(fun=mean, fun.min=mean, fun.max=mean, geom="crossbar", size=0.25, width=0.25) +
		theme_bw() +
		theme(panel.grid=element_blank(), panel.grid.major.y=element_line(color="grey", size=0.2), legend.text=element_text(size=8)) +
		theme(axis.title.x=element_text(size=12), axis.title.y=element_text(size=12), axis.text.x=element_text(size=10), axis.text.y=element_text(size=10)) +
		labs(y="Yield (metric ton/hectare)", x="Graft")
	ggsave(file="./4_results/rolfsii_2_field-2017_yield_dot.png", device="png", plot=plot.e2.yld.dot, width=3, height=5, units="in")
	ggsave(file="./4_results/Solares_tomato-grafting_PlantDis_fig-3.tiff", device="tiff", plot=plot.e2.yld.dot, width=3, height=5, units="in", dpi=500, compression="lzw")

