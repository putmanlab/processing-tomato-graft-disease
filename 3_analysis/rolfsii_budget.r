##############################################
# PROCESSING TOMATO Southern blight grafting #
# Partial Budget Analysis			         #
##############################################

## built on Docker rocker/tidyverse:4.2.0

if (!require(conflicted)) {
  install.packages("conflicted")
}
library(conflicted)

library(dplyr)
library(ggplot2)
library(forcats)
library(readr)
library(stringr)
library(tidyr)
library(RColorBrewer)


# install ggpattern for bar fill patterns
	# does not work due to missing library for dependency package 'units'
#devtools::install_version("ggpattern", version="0.4.1", repos="https://cran.r-project.org/", dependencies=c("Depends","Imports"), upgrade="never")
#library(ggpattern)

conflict_prefer("date", "lubridate")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("spread", "tidyr")

setwd("/home/tomato_graft_rolfsii/")


##########################
# A. Make Dataframe #
##########################
### make dataframe
	## set vectors
	production = c("conventional","organic")
#	yield_base = seq(from=37, to=73, by=4) # short ton/acre
	yield_base = c(82.96, 91.93, 100.9, 109.87, 118.84, 127.81, 136.78, 145.75, 154.72, 163.68) # tonne/hectare
	treatment = c("graft","nongraft")
	incid_nongraft = c(0,5,10,20,20.3,30,40)
#	price = c(128,133,138,143,148,153) # $/short ton
	price = c(141.06, 146.57, 152.08, 157.59, 163.1, 168.61) # $/short ton

	## make df
	df.pt = expand_grid(production, yield_base, treatment, incid_nongraft, price)
	
	## add graft incid
	df.incid = tibble(
		incid_nongraft = c(0,5,10,20,20.3,30,40),
		incid_graft = c(0,0.5,1,2,2.7,3,4) )
	
	df.pt = df.pt %>% left_join(df.incid, by=c("incid_nongraft" = "incid_nongraft"))
	
	## adjust organic prices
	df.pt = df.pt %>% mutate(price=replace(price, production == "organic", price[production == "organic"] + 29.75))
	
### add cost and return changes
	## add: added returns, reduced costs, added costs
	df.pt = df.pt %>% mutate(
		yield_add = case_when(
			(treatment == "graft") ~ round(yield_base*0.145, digits=1),
			(treatment == "nongraft") ~ 0),
		cost_reduce = case_when(
			(treatment == "graft") ~ 893,
			(treatment == "nongraft") ~ 0),
		cost_add = case_when(
			(treatment == "graft") ~ -7140,
			(treatment == "nongraft") ~ 0) )
			
	## add reduced yield
	df.pt = df.pt %>% mutate(yield_loss = case_when(
			(treatment == "nongraft") 	~ round( ( (yield_base + yield_add) * (-incid_nongraft/100) ), digits=1),
			(treatment == "graft") 		~ round( ( (yield_base + yield_add) * (-incid_graft/100	 ) ), digits=1) ) )
			
	## calculate returns
	df.pt = df.pt %>% mutate(
		return_add = round(yield_add * price, digits=0),
		return_reduce = round(yield_loss * price, digits=0),
		cost_reduce_harvest = round(yield_loss * 7.71 * -1, digits=0),
		cost_add_harvest = round(yield_add * 7.71 * -1, digits=0) )

### sum cost changes
	df.pt = df.pt %>% mutate(cost_net = return_add + cost_reduce + cost_reduce_harvest + cost_add + cost_add_harvest + return_reduce)

### check against paper example
df.pt %>% filter(yield_base == 109.87 & price == 152.08 & (
	(treatment == "graft" & incid_nongraft == 0) |
	(treatment == "nongraft" & incid_nongraft == 20.3) |
	(treatment == "graft" & incid_graft == 2.7) ) )

### clean up
	## remove static scenario
	df.pt = df.pt %>% filter(!(treatment == "nongraft" & incid_nongraft == 0))
	
	## remove paper example
	df.pt = df.pt %>% filter(!(incid_nongraft == 20.3))
	
### calculate line equations
	## remove unneeded columns
	df.line = df.pt %>% 
		select(production, price, incid_nongraft, treatment, yield_base, cost_net) %>% 
		arrange(production, price, incid_nongraft, treatment, yield_base)
	
	## filter for end points
	df.line = df.line %>% filter(yield_base %in% c(82.96, 163.68))
	
	## collapse to one line
	df.line = df.line %>% 
		mutate(yield_base=paste("tonne_", round(yield_base, digits=0), sep="")) %>%
		spread(key=yield_base, value=cost_net)
	
	## calculate
		# slope
		df.line = df.line %>% mutate(slope=((tonne_164-tonne_83)/(163.68-82.96)))
		
		# intercept
		df.line = df.line %>% mutate(intercept=(tonne_83-(slope*82.96)))
		
### calculate intersection
	## remove unneeded columns
	df.line = df.line %>% select(-tonne_83, -tonne_164)
	
	## rename columns
	df.line = df.line %>% rename(m=slope, b=intercept)
	
	## gather
	df.line = df.line %>% gather(key="variable", value="value", -production, -price, -incid_nongraft, -treatment)
	
	## unite
	df.line = df.line %>% unite("linevar", c(treatment, variable), remove=T)
	
	## spread
	df.line = df.line %>% spread(key=linevar, value=value)
	
	## calculate
		# x
		df.line = df.line %>% mutate(break_yield=((nongraft_b-graft_b)/(graft_m-nongraft_m)))
	
		# y
		df.line = df.line %>% mutate(break_cost = (graft_m * break_yield) + graft_b )

### summarize
	df.line %>% filter(break_yield < 163.68) %>% arrange(production, incid_nongraft, price) %>% print(n=Inf)
	
################
# B. Visualize #
################

### functions for strip labels
	lab.price = as_labeller(function(x) { paste("$", x, "/t", sep="") })
	lab.incid = as_labeller(function(x) { paste(x, "%", sep="") })

### individual facets
	plot.1.conv = df.pt %>% filter(production == "conventional") %>% mutate(price=round(price, digits=0)) %>% {
	ggplot(., aes(x=yield_base, y=cost_net, linetype=treatment)) +
		geom_line(size=0.5) +
		facet_grid(incid_nongraft ~ price, labeller=labeller(price=lab.price, incid_nongraft=lab.incid)) +
		scale_x_continuous(breaks=c(93,123,153), minor_breaks=c(103,113,133,143)) +
		theme_bw() +
		theme(axis.title=element_text(size=12), axis.text=element_text(size=10), strip.text=element_text(size=12)) +
		theme(axis.title.x=element_text(margin=margin(7.5,0,-5,0)), axis.title.y=element_text(margin=margin(0,7.5,0,0))) +
		theme(legend.position="bottom", legend.margin=margin(0,0,0,0), legend.text=element_text(size=11)) +
		labs(x="Yield (tonnes [t]/hectare)", y="Net returns over analyzed costs ($)", linetype="Transplants")
	}
	ggplot2::ggsave(file="./4_results/rolfsii_budget_facet_conv.png", device="png", plot=plot.1.conv, width=6.5, height=6.5, units="in")

	plot.1.org = df.pt %>% filter(production == "organic") %>% mutate(price=round(price, digits=0))  %>% {
	ggplot(., aes(x=yield_base, y=cost_net, linetype=treatment)) +
		geom_line(size=0.5) +
		facet_grid(incid_nongraft ~ price, labeller=labeller(price=lab.price, incid_nongraft=lab.incid)) +
		scale_x_continuous(breaks=c(93,123,153), minor_breaks=c(103,113,133,143)) +
		theme_bw() +
		theme(axis.title=element_text(size=12), axis.text=element_text(size=10), strip.text=element_text(size=12)) +
		theme(axis.title.x=element_text(margin=margin(7.5,0,-5,0)), axis.title.y=element_text(margin=margin(0,7.5,0,0))) +
		theme(legend.position="bottom", legend.margin=margin(0,0,0,0)) +
		labs(x="Yield (tonnes [t]/hectare)", y="Net returns over analyzed costs ($)", linetype="Transplants")
	}
	ggplot2::ggsave(file="./4_results/rolfsii_budget_facet_org.png", device="png", plot=plot.1.org, width=6.5, height=6.5, units="in")
		
	