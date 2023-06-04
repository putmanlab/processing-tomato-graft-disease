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

conflict_prefer("date", "lubridate")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("spread", "tidyr")

setwd("/home/tomato_graft_rolfsii/")


#####################
# A. Make Dataframe #
#####################

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


######################
# B. Calculate Lines #
######################

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


################
# C. Summarize #
################

### points
	## calculate graft-nongraft gap
		# remove unneeded columns
		summ.gap = df.pt %>% select(production, incid_nongraft, price, yield_base, treatment, cost_net)
		
		# spread
		summ.gap = summ.gap %>% spread(key=treatment, value=cost_net)
		
		# calculate
		summ.gap = summ.gap %>% mutate(diff_graft_nongraft=graft-nongraft)
		
		# remove NA rows
		summ.gap = summ.gap %>% filter(!is.na(nongraft))
		
	## show
	summ.gap %>% filter(
		(production == "conventional" & incid_nongraft == 40 & price == 152.08) |
		(production == "organic" & incid_nongraft == 40 & price == 181.83) 
		)

### lines
	## remove unneeded columns and sort
	summ.line = df.line %>% 
		select(production, incid_nongraft, price, break_yield, break_cost) %>%
		arrange(production, incid_nongraft, price)
	
	## show only yield below max yield evaluated
	summ.line %>% filter(break_yield <= 163.68) %>% print(n=Inf)
	
	## export
		# round
		summ.line.exp = summ.line %>% mutate(
			price=round(price, digits=2),
			break_yield=round(break_yield, digits=1),
			break_cost=round(break_cost, digits=0) )
			
	write_csv(summ.line.exp, file="./4_results/rolfsii_budget_breakeven-points.csv", col_names=T, append=F, na="NA")
	
	
##########################
# D. Visualize - Prepare #
##########################

### functions for strip labels
	lab.price = as_labeller(function(x) { paste("$", x, "/t", sep="") })
	lab.incid = as_labeller(function(x) { paste(x, "%", sep="") })
	
### get dataset for area fill
	## rename
	df.line.fill = df.line %>% rename(yield_break_graft=break_yield)

	## set boundaries - yield
	df.line.fill = df.line.fill %>% mutate(
		yield_min_graft=case_when(
			(yield_break_graft <   82.96)						    	~ 82.96,
			(yield_break_graft >=  82.96 & yield_break_graft <  109.87) ~ yield_break_graft,
			(yield_break_graft >= 109.87)						    	~ NA_real_),
		yield_avgmax_graft=case_when(
			(yield_break_graft <  109.87) 								~ 109.87,
			(yield_break_graft >= 109.87) 								~ NA_real_),
		yield_avgmin_graft=case_when(
			(yield_break_graft <  109.87) 								~ 109.87,
			(yield_break_graft >= 109.87 & yield_break_graft <= 163.68) ~ yield_break_graft,
			(yield_break_graft >  163.68) 								~ NA_real_),
		yield_max_graft=case_when(
			(yield_break_graft <  163.68) 								~ 163.68,
			(yield_break_graft >= 163.68) 								~ NA_real_) )

	## duplicate
	df.line.fill = df.line.fill %>% mutate(
		yield_break_nongraft=yield_break_graft, yield_min_nongraft=yield_min_graft, yield_avgmax_nongraft=yield_avgmax_graft, 
		yield_avgmin_nongraft=yield_avgmin_graft, yield_max_nongraft=yield_max_graft)
		
	## calculate cost
	df.line.fill = df.line.fill %>% mutate(
		across(
			c(yield_break_graft, yield_min_graft, yield_avgmax_graft, yield_avgmin_graft, yield_max_graft), 
			~ (( graft_m * .x) + graft_b),
			.names="cost_{col}"),
		across(
			c(yield_break_nongraft, yield_min_nongraft, yield_avgmax_nongraft, yield_avgmin_nongraft, yield_max_nongraft), 
			~ (( nongraft_m * .x) + nongraft_b),
			.names="cost_{col}") )

	## remove unneeded columns
	df.line.fill = df.line.fill %>% select(
		-graft_b, -graft_m, -nongraft_b, -nongraft_m,
		-break_cost,
		-yield_break_nongraft, -yield_break_graft, -cost_yield_break_nongraft, -cost_yield_break_graft)
			
	## gather
	df.line.fill = df.line.fill %>% gather(key="variable", value="value", -production, -price, -incid_nongraft)
	
	## remove "yield"
	df.line.fill = df.line.fill %>% mutate(variable=str_replace(variable, "_yield_", "_"))
	
	## separate
	df.line.fill = df.line.fill %>% separate(variable, into=c("variable","position","treatment"), sep="_", remove=T)
	
	## spread cost/yield (y axis)
	df.line.fill = df.line.fill %>% spread(key=variable, value=value)

	## filter
	df.line.fill = df.line.fill %>% filter(!(is.na(cost) & is.na(yield)))

	## spread break/max (x axis)
	df.line.fill = df.line.fill %>% spread(key=treatment, value=cost)
	
	## add fill vairable
	df.line.fill = df.line.fill %>% mutate(group=case_when(
		(position %in% c("min","avgmax")) ~ "< 109.9 t/ha",	
		(position %in% c("avgmin","max")) ~ ">= 109.9 t/ha") )	
	
	## remove column
#	df.line.fill = df.line.fill %>% select(-position)
	
	## find min/max
	df.line.fill = df.line.fill %>% rowwise() %>% mutate(cost_min=min(graft,nongraft), cost_max=max(graft,nongraft))
	
	## remove columns
	df.line.fill = df.line.fill %>% select(-graft, -nongraft)
	
	## round prices
	df.line.fill = df.line.fill %>% mutate(price=round(price, digits=0))


################
# E. Visualize #
################

### functions for strip labels
	lab.price = as_labeller(function(x) { paste("$", x, "/t", sep="") })
	lab.incid = as_labeller(function(x) { paste(x, "%", sep="") })

### individual facets
	plot.1.conv = df.pt %>% filter(production == "conventional") %>% mutate(price=round(price, digits=0)) %>% {
	ggplot(.) +
		geom_line(size=0.7, aes(x=yield_base, y=cost_net, linetype=treatment)) +
		geom_ribbon(data={ df.line.fill %>% filter(production == "conventional") }, aes(x=yield, ymin=cost_min, ymax=cost_max, fill=group), alpha=0.4) +
		facet_grid(incid_nongraft ~ price, labeller=labeller(price=lab.price, incid_nongraft=lab.incid)) +
		scale_x_continuous(breaks=c(93,123,153), minor_breaks=c(103,113,133,143)) +
		scale_y_continuous(breaks=c(-1000,-4000,-7000,-10000), minor_breaks=c(-2000,-3000,-5000,-6000,-8000,-9000)) +
		theme_bw() +
		theme(axis.title=element_text(size=12), axis.text=element_text(size=11), strip.text=element_text(size=12)) +
		theme(axis.title.x=element_text(margin=margin(7.5,0,-5,0)), axis.title.y=element_text(margin=margin(0,7.5,0,0))) +
		theme(legend.position="bottom", legend.box="vertical", legend.margin=margin(0,0,0,0), legend.text=element_text(size=11)) +
		guides(linetype=guide_legend(order=1), fill=guide_legend(order=0)) +
		labs(x="Yield (tonne/hectare [t/ha])", y="Net returns over analyzed costs ($/ha)", linetype="Transplants", fill="Returns higher in\ngrafted vs. nongrafted")
	}
	ggplot2::ggsave(file="./4_results/rolfsii_budget_facet_conv.png", device="png", plot=plot.1.conv, width=6.5, height=6.5, units="in")

	plot.1.org = df.pt %>% filter(production == "organic") %>% mutate(price=round(price, digits=0))  %>% {
	ggplot(.) +
		geom_line(size=0.7, aes(x=yield_base, y=cost_net, linetype=treatment)) +
		geom_ribbon(data={ df.line.fill %>% filter(production == "organic") }, aes(x=yield, ymin=cost_min, ymax=cost_max, fill=group), alpha=0.4) +
		facet_grid(incid_nongraft ~ price, labeller=labeller(price=lab.price, incid_nongraft=lab.incid)) +
		scale_x_continuous(breaks=c(93,123,153), minor_breaks=c(103,113,133,143)) +
		scale_y_continuous(breaks=c(-1000,-4000,-7000,-10000), minor_breaks=c(-2000,-3000,-5000,-6000,-8000,-9000,-11000,-12000)) +
		theme_bw() +
		theme(axis.title=element_text(size=12), axis.text=element_text(size=11), strip.text=element_text(size=12)) +
		theme(axis.title.x=element_text(margin=margin(7.5,0,-5,0)), axis.title.y=element_text(margin=margin(0,7.5,0,0))) +
		theme(legend.position="bottom", legend.box="vertical", legend.margin=margin(0,0,0,0), legend.text=element_text(size=11)) +
		guides(linetype=guide_legend(order=1), fill=guide_legend(order=0)) +
		labs(x="Yield (tonne/hectare [t/ha])", y="Net returns over analyzed costs ($/ha)", linetype="Transplants", fill="Returns higher in\ngrafted vs. nongrafted")
	}
	ggplot2::ggsave(file="./4_results/rolfsii_budget_facet_org.png", device="png", plot=plot.1.org, width=6.5, height=6.5, units="in")
		
	