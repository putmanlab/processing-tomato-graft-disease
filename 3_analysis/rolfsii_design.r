##############################################
# PROCESSING TOMATO Southern blight grafting #
# Experimental Design                        #
##############################################

## built on Docker putmanlab/field-exp-analysis-docker

library(agricolae)
library(dplyr)
library(tidyr)
library(readr)

directory="/home/tomato_graft_rolfsii"
setwd(directory)

#####################################
# A. Exp. 1 - Graft Greenhouse 2017 #
#####################################

### set up trial parameters
	## treatments
	cult.e1 = c("5608","8504")
	graft.e1 = c("none","Maxi")
	inoc.e1 = c(0,5,10,20)
	reps.e1 = 8
	
	## expand factor levels into factorial combinations
	trts.t.e1 = expand.grid('cultivar'=cult.e1, 'graft'=graft.e1, 'inoculum'=inoc.e1)
	
	## reorder based on Natalie letters
	trts.e1 = trts.t.e1[order(rev(trts.t.e1$cultivar), rev(trts.t.e1$graft)),]
	
	## convert row names into dataframe column
	#trts.e1 = data.frame('trt'=row.names(trts.e1), trts.e1)
	
	## add treatment number and letter columns
	trts.e1 = data.frame('trt_let'=NA, trts.e1, 'trt'=NA)
	trts.e1$trt_let = LETTERS[seq(from=1, to=16)]
	trts.e1$trt = c(1:16)
 
	## calculate number of units/pots
	units.e1 = reps.e1 * length(trts.e1$trt.e1)

### Run 1 
	## set random number seed *** !!! USE DIFFERENT SEED EACH TIME !!! ***
	## obtained from blind selection of random number in "A Million Random Digits With 100,000 Normal Deviates" by RAND Corporation
	seed.e1.r1 = 16757

	## randomize
	des.e1.r1 = design.rcbd(trt=trts.e1$trt, r=reps.e1, seed=seed.e1.r1, kinds="Super-Duper", first=TRUE, continue=FALSE)

	## change name of treatment number column
	names(des.e1.r1$book)[names(des.e1.r1$book)=="trts.e1$trt"] = "trt"

	## add treatment info to treatment number and randomizations
	des.out.e1.r1 = data.frame(merge(des.e1.r1$book, trts.e1, by.x='trt', by.y='trt'))
	des.out.e1.r1 = des.out.e1.r1[order(des.out.e1.r1$trt, des.out.e1.r1$plots),]
	
	## copy experimental unit number from row names to column in dataframe
	des.out1.e1.r1 = data.frame('exp_unit'=row.names(des.out.e1.r1), des.out.e1.r1)

	## same seed = same randomization, every time; append seed and datetime to filename to prevent accidentally overwriting randomization
	write.table(des.out1.e1.r1,file=paste("graft-rolfsii_exp-1_run-1_randomized_", "seed-", seed.e1.r1, "_", format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), ".txt", sep=""),row.names=F,col.names=T,quote=F,sep="\t")
	
################################
# B. Exp. 6 - Graft Field 2019 #
################################

### set up trial parameters
	## treatments	
	cultivar.e6 = c("5608","8504")
	graft.e6 = c("none","standard","tall")
	reps.e6 = 6
	
	## expand factor levels into factorial treatment combinations
	trt.e6 = crossing('cultivar'=cultivar.e6, 'graft'=graft.e6)
	
	## add treatment number
	trt.e6 = trt.e6 %>% mutate(treatment=1:6)

### randomization
	## set random number seed *** !!! USE DIFFERENT SEED EACH TIME !!! ***
	## obtained from blind selection of random number in "A Million Random Digits With 100,000 Normal Deviates" by RAND Corporation
	seed.e6 = 418
	
	## get randomization
	des.e6 = design.rcbd(trt=trt.e6$treatment, r=reps.e6, seed=seed.e6, kinds="Super-Duper", first=TRUE, continue=FALSE)
	
	## change column name
	des.e6.out = des.e6$book %>% as_tibble() %>% rename("treatment"=`trt.e6$treatment`) %>% mutate(treatment=as.integer(treatment))
	
	## rejoin with factorial treatment info
	des.e6.out = des.e6.out %>% left_join(trt.e6, by=c("treatment" = "treatment"))
	
	## same seed = same randomization, every time; append seed and datetime to filename to prevent accidentally overwriting randomization
	write_delim(des.e6.out, path=paste("graft-rolfsii_6_graft-field-2019_randomized_", "seed-", seed.e6, "_", format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), ".txt", sep=""), col_names=T, append=F, delim="\t")
