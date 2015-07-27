#accessing data

#social trust study 
#data not including deep
# 916854  consent
# 597418	Easy Scen 1
# 353562	Easy Scen 2 
# 835338	Easy Scen 3
# 678612	Easy Scen 4
# 873281	Hard Scen 1
# 576721	Hard Scen 2
# 196727	Hard Scen 3
# 292542	Hard Scen 4
# 799779	Easy SC 1
# 233454	Hard SC 1
# 177542	Subjective SES
# 488174	Brazilian ST
# 134398	Objective SES
# 458795	Demographics

# library(decider)
library(RMySQL)
library(stringr)
library(testthat)
library(dplyr)

# https://vlab.decisionsciences.columbia.edu/act_exp/Mar15_20/startturk.php?workerID=testing123
# raw_data <- get_hoover(vers = 2, exp.list = c(552),
#             sid.list = c(916854, 597418, 353562, 835338, 
#                          678612, 873281, 576721, 196727, 
#                          292542, 799779, 233454, 177542,
#                          488174, 134398, 458795))
#no need to do a fresh pull since the wave is finished running!            

raw_data <- read.csv("expiremental_data/data/raw_data/raw_data.csv", stringsAsFactors=F)



