
###############################################################
#Cleaning Data and Crossing With Income#
###############################################################

#necessary packages and functions#
source("WVS_data/scripts/load_libraries.R")
source("WVS_data/scripts/all_countries/0_utility_functions.R")

#reading in data#
w2 <- readRDS("WVS_data/data/in_process_data/wave_2_gathered.RDS") 
w5 <- readRDS("WVS_data/data/in_process_data/wave_5_gathered.RDS")
w6 <- readRDS("WVS_data/data/in_process_data/wave_6_gathered.RDS")

#clarifying raw data responses#
w2_clean <- clarify(w2)
w5_clean <- clarify(w5)
w6_clean <- clarify(w6)

#######################################################################################
#income crossing
sAfrica <- filter(yo, country == "South Africa")
crossin <- sAfrica %>% select(happy, income, country) %>%
            mutate(low_income = ifelse(income %in% c(1:3), T, F)) %>%            
            group_by(low_income, happy) %>% summarize(happy_income = n()) 
              
#get relevant variables crossed with income for every country

#######################################################################################

#TO DO: 
  #wave 2
    #get relevant variables crossed with income for every country
  #wave 5
    #get relevant variables for every country
    #get relevant variables crossed with income for every country
  #wave 6
    #get relevant variables for every country
    #get relevant variables crossed with income for every country
  #continent variable!
    #create csv that maps to country csv for continent 
  #3 waves => we want to have an average score for each variable and collapse into one data set

  #ensure that we have data divided by:
    #continent 
    #low vs high income

