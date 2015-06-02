
###############################################################
#Clean/Merge Data and labeling by income#
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

  #writing out data#
  saveRDS(w2_clean, "WVS_data/data/in_process_data/wave_2_cleaned.RDS")
    write_csv(w2_clean, "WVS_data/data/in_process_data/wave_2_cleaned.csv")
  saveRDS(w5_clean, "WVS_data/data/in_process_data/wave_5_cleaned.RDS")
    write_csv(w5_clean, "WVS_data/data/in_process_data/wave_5_cleaned.csv")
  saveRDS(w6_clean, "WVS_data/data/in_process_data/wave_6_cleaned.RDS")
    write_csv(w6_clean, "WVS_data/data/in_process_data/wave_6_cleaned.csv")
  
#merging together into one data set#
full_data <- rbind(w2_clean, w5_clean)
full_data <- rbind(full_data, select(w6_clean, -security))
    remove(w2, w2_clean, w5, w5_clean, w6, w6_clean) #emptying workspace a bit

#dividing by income#
full_data <- full_data %>%
              mutate(lowIncome_4 = ifelse(income %in% c(1:4), T, F),
                     lowIncome_3 = ifelse(income %in% c(1:3), T, F),
                     lowIncome_2 = ifelse(income %in% c(1:2), T, F))

#writing out data#
  saveRDS(full_data, "WVS_data/data/in_process_data/fulldata_cleaned.RDS")
    write_csv(full_data, "WVS_data/data/in_process_data/fulldata_cleaned.csv")

  
#TO DO: 
#continent variable!
  #create csv that maps to country csv for continent 

#ensure that we have data divided by:
  #continent 

###############################################################
# Merge the continent
###############################################################
    country_list_UN <- read_csv("WVS_data/data/raw_data/country_list_UN.csv")
    merged <- left_join(codes, country_list_UN, by = c("country" = "Name"))
    # merged <- merge(codes, country_list_UN, by.x="country", by.y="Name", all.x=T)
    codes2 <- merged[c("country", "country_code","Continent", "Region")] #TO DO: write out file so we can save
    
    