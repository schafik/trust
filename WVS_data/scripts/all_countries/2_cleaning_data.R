
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

#doing some cleaning on income#
full_data <- full_data %>%
              mutate(income = as.numeric(as.character(income)),
                     lowIncome_4 = ifelse(income <= 4, T, F),
                     lowIncome_3 = ifelse(income <= 3, T, F),
                     lowIncome_2 = ifelse(income <= 2, T, F),
                     lowIncome_1 = ifelse(income <= 1, T, F))

#Merge the continent##########################################################
country_list_UN <- read_csv("WVS_data/data/raw_data/country_list_UN.csv") %>%
            select(country_code = Countrycode_UN, continent = Continent, region = Region)

full_data <- left_join(full_data, country_list_UN, by = "country_code") %>% 
              select(-country_code) %>%
              select(wave, country, continent:region, happy:lowIncome_1)

#writing out data#############################################################
  saveRDS(full_data, "WVS_data/data/in_process_data/fulldata_cleaned.RDS")
    write_csv(full_data, "WVS_data/data/in_process_data/fulldata_cleaned.csv")
  
