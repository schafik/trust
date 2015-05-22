
###############################################################
#Gathering Raw Data for All Countries#
###############################################################

#loading useful functions/packages#
source("WVS_data/scripts/load_libraries.R")
source("WVS_data/scripts/all_countries/0_utility_functions.R")

#loading country codes#
codes <- read_delim("WVS_data/data/raw_data/country_codes.csv", 
                    delim = "##", col_names=F) %>% 
          select(country_code = X1, country = X3)

#######################################################################################
#Wave 2#
#######################################################################################
# http://www.worldvaluessurvey.org/WVSDocumentationWV2.jsp  (1990-1994) 
#V18 - feeling of happiness
#V83 - state of health (subjective)
#V96 - satisfaction with your life
#V94 - most people can be trusted 
#V132 - satisfaction with financial situation of household
#v347_32 - how much you trust: Your neighborhood
#V363 - scale of incomes
#V375 - highest educational level attained
#V375CS - education (country specific)

w2 <- readRDS("WVS_data/data/raw_data/wave2_raw.RDS") %>%
  select(wave = V1, country_code = V2, happy = V18,
         health = V83, life = V96, gTrust = V94, 
         finance = V132, nTrust = V347_32, income = V363, 
         edu = V375, edu_c = V375CS) 
      w2 <- left_join(w2, codes, by="country_code") %>% select(-country_code)
      w2 <- smartNAs(w2)
  
  #writing out
  saveRDS(w2, "WVS_data/data/in_process_data/wave_2_gathered.RDS")
  write_csv(w2, "WVS_data/data/in_process_data/wave_2_gathered.csv")

#######################################################################################
###Wave 5##############################################################################
#######################################################################################
# http://www.worldvaluessurvey.org/WVSDocumentationWV5.jsp  (2005-2009)
#V10 - feeling of happiness
#V11 - state of health (subjective)
#V22 - satisfaction with your life
#V23 - most people can be trusted 
#V68 - satisfaction with financial situation of household
#v126 - how much you trust: Your neighborhood
#V253 - scale of incomes
#V238 - highest educational level attained
#V238CS - education (country specific)

w5 <- readRDS("WVS_data/data/raw_data/wave5_raw.RDS") %>%
  select(wave = V1, country_code = V2, happy = V10,
         health = V11, life = V22, gTrust = V23, 
         finance = V68, nTrust = V126, 
         income = V253, edu = V238, edu_c = V238CS) 
      w5 <- left_join(w5, codes, by="country_code") %>% select(-country_code)
      w5 <- smartNAs(w5)

  #writing out
  saveRDS(w5, "WVS_data/data/in_process_data/wave_5_gathered.RDS")
  write_csv(w5, "WVS_data/data/in_process_data/wave_5_gathered.csv")

#######################################################################################
###Wave 6##############################################################################
#######################################################################################
# http://www.worldvaluessurvey.org/WVSDocumentationWV6.jsp  (2010-2014)
#V10 - feeling of happiness
#V11 - state of health (subjective)
#V23 - satisfaction with your life
#V24 - most people can be trusted 
#V59 - satisfaction with financial situation of household
#v103 - how much you trust: Your neighborhood
#v170 - secure in neighborhood
#V239 - scale of incomes
#V248 - highest educational level attained
#V248_CS - education (country specific)

w6 <- readRDS("WVS_data/data/raw_data/wave6_raw.RDS") %>%
        select(wave = V1, country_code = V2, happy = V10,
               health = V11, life = V23, gTrust = V24, 
               finance = V59, nTrust = V103, security = V170, 
               income = V239, edu = V248, edu_c = V248_CS) 
        w6 <- left_join(w6, codes, by="country_code") %>% select(-country_code)
        w6 <- smartNAs(w6)

  #writing out
  saveRDS(w6, "WVS_data/data/in_process_data/wave_6_gathered.RDS")
  write_csv(w6, "WVS_data/data/in_process_data/wave_6_gathered.csv")

