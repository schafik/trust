
#data cleaning

cleaned_data <- raw_data

#COLUMN CLEANING#
#getting rid of annoying names exported in LS
  names(cleaned_data) <- str_replace(names(cleaned_data), "s\\d{6}\\.q\\d{4}\\.", "")
  names(cleaned_data) <- str_replace(names(cleaned_data), "s.\\d{6}\\.", "")
  names(cleaned_data) <- str_replace(names(cleaned_data), "f.s\\d{6}\\.q\\d{4}\\.", "")
  names(cleaned_data) <- str_replace(names(cleaned_data), "s\\d{6}\\.q\\d{4}\\.po", "")
  
  names(cleaned_data) <- str_replace_all(names(cleaned_data), "\\W", "_") #periods
  names(cleaned_data) <- tolower(names(cleaned_data)) #lowercase
  names(cleaned_data) <- str_replace(names(cleaned_data), "_scend{1}", "") #easy vs. hard
  
#getting rid of time data
  cleaned_data <- cleaned_data[, unique(colnames(cleaned_data))] %>%
                      select(-c(datetime, submit:length))

#getting rid of duplicated columns
  cleaned_data <- cleaned_data %>% 
            select(-c(f_household_size, education, f_month_of_birth, income,
                      marital, current_employment, current_occupation, children,
                      race, political, sex, f_macarth, f_scen4_1, f_scen4_1, f_hard_scen4_1)) %>%
            rename(income = f_income, education = f_education, marital = f_marital,
                   current_employment = f_current_employment, children = f_children, 
                   current_occupation = f_current_occupation, political = f_political,
                   race = f_race, sex = f_sex) %>%

#Variable Cleaning#
          mutate(children = str_replace(children, "5 or more", "5_or_more"),
                 race = str_replace(race, "Black/African descent", "Black"),
                 race = str_replace(race, "Caucasian/White", "Caucasian"),
                 race = str_replace(race, "Latino/Hispanic", "Latino"),
                 race = str_replace_all(race, " ", "_"),
                 marital = str_replace_all(marital, " ", "_"),
                 age = ifelse(age < 1948, NA, 2015 - age))


# 1 not at all 5 = completely

#ROW CLEANING#
cleaned_data <- dplyr::filter(cleaned_data, 
                              consent_1 == "Y", #clean non-consent
                              serial > 132340, #clean for testing data
                              !is.na(path))      #clean NA paths
                              
#WRITING OUT DATA#
write.csv(cleaned_data, "data/in_process_data/cleaned_data.csv", row.names=F)

