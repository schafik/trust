
#data merging

#reading in main data

cleaned_data <- read.csv("expiremental_data/data/in_process_data/cleaned_data.csv", 
                         stringsAsFactors=F)

#reading in deep data  
deep_time <- read.csv("expiremental_data/data/DEEP/time/wts_time_q20_R0_1_r.csv", stringsAsFactors=F) %>%
                dplyr::rename(beta = V1,
                              daily_dsic_rate = V2) %>% dplyr::select(-X)
deep_serials <- read.csv("data/DEEP/serials_time_1.csv", header = F) %>% dplyr::rename(serial = V1)
deep <- cbind(deep_serials, deep_time)
  remove(deep_time, deep_serials)

######Merge the serials_time_1.csv file with wts_time_q20_R0_1_r.csv
#merging data
full_data <- left_join(cleaned_data, deep, by = "serial")

#writing out
write.csv(full_data, "data/in_process_data/full_data.csv", row.names=F)

