#####################################
#transforming raw data#
#####################################
library(readr)
library(readxl)
library(reshape2)
library(dplyr)
library(stringr)
library(ggplot2)

#STEP 0: make sure to:
  #setwd("/git_repositories/trust")

#STEP 1: make sure you download the following raw data from the WVS site for each country
#and name them in the same file names as seen in /raw_data/USA/

  #WVS variables
    #0)low-income individuals
      #V239 - scale of incomes

    #1)Temporal Discounting
      #well being
        #V10 - feeling of happiness
        #V11 - state of health (subjective)
        #V23 - satisfaction with your life
      #financial
        #V59 - satisfaction with financial situation of household
      #education
        #V248 - highest educational level attained
        #V248_CS - education (country specific)
    #2)general trust 
      #V24 - most people can be trusted 

    #3)neighborhood trust
      #v103 - how much you trust: Your neighborhood
      #v170 - secure in neighborhood

  #this includes:
    ##the variable of interest
    ##the crossed data of your variable + income

#STEP 2: run the pipeline up to script 1 to get data for this country!

######################################################################################
#xls to csv function##################################################################
######################################################################################
WVS_xls2csv <- function(country, variable, crossed = F) {
  
  #happy#
  if (variable == "happy") {
    if (crossed == T) {df <- read_excel(paste0("WVS_data/data/raw_data/", 
                                               country, "/", variable, "C", ".xls"), 
                                        col_names=T, skip = 6)
                       df <- df[c(1:6), c(1, 3:13)] #column header cleaning
                       names(df) <- tolower(str_replace_all(names(df), " ", "_"))       
                       foo <- names(df)  #name cleaning
                       foo <- foo[-1]
                       foo <- c("happy", foo)
                       names(df) <- foo; remove(foo)}
    else {df <- read_excel(paste0("WVS_data/data/raw_data/", 
                                  country, "/", variable, ".xls"), #reading in
                           col_names=F, skip = 6) #renaming below
          names(df) <- c("happy", "tally", "percent")
          df <- df %>% mutate( #column cleaning
            tally = str_replace_all(tally, ".000", ""), #european periods
            tally = as.numeric(str_replace_all(tally, "\\.", "")))}}
  
  #health#
  else if (variable == "health") {  
    if (crossed == T) {df <- read_excel(paste0("WVS_data/data/raw_data/", 
                                               country, "/", variable, "C", ".xls"), #reading in
                                        col_names=T, skip = 6) #renaming below
                       df <- df[c(1:6), c(1, 3:13)] #column header cleaning
                       names(df) <- tolower(str_replace_all(names(df), " ", "_"))       
                       foo <- names(df)
                       foo <- foo[-1]
                       foo <- c("health", foo)
                       names(df) <- foo; remove(foo)}
    else {df <- read_excel(paste0("WVS_data/data/raw_data/", 
                                  country, "/", variable, ".xls"), 
                           col_names=T, skip = 5)
          names(df) <- c("health", "tally", "percent")
          df <- df %>% mutate(
            tally = str_replace_all(tally, ".000", ""),
            tally = as.numeric(str_replace_all(tally, "\\.", "")))}}
  
  #life#
  else if (variable == "life") {
    if (crossed == T) {df <- read_excel(paste0("WVS_data/data/raw_data/", 
                                               country, "/", variable, "C", ".xls"), #reading in
                                        col_names=T, skip = 6) #renaming below
                       df <- df[c(1:12), c(1, 3:13)] #column header cleaning
                       names(df) <- tolower(str_replace_all(names(df), " ", "_"))       
                       foo <- names(df)
                       foo <- foo[-1]
                       foo <- c("life", foo)
                       names(df) <- foo; remove(foo)}
    else {df <- read_excel(paste0("WVS_data/data/raw_data/", 
                                  country, "/", variable, ".xls"), 
                           col_names=T, skip = 5)
          names(df) <- c("life", "tally", "percent")
          df <- df[c(1:12),] %>% mutate(
            tally = str_replace_all(tally, ".000", ""),
            tally = as.numeric(str_replace_all(tally, "\\.", "")))}}
  
  #financial#
  else if (variable == "finance") {
    if (crossed == T) {df <- read_excel(paste0("WVS_data/data/raw_data/", 
                                               country, "/", variable, "C", ".xls"), #reading in
                                        col_names=T, skip = 6) #renaming below
                       df <- df[c(1:12), c(1, 3:13)]
                       names(df) <- tolower(str_replace_all(names(df), " ", "_"))       
                       foo <- names(df)
                       foo <- foo[-1]
                       foo <- c("finance", foo)
                       names(df) <- foo; remove(foo)}
    else {df <- read_excel(paste0("WVS_data/data/raw_data/", 
                                  country, "/", variable, ".xls"), 
                           col_names=T, skip = 5)
          names(df) <- c("finance", "tally", "percent")
          df <- df[c(1:12),] %>% mutate(
            tally = str_replace_all(tally, ".000", ""),
            tally = as.numeric(str_replace_all(tally, "\\.", "")))}}
  
  #education#
  else if (variable == "edu") {
    if (crossed == T) {df <- read_excel(paste0("WVS_data/data/raw_data/", 
                                               country, "/", variable, "C", ".xls"), #reading in
                                        col_names=T, skip = 6) #renaming below
                       df <- df[c(1:8),c(1, 3:13)]
                       names(df) <- tolower(str_replace_all(names(df), " ", "_"))       
                       foo <- names(df)
                       foo <- foo[-1]
                       foo <- c("edu", foo)
                       names(df) <- foo; remove(foo)}
    else {df <- read_excel(paste0("WVS_data/data/raw_data/", 
                                  country, "/", variable, ".xls"), 
                           col_names=T, skip = 5)
          names(df) <- c("edu", "tally", "percent")
          df <- df %>% mutate(
            tally = str_replace_all(tally, ".000", ""),
            tally = as.numeric(str_replace_all(tally, "\\.", "")))}}
  
  #education country specific#
  else if (variable == "edu_c") {
    if (crossed == T) {df <- read_excel(paste0("WVS_data/data/raw_data/", 
                                               country, "/", variable, "C", ".xls"), #reading in
                                        col_names=T, skip = 6) #renaming below
                       df <- df[c(1:15),c(1, 3:13)]
                       names(df) <- tolower(str_replace_all(names(df), " ", "_"))       
                       foo <- names(df)
                       foo <- foo[-1]
                       foo <- c("edu_c", foo)
                       names(df) <- foo; remove(foo)}                     
    else {df <- read_excel(paste0("WVS_data/data/raw_data/", 
                                  country, "/", variable, ".xls"), #reading in
                           col_names=T, skip = 5) #renaming below
          names(df) <- c("edu_c", "tally", "percent")
          df <- df %>% mutate(
            tally = str_replace_all(tally, ".000", ""),
            tally = as.numeric(str_replace_all(tally, "\\.", "")))}}
  
  #general trust#
  else if (variable == "gTrust") {
    if (crossed == T) {df <- read_excel(paste0("WVS_data/data/raw_data/", 
                                               country, "/", variable, "C", ".xls"), #reading in
                                        col_names=T, skip = 6) #renaming below
                       df <- df[c(1:4),c(1, 3:13)]
                       names(df) <- tolower(str_replace_all(names(df), " ", "_"))       
                       foo <- names(df)
                       foo <- foo[-1]
                       foo <- c("gTrust", foo)
                       names(df) <- foo; remove(foo)}
    else {df <- read_excel(paste0("WVS_data/data/raw_data/", 
                                  country, "/", variable, ".xls"), #reading in
                           col_names=T, skip = 5) #renaming below
          names(df) <- c("gTrust", "tally", "percent")
          df <- df %>% mutate(
            tally = str_replace_all(tally, ".000", ""),
            tally = as.numeric(str_replace_all(tally, "\\.", "")))}}
  
  #neighborhood trust#
  else if (variable == "nTrust") {
    if (crossed == T) {df <- read_excel(paste0("WVS_data/data/raw_data/", 
                                               country, "/", variable, "C", ".xls"), #reading in
                                        col_names=T, skip = 6) #renaming below
                       df <- df[c(1:6),c(1, 3:13)]
                       names(df) <- tolower(str_replace_all(names(df), " ", "_"))       
                       foo <- names(df)
                       foo <- foo[-1]
                       foo <- c("nTrust", foo)
                       names(df) <- foo; remove(foo)}
    else {df <- read_excel(paste0("WVS_data/data/raw_data/", 
                                  country, "/", variable, ".xls"), #reading in
                           col_names=T, skip = 5) #renaming below
          names(df) <- c("nTrust", "tally", "percent")
          df <- df %>% mutate(
            tally = str_replace_all(tally, ".000", ""),
            tally = as.numeric(str_replace_all(tally, "\\.", "")))}}
  
  #neighborhood security#
  else if (variable == "security") {
    if (crossed == T) {df <- read_excel(paste0("WVS_data/data/raw_data/", 
                                               country, "/", variable, "C", ".xls"), #reading in
                                        col_names=T, skip = 6) #renaming below
                       df <- df[c(1:6),c(1, 3:13)]
                       names(df) <- tolower(str_replace_all(names(df), " ", "_"))       
                       foo <- names(df)
                       foo <- foo[-1]
                       foo <- c("security", foo)
                       names(df) <- foo; remove(foo)}
    else {df <- read_excel(paste0("WVS_data/data/raw_data/", 
                                  country, "/", variable, ".xls"), #reading in
                           col_names=T, skip = 5) #renaming below
          names(df) <- c("security", "tally", "percent")
          df <- df %>% mutate(
            tally = str_replace_all(tally, ".000", ""),
            tally = as.numeric(str_replace_all(tally, "\\.", "")))}}
  
  #income#
  else if (variable == "income") {    
    df <- read_excel(paste0("WVS_data/data/raw_data/", 
                            country, "/", "income", ".xls"), #reading in
                     col_names=T, skip = 5) #renaming below
    df <- df[c(1:12),]
    names(df) <- c("income", "tally", "percent")
    df <- df %>% mutate(
      tally = str_replace_all(tally, ".000", ""),
      tally = as.numeric(str_replace_all(tally, "\\.", "")))}
  
  return(df) #returns back the data frame
}

######################################################################################
#writing out clean data function######################################################
######################################################################################
WVS_raw2clean <- function(pais) {
  
  #making new folder for country
  parent_dir <- paste("WVS_data/data/in_process_data", pais, sep = "/")
  dir.create(parent_dir, showWarnings=F) 
  
  #formatting and writing out new data files!
  df1 <- WVS_xls2csv(pais, "happy"); write_csv(df1, paste0(parent_dir, "/happy_cleaned_", pais, ".csv")) 
  df2 <- WVS_xls2csv(pais, "happy", crossed = T); write_csv(df2, paste0(parent_dir, "/happyC_cleaned_", pais, ".csv")) 
  df3 <- left_join(df1, df2, by ="happy") %>% mutate(country = pais)
  write_csv(df3, paste0(parent_dir, "/happy_full_cleaned_", pais, ".csv"))
  
  df1 <- WVS_xls2csv(pais, "health"); write_csv(df1, paste0(parent_dir, "/health_cleaned_", pais, ".csv")) 
  df2 <- WVS_xls2csv(pais, "health", crossed = T); write_csv(df2, paste0(parent_dir, "/healthC_cleaned_", pais, ".csv"))            
  df3 <- left_join(df1, df2, by ="health") %>% mutate(country = pais)
  write_csv(df3, paste0(parent_dir, "/health_full_cleaned_", pais, ".csv"))
  
  df1 <- WVS_xls2csv(pais, "life"); write_csv(df1, paste0(parent_dir, "/life_cleaned_", pais, ".csv")) 
  df2 <- WVS_xls2csv(pais, "life", crossed = T); write_csv(df2, paste0(parent_dir, "/lifeC_cleaned_", pais, ".csv"))            
  df3 <- left_join(df1, df2, by ="life") %>% mutate(life = 
            ifelse(life == "No answer; BH: Refused", "No answer",
            ifelse(life == "Completely satisfied","Fully satisfied",
            ifelse(life == "Completely dissatisfied", "Fully dissatisfied", life))))
          write_csv(df3, paste0(parent_dir, "/life_full_cleaned_", pais, ".csv"))
  
  df1 <- WVS_xls2csv(pais, "finance"); write_csv(df1, paste0(parent_dir, "/finance_cleaned_", pais, ".csv")) 
  df2 <- WVS_xls2csv(pais, "finance", crossed = T); write_csv(df2, paste0(parent_dir, "/financeC_cleaned_", pais, ".csv"))            
  df3 <- left_join(df1, df2, by ="finance") %>% mutate(finance = 
           ifelse(finance == "No answer; BH: Refused", "No answer",
           ifelse(finance == "Completely satisfied", "Fully satisfied",
           ifelse(finance == "Completely dissatisfied", "Fully dissatisfied", finance))))
          write_csv(df3, paste0(parent_dir, "/finance_full_cleaned_", pais, ".csv"))
  
  df1 <- WVS_xls2csv(pais, "edu"); write_csv(df1, paste0(parent_dir, "/edu_cleaned_", pais, ".csv")) 
  df2 <- WVS_xls2csv(pais, "edu", crossed = T); write_csv(df2, paste0(parent_dir, "/eduC_cleaned_", pais, ".csv"))            
  df3 <- left_join(df1, df2, by ="edu") %>% mutate(country = pais)
    write_csv(df3, paste0(parent_dir, "/edu_full_cleaned_", pais, ".csv"))
  
  df1 <- WVS_xls2csv(pais, "edu_c"); write_csv(df1, paste0(parent_dir, "/edu_c_cleaned_", pais, ".csv")) 
  df2 <- WVS_xls2csv(pais, "edu_c", crossed = T); write_csv(df2, paste0(parent_dir, "/edu_cC_cleaned_", pais, ".csv"))            
  df3 <- left_join(df1, df2, by ="edu_c") %>% mutate(country = pais)
    write_csv(df3, paste0(parent_dir, "/edu_c_full_cleaned_", pais, ".csv"))
  
  df1 <- WVS_xls2csv(pais, "gTrust"); write_csv(df1, paste0(parent_dir, "/gTrust_cleaned_", pais, ".csv")) 
  df2 <- WVS_xls2csv(pais, "gTrust", crossed = T); write_csv(df2, paste0(parent_dir, "/gTrustC_cleaned_", pais, ".csv"))            
  df3 <- left_join(df1, df2, by ="gTrust") %>% mutate(country = pais)
    write_csv(df3, paste0(parent_dir, "/gTrust_full_cleaned_", pais, ".csv"))
  
  df1 <- WVS_xls2csv(pais, "nTrust"); write_csv(df1, paste0(parent_dir, "/nTrust_cleaned_", pais, ".csv")) 
  df2 <- WVS_xls2csv(pais, "nTrust", crossed = T); write_csv(df2, paste0(parent_dir, "/nTrustC_cleaned_", pais, ".csv"))            
  df3 <- left_join(df1, df2, by ="nTrust") %>% mutate(country = pais)
    write_csv(df3, paste0(parent_dir, "/nTrust_full_cleaned_", pais, ".csv"))
  
  df1 <- WVS_xls2csv(pais, "security"); write_csv(df1, paste0(parent_dir, "/security_cleaned_", pais, ".csv")) 
  df2 <- WVS_xls2csv(pais, "security", crossed = T); write_csv(df2, paste0(parent_dir, "/security_cleaned_", pais, ".csv"))            
  df3 <- left_join(df1, df2, by ="security") %>% mutate(country = pais)
    write_csv(df3, paste0(parent_dir, "/security_full_cleaned_", pais, ".csv"))
  
  df1 <- WVS_xls2csv(pais, "income"); write_csv(df1, paste0(parent_dir, "/income_full_cleaned_", pais, ".csv"))
  
  print(paste0(pais, " DATA FINISHED:", " check out 'in_process_data/", pais, "' folder for your clean data!"))
}





