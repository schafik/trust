

#for reading in data###########################################################################
read_WVS <- function(country, variable) {
  df <- read_csv(paste0("/git_repositories/trust/WVS_data/data/in_process_data/", #read in data
                        country, "/", variable, "_full_cleaned_", country, ".csv"))
}


#for plotting##################################################################################
all_respondents <- function(country, variable, coordflip = F) {
  df <- read_WVS(country, variable) 
  df <- df[-nrow(df),] #removing last row
  if (coordflip == T){
    ggplot(df, aes_string(x=variable, y="percent")) + #plot!
      geom_bar(stat="identity", colour = "black", fill = "#F0E442") + 
      ggtitle(paste0(country, " ", variable, "\nall respondents")) + xlab("") + 
      geom_text(stat='identity', aes_string(label="tally", hjust = -.25)) + 
      theme(axis.text.x  = element_text(face="bold")) + coord_flip()}
  else{
  ggplot(df, aes_string(x=variable, y="percent")) + #plot!
    geom_bar(stat="identity", colour = "black", fill = "#F0E442") + 
    ggtitle(paste0(country, " ", variable, "\nall respondents")) + 
    geom_text(stat='identity', aes_string(label="tally", vjust=-.25)) + xlab("") + 
    theme(axis.text.x  = element_text(face="bold"))}
}

by_income <- function(country, var, classification, coordflip = F) {
    df <- read_WVS(country, var)
    df <- df[-nrow(df), -c(2,3)] #removing last row and 2nd + 3rd columns
  if (classification == 4) {
    df <- melt(df, id = var) %>% #melting to long format  
        mutate(income = ifelse(variable %in% c("lower_step", "second_step", "third_step", "fourth_step"), 
                        "low income", "middle/high income")) %>%    
        group_by_(as.name(var), ~income) %>% filter(variable != "country") %>%
        summarize(percent =  sum(as.numeric(value))) %>% 
        mutate(percent = ifelse(income == "low income",
                                round(percent/4, digits=2),
                                round(percent/7, digits=2)))}
  else if (classification == 3) {
    df <- melt(df, id = var) %>% 
      mutate(income = ifelse(variable %in% c("lower_step", "second_step", "third_step"), 
                             "low income", "middle/high income")) %>%    
      group_by_(as.name(var), ~income) %>% filter(variable != "country") %>%
      summarize(percent =  sum(as.numeric(value))) %>% 
      mutate(percent = ifelse(income == "low income",
                              round(percent/3, digits=2),
                              round(percent/8, digits=2)))}
  ggplot(df, aes_string(x=var, y="percent", fill="income")) + 
    ggtitle(paste0(country, " ", var, "\nby income")) + 
    geom_bar(stat="identity", colour = "black", position=position_dodge()) + 
    theme(legend.position="bottom", axis.text.x  = element_text(face="bold")) + xlab("")
}

edu_labs <- read_WVS("USA", "edu") %>% select(edu) %>% filter(edu != "(N)")
  edu_labs <- edu_labs$edu
  
edu_c_labs <- read_WVS("USA", "edu_c") %>% select(edu_c) %>% filter(edu_c != "(N)")
  edu_c_labs <- edu_c_labs$edu_c

nTrust_labs <- read_WVS("USA", "nTrust") %>% select(nTrust) %>% filter(nTrust != "(N)")
  nTrust_labs <- nTrust_labs$nTrust

income_labs <- read_WVS("USA", "income") %>% select(income) %>% filter(income != "(N)")
  income_labs <- as.vector(income_labs$income)


#remove(all_respondents, by_income, edu_labs, edu_c_labs, nTrust_labs, income_labs)


