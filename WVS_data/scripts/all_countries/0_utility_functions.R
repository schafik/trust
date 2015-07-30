############################################################
#useful functions for data pipeline#########################
############################################################

smartNAs <- function(df) {
  df <- df %>% mutate( 
    #for questions not answered
    happy = ifelse(happy == -2, "No answer", happy),
    health = ifelse(health == -2, "No answer", health),
    life = ifelse(life == -2, "No answer", life),
    gTrust = ifelse(gTrust == -2, "No answer", gTrust),
    finance = ifelse(finance == -2, "No answer", finance),
    nTrust = ifelse(nTrust == -2, "No answer", nTrust),
    income = ifelse(income == -2, "No answer", income),
    edu = ifelse(edu == -2, "No answer", edu),
    edu_c = ifelse(edu_c == -2, "No answer", edu_c),
    
    #for questions that are NA
    happy = ifelse(happy %in% c(-1, -3, -4), NA, happy),
    health = ifelse(health %in% c(-1, -3, -4), NA, health),
    life = ifelse(life %in% c(-1, -3, -4), NA, life),
    gTrust = ifelse(gTrust %in% c(-1, -3, -4), NA, gTrust),
    finance = ifelse(finance %in% c(-1, -3, -4), NA, finance),
    nTrust = ifelse(nTrust %in% c(-1, -3, -4), NA, nTrust),
    income = ifelse(income %in% c(-1, -3, -4), NA, income),
    edu = ifelse(edu %in% c(-1, -3, -4), NA, edu),
    edu_c = ifelse(edu_c %in% c(-1, -3, -4), NA, edu_c))
  return(df)
}


###############################################################################
###################transcribing helpers#################################
###################################################################
health_fx <- function(df) {
  df <- df %>% mutate(health =
            ifelse(health == 1, "Very good", 
            ifelse(health == 2, "Good", 
            ifelse(health == 3, "Fair", 
            ifelse(health == 4, "Poor",
            ifelse(health == 5, "Very poor", health))))),
      health = factor(as.character(health), levels = 
        c("Very good", "Good", "Fair", "Poor", "Very poor", "No answer")))
}

happy_fx <- function(df) {
  df <- df %>% mutate(happy =
            ifelse(happy == 1, "Very happy", 
            ifelse(happy == 2, "Rather happy", 
            ifelse(happy == 3, "Not very happy", 
            ifelse(happy == 4, "Not at all happy", happy)))),
      happy = factor(as.character(happy), levels = 
        c("Very happy", "Rather happy", "Not very happy", "Not at all happy")))
}
# 
life_fx <- function(df) {
  df <- df %>% mutate(life =
            ifelse(life == 1, "Completely dissatisfied", 
            ifelse(life == 2, "2", 
            ifelse(life == 3, "3", 
            ifelse(life == 4, "4", 
            ifelse(life == 5, "5", 
            ifelse(life == 6, "6", 
            ifelse(life == 7, "7", 
            ifelse(life == 8, "8", 
            ifelse(life == 9, "9", 
            ifelse(life == 10, "Completely satisfied", life)))))))))),
      life = factor(as.character(life), levels = 
        c("Completely dissatisfied", paste0(2:9), "Completely satisfied")))
}

gTrust_fx <- function(df) {
  df <- df %>% mutate(gTrust =
            ifelse(gTrust == 1, "Most people can be trusted", 
            ifelse(gTrust == 2, "Need to be very careful", gTrust)),
    gTrust = factor(as.character(gTrust), levels = 
        c("Most people can be trusted", "Need to be very careful")))
}
# 
finance_fx <- function(df) {
  df <- df %>% mutate(finance =
            ifelse(finance == 1, "Completely dissatisfied", 
            ifelse(finance == 2, "2", 
            ifelse(finance == 3, "3", 
            ifelse(finance == 4, "4", 
            ifelse(finance == 5, "5", 
            ifelse(finance == 6, "6", 
            ifelse(finance == 7, "7", 
            ifelse(finance == 8, "8", 
            ifelse(finance == 9, "9", 
            ifelse(finance == 10, "Completely satisfied", finance)))))))))),
   finance = factor(as.character(finance), levels = 
      c("Completely dissatisfied", paste0(2:9), "Completely satisfied")))
}

nTrust_fx <- function(df) {
  df <- df %>% mutate(nTrust =
            ifelse(nTrust == 1, "Trust completely", 
            ifelse(nTrust == 2, "Trust somewhat", 
            ifelse(nTrust == 3, "Do not trust very much", 
            ifelse(nTrust == 4, "Do not trust at all", nTrust)))),
  nTrust = factor(as.character(nTrust), levels = 
    c("Trust completely", "Trust somewhat", 
      "Do not trust very much", "Do not trust at all")))
}

income_fx <- function(df) {
  df <- df %>% mutate(income =
            ifelse(income == 1, "1", 
            ifelse(income == 2, "2", 
            ifelse(income == 3, "3", 
            ifelse(income == 4, "4", 
            ifelse(income == 5, "5", 
            ifelse(income == 6, "6", 
            ifelse(income == 7, "7", 
            ifelse(income == 8, "8", 
            ifelse(income == 9, "9", 
            ifelse(income == 10, "10", 
            ifelse(income == -5, NA, income))))))))))),
    income = factor(as.character(income), levels = 
      c(paste0(1:10))))
}
# 
edu_fx <- function(df) {
  df <- df %>% mutate(edu =
            ifelse(edu == 1, "No formal education", 
            ifelse(edu == 2, "Incomplete primary school", 
            ifelse(edu == 3, "Complete primary school", 
            ifelse(edu == 4, "Incomplete secondary school: technical/ vocational type", 
            ifelse(edu == 5, "Complete secondary school: technical/ vocational type", 
            ifelse(edu == 6, "Incomplete secondary school: university-preparatory type",
            ifelse(edu == 7, "Complete secondary school: university-preparatory type", 
            ifelse(edu == 8, "Some university-level education, without degree", 
            ifelse(edu == 9, "University - level education, with degree", 
            ifelse(edu == -5, NA, edu)))))))))),
    edu = factor(as.character(edu), levels = 
      c("No formal education", "Incomplete primary school", "Complete primary school",
        "Incomplete secondary school: technical/ vocational type", 
        "Complete secondary school: technical/ vocational type",
        "Incomplete secondary school: university-preparatory type",
        "Complete secondary school: university-preparatory type", 
        "Some university-level education, without degree", 
        "University - level education, with degree")))
}


clarify <- function(df) {     #helper function that brings the above together
  df <- happy_fx(df)
  df <- health_fx(df)
  df <- life_fx(df)
  df <- gTrust_fx(df)
  df <- nTrust_fx(df)
  df <- finance_fx(df)
  df <- income_fx(df)
  df <- edu_fx(df)
  return(df)
}



