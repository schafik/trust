
#reading in libraries
library(readxl)
library(plyr)
library(dplyr)

#read in 6 sheets
sheet1 <- read_excel("2b_bangladesh/data/bangladesh_data.xlsx", sheet = 1, col_names = TRUE, skip = 1)
sheet2 <- read_excel("2b_bangladesh/data/bangladesh_data.xlsx", sheet = 2, col_names = TRUE, skip = 1)
sheet3 <- read_excel("2b_bangladesh/data/bangladesh_data.xlsx", sheet = 3, col_names = TRUE, skip = 1)
sheet4 <- read_excel("2b_bangladesh/data/bangladesh_data.xlsx", sheet = 4, col_names = TRUE, skip = 1)
sheet5 <- read_excel("2b_bangladesh/data/bangladesh_data.xlsx", sheet = 5, col_names = TRUE, skip = 1)
sheet6 <- read_excel("2b_bangladesh/data/bangladesh_data.xlsx", sheet = 6, col_names = TRUE, skip = 1)

colnames(sheet1) <- c("ref.no", "interviewerID", "Name", "Gender", "Union",
                      "MonthlyHHIncome", "no.HHmember", "Treatment", 
                      "Q1", "Q2", "Q3", "Q4", 
                      "Q5", "Q6", "Q7", "Q8", "Q9", 
                      "Q10",  NA, "500_1", "525", "500_2", 
                      "550", "500_3", "750", "500_4", "1000",
                      "500_5", "1500", "500_6", "2500", "500_7", 
                      "3000", "Switch_point")

colnames(sheet2) <- c("ref.no", "interviewerID", "Name", "Gender", "Union",
                      "MonthlyHHIncome", "no.HHmember", "Treatment", 
                      "Q1", "Q2", "Q3", "Q4", 
                      "Q5", "Q6", "Q7", "Q8", "Q9", 
                      "Q10",  NA, "500_1", "525", "500_2", 
                      "550", "500_3", "750", "500_4", "1000",
                      "500_5", "1500", "500_6", "2500", "500_7", 
                      "3000", "Switch_point")

colnames(sheet3) <- c("ref.no", "interviewerID", "Name", "Gender", "Union",
                      "MonthlyHHIncome", "no.HHmember", "Treatment", 
                      "Q1", "Q2", "Q3", "Q4", 
                      "Q5", "Q6", "Q7", "Q8", "Q9", 
                      "Q10",  NA,  "500_1", "525", "500_2", 
                      "550", "500_3", "750", "500_4", "1000",
                      "500_5", "1500", "500_6", "2500", "500_7", 
                      "3000", "Switch_point")

colnames(sheet4) <- c("ref.no", "interviewerID", "Name", "Gender", "Union",
                      "MonthlyHHIncome", "no.HHmember", "Treatment", 
                      "Q1", "Q2", "Q3", "Q4", 
                      "Q5", "Q6", "Q7", "Q8", "Q9", 
                      "Q10",  NA,  "500_1", "525", "500_2", 
                      "550", "500_3", "750", "500_4", "1000",
                      "500_5", "1500", "500_6", "2500", "500_7", 
                      "3000", "Switch_point")


colnames(sheet5) <- c("ref.no", "interviewerID", "Name", "Gender", 
                      "Age", "Union", "MonthlyHHIncome", 
                      "no.HHmember", "Treatment",
                      "Q1", "Q2", "Q3", "Q4", 
                      "Q5", "Q6", "Q7", "Q8", "Q9", 
                      "Q10",  NA, "500_1", "525", "500_2", 
                      "550", "500_3", "750", "500_4", "1000",
                      "500_5", "1500", "500_6", "2500", "500_7", 
                      "3000", "Switch_point")

colnames(sheet6) <- c("ref.no", "interviewerID", "Name", "Gender", 
                      "Age", "Union", "MonthlyHHIncome", 
                      "no.HHmember", "Treatment",
                      "Q1", "Q2", "Q3", "Q4", 
                      "Q5", "Q6", "Q7", "Q8", "Q9", 
                      "Q10", NA, "500_1", "525", "500_2", 
                      "550", "500_3", "750", "500_4", "1000",
                      "500_5", "1500", "500_6", "2500", "500_7", 
                      "3000", "Switch_point")
                   
# remove empty columns
sheet1 <- sheet1[, -19]
sheet2 <- sheet2[, -19]
sheet3 <- sheet3[, -19]
sheet4 <- sheet4[, -19]
sheet5 <- sheet5[, -20]
sheet6 <- sheet6[, -20]

#merge/combine sheets into one df 
l <- list(sheet1, sheet2, sheet3, sheet4, sheet5, sheet6)
full_data <- do.call(rbind.fill, l); remove(l, sheet1, sheet2, sheet3,
                                                sheet4, sheet5, sheet6)

full_data_sub <- full_data %>% select(ref.no, `500_1`:`3000`, Switch_point)
  
# 1. Check for transitivity -----------------------------------------------
LL_only <- dplyr::select(full_data_sub, ref.no, `525`, `550`, `750`, 
                         `1000`, `1500`, `2500`, `3000`)  # [L]arger [Later]

LL_only$diff1 <- LL_only$`550`-LL_only$`525`
LL_only$diff2 <- LL_only$`750`-LL_only$`550`
LL_only$diff3 <- LL_only$`1000`-LL_only$`750`
LL_only$diff4 <- LL_only$`1500`-LL_only$`1000`
LL_only$diff5 <- LL_only$`2500`-LL_only$`1500`
LL_only$diff6 <- LL_only$`3000`-LL_only$`2500`

# check for any negatives... none => intransitive respondents! #
n_negativo <- function(x) sum(x < 0)
colwise(n_negativo)(LL_only %>% select(diff1:diff6)) %>%
    transmute(negatives = rowSums(.)); remove(n_negativo, LL_only, full_data_sub)

# 2. Caclulate iPoint -----------------------------------------------

full_data <- full_data %>% mutate(
        iPoint = ifelse(Switch_point == "never", 3001, #means present biased!
                 ifelse(Switch_point == "525", 512.5, 
                 ifelse(Switch_point == "550", 537.5,
                 ifelse(Switch_point == "750", 650,
                 ifelse(Switch_point == "1000", 875,
                 ifelse(Switch_point == "1500", 1250,
                 ifelse(Switch_point == "2500", 2000,
                 ifelse(Switch_point == "3000", 2750, NA)))))))),

# 3. Caclulate delta (discount factor) -------------------------------
        
        delta = 500/iPoint,      #hyperbolic + exponential the same

# 4. Caclulate k (discount rate) -------------------------------------

        k_hyperbolic_quarterly = (1 - delta)/delta,      #hyperbolic
        k_exponential_quarterly = -log(delta),          #exponential

  #higher value means more present biased/less patient!

# 5. Caclulate k (annual discount rate) ------------------------------

        k_hyperbolic_annual = ((k_hyperbolic_quarterly + 1)^4) - 1, 
        k_exponential_annual = ((k_exponential_quarterly + 1)^4) - 1)          



write_csv(full_data, "2b_bangladesh/data/discounting_rates.csv")


