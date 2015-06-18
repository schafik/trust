# attempting pca with WVS data
require('dplyr')
library(readr)

codes <- read_delim("WVS_data/data/raw_data/country_codes.csv", 
                    delim = "##", col_names=F) %>% 
    select(country_code = X1, country = X3)


w2 <- readRDS("WVS_data/data/raw_data/wave2_raw.RDS") %>%
        select(wave = V1, country_code = V2, happy = V18,
                 health = V83, life = V96, gTrust = V94, 
                 finance = V132, nTrust = V347_32, income = V363, 
                 edu = V375, edu_c = V375CS) 
w2 <- left_join(w2, codes, by="country_code") %>% select(-country_code)
      

w5 <- readRDS("WVS_data/data/raw_data/wave5_raw.RDS") %>%
        select(wave = V1, country_code = V2, happy = V10,
                 health = V11, life = V22, gTrust = V23, 
                 finance = V68, nTrust = V126, 
                 income = V253, edu = V238, edu_c = V238CS) 
w5 <- left_join(w5, codes, by="country_code") %>% select(-country_code)


w6 <- readRDS("WVS_data/data/raw_data/wave6_raw.RDS") %>%
            select(wave = V1, country_code = V2, happy = V10,
                   health = V11, life = V23, gTrust = V24, 
                   finance = V59, nTrust = V103, security = V170, 
                   income = V239, edu = V248, edu_c = V248_CS)
w6 <- left_join(w6, codes, by="country_code") %>% select(-country_code, -security)



# re-order
NA_recode <- function(input_vec) {
    ifelse(input_vec %in% c(-1, -3, -4, -2, -5), NA, input_vec)
}


wvs_data <- rbind_list(w2, w5, w6)    

wvs_data <- wvs_data %>% mutate(health =
                                    ifelse(health == 1, 5, 
                                    ifelse(health == 2, 4, 
                                    ifelse(health == 3, 3, 
                                    ifelse(health == 4, 2,
                                    ifelse(health == 5, 1, health))))),
                                happy =
                                    ifelse(happy == 1, 4, 
                                    ifelse(happy == 2, 3, 
                                    ifelse(happy == 3, 2, 
                                    ifelse(happy == 4, 1, happy)))),
                                nTrust =
                                    ifelse(nTrust == 1, 5, 
                                    ifelse(nTrust == 2, 4, 
                                    ifelse(nTrust == 3, 3, 
                                    ifelse(nTrust == 4, 2,
                                    ifelse(nTrust == 5, 1, nTrust)))))
                                )





wvs_data <- wvs_data %>% mutate(happy = NA_recode(happy),
                                health = NA_recode(health),
                                life = NA_recode(life),
                                gTrust = NA_recode(gTrust),
                                finance = NA_recode(finance),
                                nTrust = NA_recode(nTrust),
                                income = NA_recode(income),
                                edu = NA_recode(edu),
                                edu_c = NA_recode(edu_c))
# data prep done, PCA
normalize <- function(input_vec) {
    scale(input_vec, scale = T, center = T)
}


# wvs_data_norm <- wvs_data %>% mutate(happy = normalize(happy),
#                                 health = normalize(health),
#                                 life = normalize(life),
#                                 gTrust = normalize(gTrust),
#                                 finance = normalize(finance),
#                                 nTrust = normalize(nTrust),
#                                 income = normalize(income),
#                                 edu = normalize(edu),
#                                 edu_c = normalize(edu_c))

hist(wvs_data$edu)
pca_df <- wvs_data %>% select(-edu_c, -wave, -country, -nTrust, -gTrust)

summary(pca_df)

pca_mod <- prcomp(~ ., data=pca_df, na.action=na.omit, scale=TRUE, center=TRUE)
pca_mod
summary(pca_mod)
plot(pca_mod, type="l")
#loadings(pca_mod)
pca_mod$rotation
test <- pca_mod$rotation[,"PC1"]

#getting 1st prcomp
pc1 <- pca_mod$x[,1]

# biplot(pca_mod)


pca_val_data <- wvs_data %>% 
                    filter(row.names(wvs_data) %in% names(pc1)) %>%
                    select(wave, nTrust) %>%
                    mutate(pc1 = pc1)

fit <- lm(pc1~nTrust, data = pca_val_data)
summary(fit)
# plot(fit)

boxplot(pc1~nTrust, data=pca_val_data)


require(ggplot2)
# calculate group mean
pca_val_data %>% group_by(nTrust) %>% summarise(gmean = mean(pc1, na.rm = T))
