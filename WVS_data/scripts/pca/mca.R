#MCA
require(FactoMineR)
require(dplyr)

full_data <- readRDS("WVS_data/data/in_process_data/fulldata_cleaned.RDS")

names(full_data)

mca_data <- full_data %>% select(happy, life, gTrust, finance, nTrust, income, edu, lowIncome_4)
category_counts <- apply(mca_data, 2, function(x) nlevels(as.factor(x)))

category_counts

mca_mod <-  MCA(mca_data, graph = FALSE, na.method = "Average")
mca_mod
mca_mod$eig

mca_var_df <- data.frame(mca_mod$var$coord, Variable = rep(names(category_counts), category_counts))
# data frame with observation coordinates
mca_obs_df = data.frame(mca_mod$ind$coord)

# plot of variable categories
ggplot(data=mca_var_df, 
       aes(x = Dim.1, y = Dim.2, label = rownames(mca_var_df))) +
    geom_hline(yintercept = 0, colour = "gray70") +
    geom_vline(xintercept = 0, colour = "gray70") +
    geom_text(aes(colour=Variable)) +
    ggtitle("MCA plot of variables using R package FactoMineR")


ggplot(data = mca_obs_df, aes(x = Dim.1, y = Dim.2)) +
    geom_hline(yintercept = 0, colour = "gray70") +
    geom_vline(xintercept = 0, colour = "gray70") +
    geom_point(colour = "gray50", alpha = 0.7) +
    geom_density2d(colour = "gray80") +
    geom_text(data = mca_var_df, 
              aes(x = Dim.1, y = Dim.2, 
                  label = rownames(mca_var_df), colour = Variable)) +
    ggtitle("MCA plot of variables using R package FactoMineR") +
    scale_colour_discrete(name = "Variable")


# res.mca = MCA(tea, quanti.sup=19, quali.sup=c(20:36))
plot.MCA(mca_mod, invisible=c("var","quali.sup"), cex=0.7)
plot.MCA(mca_mod, invisible=c("ind","quali.sup"), cex=0.7)
plot.MCA(mca_mod, invisible=c("ind"))
plot.MCA(mca_mod, invisible=c("ind", "var"))
dimdesc(mca_mod)
