# Comparison 3X: seeds_per_pod in JC hybrids
#load packages 
install.packages("multcompView")
install.packages("viridis")

library(viridis)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggplot2)
library(ggpubr)
library(datasets)
library(multcompView)


# vollständige Rohdatentabelle laden ######
setwd("C:/Users/Helen Behn/Documents/R/EPPN Data Analysis/Final raw data table")
DATA <- read_csv("EPPN raw data complete 220419.csv")

#drop first column
DATA = DATA[,2:37]
View(DATA)

# subsetting
comp_3X <- DATA[DATA$Genotype %in% c("C1", "C2", "J1", "J1C1", "J1C2"), ]
head(comp_3X)
View(comp_3X)

#create column giving generation###############################################
comp_3X <- comp_3X %>%
  mutate(Generation = case_when(
    Genotype == "J1" ~ "P1",
    Genotype == "C1" ~ "P2",
    Genotype == "C2" ~ "P2",
    Genotype == "J1C1" ~ "JC hybrid",
    Genotype == "J1C2" ~ "JC hybrid"))
view(comp_3X)


# Boxplot with CLD nach Rosane Rech##########################################################
# loading the appropriate libraries
library(datasets)
library(ggplot2)
library(multcompView)
library(dplyr)

# analysis of variance
anova <- aov(seeds_per_pod ~ Genotype, data = comp_3X)
summary(anova)

# Tukey's test
tukey <- TukeyHSD(anova)
print(tukey)

# compact letter display
cld <- multcompLetters4(anova, tukey)
print(cld)

# table with factors and 3rd quantile
Tk <- group_by(comp_3X, Genotype) %>%
  summarise(mean = round(mean(seeds_per_pod, na.rm = TRUE), digits = 1), quant = round(quantile(seeds_per_pod, probs = 0.75, na.rm = TRUE), digits = 1)) %>%
  arrange(desc(mean)) #WICHTIG: arrange(desc(mean)) muss hier rein!!!

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$Genotype)
Tk$cld <- cld$Letters

#create column giving generation###############################################
ANOVA_Tukey_3X <- Tk %>%
  mutate(Generation = case_when(
    Genotype == "J1" ~ "P1",
    Genotype == "C1" ~ "P2",
    Genotype == "C2" ~ "P2",
    Genotype == "J1C1" ~ "JC hybrid",
    Genotype == "J1C2" ~ "JC hybrid"))
view(ANOVA_Tukey_3X)

# Reorder factor levels in comp_3 and ANOVA_Tukey_3
comp_3Xord <- comp_3X                             
comp_3Xord$Genotype <- factor(comp_3Xord$Genotype,     
                             levels = c("J1C1", "J1C2", "C1", "C2", "J1"))

ANOVA_Tukey_3Xord <- ANOVA_Tukey_3X                             
ANOVA_Tukey_3Xord$Genotype <- factor(ANOVA_Tukey_3Xord$Genotype,     
                                    levels = c("J1C1", "J1C2", "C1", "C2", "J1"))

# boxplot seeds_per_pod##############################################################
ggplot(comp_3Xord, aes(Genotype, seeds_per_pod, fill = Generation, color = Generation)) + 
  geom_boxplot() +
  labs(x="Genotype", y="Seeds/pod") +
  geom_text(data = ANOVA_Tukey_3Xord, aes(x = Genotype, y = quant, label = cld), color = "black", size = 4, vjust = 0.3, hjust = 4.5) +
  coord_flip() +
  theme(axis.title = element_text(size = 16),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8)) +
  scale_color_viridis(discrete = TRUE, option = "D", direction = 1, guide = "none", alpha = 1) +  #legend removed by guide = "none" 
  scale_fill_viridis(discrete = TRUE, option = "D", direction = 1, alpha = 0.5)



