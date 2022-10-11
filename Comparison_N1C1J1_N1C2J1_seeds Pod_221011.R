# Comparison 2XY: Novel allohexaploids

#load packages 
install.packages("multcompView")
install.packages("viridis")
install.packages("viridisLite")
library(viridis)
library(viridisLite)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(datasets)
library(multcompView)


# vollständige Rohdatentabelle laden ######
getwd()
DATA <- read_csv("EPPN raw data complete 220419.csv")
View(DATA)


#drop first column
DATA = DATA[,2:37]
View(DATA)

#### subsetting #####
comp_N1C1J1_N1C2J1 <- DATA[DATA$Genotype %in% c("N1", "C1", "C2", "J1", 
                                         "N1C1J1_PH25", "N1C1J1_PH36", "N1C1J1_PH31", "N1C1J1_PH39", 
                                         "N1C2J1_PH26", "N1C2J1_PH40", 
                                         "N1C1J1.N1C2J1"), ]
head(comp_N1C1J1_N1C2J1)
View(comp_N1C1J1_N1C2J1)


# ANOVA, Tukey & Boxplot with CLD according to Rosane Rech##########################################################
# loading the appropriate libraries
library(datasets)

#remove previous objects
rm(anova)
rm(tukey)
rm(cld)
rm(Tk)

# analysis of variance
anova <- aov(seeds_per_pod ~ Genotype, data = comp_N1C1J1_N1C2J1)
summary(anova)

# Tukey's test
tukey <- TukeyHSD(anova)
print(tukey)

# compact letter display
cld <- multcompLetters4(anova, tukey)
print(cld)

# table with factors and 3rd quantile
Tk <- group_by(comp_N1C1J1_N1C2J1, Genotype) %>%
  summarise(mean = round(mean(seeds_per_pod, na.rm = TRUE), digits = 1), quant = round(quantile(seeds_per_pod, probs = 0.75, na.rm = TRUE), digits = 1)) %>%
  arrange(desc(mean)) #WICHTIG: arrange(desc(mean)) muss hier rein!!!

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$Genotype)
Tk$cld <- cld$Letters
print(Tk) # print dient der Überprüfung: Mittelwerte und Letter code 
# in der gleichen Reihenfolge angeordnet?

# Reorder factor levels in comp_N1C1J1_N1C2J1
comp_N1C1J1_N1C2J1ord <- comp_N1C1J1_N1C2J1                             
comp_N1C1J1_N1C2J1ord$Genotype <- factor(comp_N1C1J1_N1C2J1ord$Genotype,     
                                  levels = c("N1C1J1.N1C2J1",
                                             "N1C1J1_PH25", "N1C1J1_PH36", "N1C1J1_PH31", "N1C1J1_PH39", 
                                             "N1C2J1_PH26", "N1C2J1_PH40", 
                                             "N1", "C1", "C2", "J1"))

Tkord <- Tk
Tkord$Genotype <- factor(Tkord$Genotype,     
                         levels = c("N1C1J1.N1C2J1",
                                    "N1C1J1_PH25", "N1C1J1_PH36", "N1C1J1_PH31", "N1C1J1_PH39", 
                                    "N1C2J1_PH26", "N1C2J1_PH40", 
                                    "N1", "C1", "C2", "J1"))

#boxplot seeds_per_pod N5C2J2
ggplot(comp_N1C1J1_N1C2J1ord, aes(Genotype, seeds_per_pod)) + 
  geom_boxplot() +
  labs(x="Genotype", y="Seeds/pod") +
  geom_text(data = Tkord, aes(x = Genotype, y = quant, label = cld), color = "black", size = 3.5, vjust = 0.3, hjust = 3) +
  coord_flip() +
  theme(axis.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title= element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8)) 


