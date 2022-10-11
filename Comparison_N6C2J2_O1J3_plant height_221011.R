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
comp_N6C2J2.O1J3 <- DATA[DATA$Genotype %in% c("N6", "C2", "J2", "O1", "J3",
                                                "N6C2J2_PH24", "N6C2J2_PH35", "N6C2J2_PH44", "N6C2J2_PH48", "O1J3_PH29", "O1J3_PH47",	
                                                "N6C2J2.O1J3"), ]
head(comp_N6C2J2.O1J3)
View(comp_N6C2J2.O1J3)


# ANOVA, Tukey & Boxplot with CLD according to Rosane Rech##########################################################
# loading the appropriate libraries
library(datasets)

#remove previous objects
rm(anova)
rm(tukey)
rm(cld)
rm(Tk)

# analysis of variance
anova <- aov(plant_height ~ Genotype, data = comp_N6C2J2.O1J3)
summary(anova)

# Tukey's test
tukey <- TukeyHSD(anova)
print(tukey)

# compact letter display
cld <- multcompLetters4(anova, tukey)
print(cld)

# table with factors and 3rd quantile
Tk <- group_by(comp_N6C2J2.O1J3, Genotype) %>%
  summarise(mean = round(mean(plant_height, na.rm = TRUE), digits = 1), quant = round(quantile(plant_height, probs = 0.75, na.rm = TRUE), digits = 1)) %>%
  arrange(desc(mean)) #WICHTIG: arrange(desc(mean)) muss hier rein!!!

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$Genotype)
Tk$cld <- cld$Letters
print(Tk) # print dient der Überprüfung: Mittelwerte und Letter code 
# in der gleichen Reihenfolge angeordnet?

# Reorder factor levels in comp_N6C2J2.O1J3
comp_N6C2J2.O1J3ord <- comp_N6C2J2.O1J3                             
comp_N6C2J2.O1J3ord$Genotype <- factor(comp_N6C2J2.O1J3ord$Genotype,     
                                  levels = c("N6C2J2.O1J3",
                                             "O1J3_PH29", "O1J3_PH47", "N6C2J2_PH24", "N6C2J2_PH35", "N6C2J2_PH44", "N6C2J2_PH48", 	
                                             "O1", "J3", "J2" , "C2", "N6"))

Tkord <- Tk
Tkord$Genotype <- factor(Tkord$Genotype,     
                         levels = c("N6C2J2.O1J3",
                                    "O1J3_PH29", "O1J3_PH47", "N6C2J2_PH24", "N6C2J2_PH35", "N6C2J2_PH44", "N6C2J2_PH48", 	
                                    "O1", "J3", "J2" , "C2", "N6"))

#boxplot plant_height
ggplot(comp_N6C2J2.O1J3ord, aes(Genotype, plant_height)) + 
  geom_boxplot() +
  labs(x="Genotype", y="Plant height (cm)") +
  geom_text(data = Tkord, aes(x = Genotype, y = quant, label = cld), color = "black", size = 3.5, vjust = 0.3, hjust = 3) +
  coord_flip() +
  theme(axis.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title= element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8)) 


