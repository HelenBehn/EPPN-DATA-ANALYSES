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
comp_N5C2J2 <- DATA[DATA$Genotype %in% c("N5", "C2", "J2", 
                                      
                                      "N5C2J2_PH30", "N5C2J2_PH23", 
                                     
                                     "N5C2J2.N5C2J2"), ]
head(comp_N5C2J2)
View(comp_N5C2J2)


# ANOVA, Tukey & Boxplot with CLD according to Rosane Rech##########################################################
# loading the appropriate libraries
library(datasets)

#remove previous objects
rm(anova)
rm(tukey)
rm(cld)
rm(Tk)

# analysis of variance
anova <- aov(first_flowers_DAS ~ Genotype, data = comp_N5C2J2)
summary(anova)

# Tukey's test
tukey <- TukeyHSD(anova)
print(tukey)

# compact letter display
cld <- multcompLetters4(anova, tukey)
print(cld)

# table with factors and 3rd quantile
Tk <- group_by(comp_N5C2J2, Genotype) %>%
  summarise(mean = round(mean(first_flowers_DAS, na.rm = TRUE), digits = 1), quant = round(quantile(first_flowers_DAS, probs = 0.75, na.rm = TRUE), digits = 1)) %>%
  arrange(desc(mean)) #WICHTIG: arrange(desc(mean)) muss hier rein!!!

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$Genotype)
Tk$cld <- cld$Letters
print(Tk) # print dient der Überprüfung: Mittelwerte und Letter code 
# in der gleichen Reihenfolge angeordnet?

# Reorder factor levels in comp_N5C2J2
comp_N5C2J2ord <- comp_N5C2J2                             
comp_N5C2J2ord$Genotype <- factor(comp_N5C2J2ord$Genotype,     
                                levels = c("N5C2J2.N5C2J2",
                                           "N5C2J2_PH30", "N5C2J2_PH23", 
                                           "N5", "C2", "J2"))

Tkord <- Tk
Tkord$Genotype <- factor(Tkord$Genotype,     
                                       levels = c("N5C2J2.N5C2J2",
                                                  "N5C2J2_PH30", "N5C2J2_PH23", 
                                                  "N5", "C2", "J2"))



#boxplot first_flowers_DAS N5C2J2
ggplot(comp_N5C2J2ord, aes(Genotype, first_flowers_DAS)) + 
  geom_boxplot() +
  labs(x="Genotype", y="First flowers (DAS)") +
  geom_text(data = Tkord, aes(x = Genotype, y = quant, label = cld), color = "black", size = 3.5, vjust = 0.3, hjust = 3) +
  coord_flip() +
  theme(axis.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title= element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8)) 


