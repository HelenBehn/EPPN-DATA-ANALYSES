# Comparison 2X: Novel allohexaploids

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
setwd("C:/Users/Helen Behn/Documents/R/Data Analysis EPPN/00_raw_data/Final raw data table")
DATA <- read_csv("EPPN raw data complete 220419.csv")
View(DATA)


#drop first column
DATA = DATA[,2:37]
View(DATA)

#Comparison 2X	
# Aim: to determine if the NCJ allohexaploids have more similarity to each other than to their parent genotypes 
# Genotypes: PH22	PH23	PH24	PH25	PH26	PH27	PH28	PH42	PH30	PH35	PH36	PH40	PH45	PH44	
# PH31	PH48	PH39	PH12	PH13	PH14	PH15	PH16	PH07	PH08	PH09	PH10

#### subsetting #####
#IMPORTANT: N5C2J2.N5C2J1_PH43 has only one parent genotype: N5C2J2 (PH30 and PH23), 
# N5C2J1_PH42 was wrong, deleted from subsetting group 4

comp_2X4 <- DATA[DATA$Genotype %in% c("N6C2J2_PH24", "N6C2J2_PH35", "N6C2J2_PH44", "N6C2J2_PH48", 
                                     "N5C2J1_PH22", "N5C2J2_PH30", "N5C2J2_PH23", "N1C1J1_PH25", "N1C1J1_PH36", 
                                     "N1C1J1_PH31", "N1C1J1_PH39", "N1C2J1_PH26", "N1C2J1_PH40", "N4C2J1_PH45","N4C2J1_PH27", "N7C1J1", 
                                     
                                     "N5", "N6", "N1", "N4", "N7","C1", "C2", "J1", "J2", 
                                     
                                     "N1C1J1.N5C2J2", "N6C2J2.N7C1J1", "N5C2J2.N7C1J1", "N1C1J1. N6C2J2",
                                     "N5C2J2.N6C2J2","N1C1J1.N1C2J1", "N5C2J2.N5C2J2", "N6C2J2.N4C2J1", "N6C2J2.O1J3"), ]
head(comp_2X4)
View(comp_2X4)

#create column giving generation###############################################
comp_2X4 <- comp_2X4 %>%
  mutate(Generation = case_when(
  Genotype == "N1" ~ "P1",
  Genotype == "N4" ~ "P1",
  Genotype == "N5" ~ "P1",
  Genotype == "N6" ~ "P1",
  Genotype == "N7" ~ "P1",
  Genotype == "C1" ~ "P2",
  Genotype == "C2" ~ "P2",
  Genotype == "J1" ~ "P3",
  Genotype == "J2" ~ "P3",
  Genotype == "N5C2J1_PH22" ~ "novel allohex",
  Genotype == "N5C2J2_PH23" ~ "novel allohex",
  Genotype == "N6C2J2_PH24" ~ "novel allohex",
  Genotype == "N1C1J1_PH25" ~ "novel allohex",
  Genotype == "N1C2J1_PH26" ~ "novel allohex",
  Genotype == "N4C2J1_PH27" ~ "novel allohex",
  Genotype == "N5C2J2_PH30" ~ "novel allohex",
  Genotype == "N1C1J1_PH31" ~ "novel allohex",
  Genotype == "N6C2J2_PH35" ~ "novel allohex",
  Genotype == "N1C1J1_PH36" ~ "novel allohex",
  Genotype == "N1C1J1_PH39" ~ "novel allohex",
  Genotype == "N1C2J1_PH40" ~ "novel allohex",
  Genotype == "N5C2J1_PH42" ~ "novel allohex",
  Genotype == "N6C2J2_PH44" ~ "novel allohex",
  Genotype == "N4C2J1_PH45" ~ "novel allohex",
  Genotype == "N6C2J2_PH48" ~ "novel allohex",
  Genotype == "N7C1J1" ~ "novel allohex",
  Genotype == "N1C1J1.N5C2J2" ~ "allohex hybrid",
  Genotype == "N1C1J1.N1C2J1" ~ "allohex hybrid",
  Genotype == "N5C2J2.N7C1J1" ~ "allohex hybrid",
  Genotype == "N5C2J2.N5C2J2" ~ "allohex hybrid",
  Genotype == "N1C1J1. N6C2J2" ~ "allohex hybrid",
  Genotype == "N5C2J2.N6C2J2" ~ "allohex hybrid",
  Genotype == "N6C2J2.O1J3" ~ "allohex hybrid",
  Genotype == "N6C2J2.N4C2J1" ~ "allohex hybrid",
  Genotype == "N6C2J2.N7C1J1" ~ "allohex hybrid"))


# Reorder factor levels in comp_2X4
comp_2X4ord <- comp_2X4                             
comp_2X4ord$Genotype <- factor(comp_2X4ord$Genotype,     
                             levels = c("N1C1J1.N1C2J1", "N1C1J1.N5C2J2", "N1C1J1. N6C2J2", 
                               "N5C2J2.N7C1J1", "N5C2J2.N6C2J2", "N5C2J2.N5C2J2", 
                               "N6C2J2.N4C2J1", "N6C2J2.N7C1J1", "N6C2J2.O1J3",
                               
                               "N1C1J1_PH25", "N1C1J1_PH36","N1C1J1_PH31", "N1C1J1_PH39", 
                               "N1C2J1_PH26", "N1C2J1_PH40", 
                               "N4C2J1_PH45","N4C2J1_PH27",
                               "N6C2J2_PH24", "N6C2J2_PH35", "N6C2J2_PH44", "N6C2J2_PH48", 
                               "N5C2J1_PH42", "N5C2J1_PH22", 
                               "N5C2J2_PH30", "N5C2J2_PH23", 
                               "N7C1J1",
                               
                               "J1", "J2", "C1", "C2", "N1", "N4", "N5", "N6", "N7"))

# Boxplot with CLD nach Rosane Rech##########################################################
# loading the appropriate libraries
library(datasets)
library(ggplot2)
library(multcompView)
library(dplyr)

# analysis of variance
anova <- aov(plant_height ~ Genotype, data = comp_2X4ord)
summary(anova)

# Tukey's test
tukey <- TukeyHSD(anova)
print(tukey)

# compact letter display
cld <- multcompLetters4(anova, tukey)
print(cld)

# table with factors and 3rd quantile
Tk <- group_by(comp_2X4ord, Genotype) %>%
  summarise(mean=mean(plant_height), quant = quantile(plant_height, probs = 0.75)) 

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$Genotype)
Tk$cld <- cld$Letters

#create column giving generation###############################################
ANOVA_Tukey_2X4 <- Tk %>%
  mutate(Generation = case_when(
    Genotype == "N1" ~ "P1",
    Genotype == "N4" ~ "P1",
    Genotype == "N5" ~ "P1",
    Genotype == "N6" ~ "P1",
    Genotype == "N7" ~ "P1",
    Genotype == "C1" ~ "P2",
    Genotype == "C2" ~ "P2",
    Genotype == "J1" ~ "P3",
    Genotype == "J2" ~ "P3",
    Genotype == "N5C2J1_PH22" ~ "novel allohex",
    Genotype == "N5C2J2_PH23" ~ "novel allohex",
    Genotype == "N6C2J2_PH24" ~ "novel allohex",
    Genotype == "N1C1J1_PH25" ~ "novel allohex",
    Genotype == "N1C2J1_PH26" ~ "novel allohex",
    Genotype == "N4C2J1_PH27" ~ "novel allohex",
    Genotype == "N5C2J2_PH30" ~ "novel allohex",
    Genotype == "N1C1J1_PH31" ~ "novel allohex",
    Genotype == "N6C2J2_PH35" ~ "novel allohex",
    Genotype == "N1C1J1_PH36" ~ "novel allohex",
    Genotype == "N1C1J1_PH39" ~ "novel allohex",
    Genotype == "N1C2J1_PH40" ~ "novel allohex",
    Genotype == "N5C2J1_PH42" ~ "novel allohex",
    Genotype == "N6C2J2_PH44" ~ "novel allohex",
    Genotype == "N4C2J1_PH45" ~ "novel allohex",
    Genotype == "N6C2J2_PH48" ~ "novel allohex",
    Genotype == "N7C1J1" ~ "novel allohex",
    Genotype == "N1C1J1.N5C2J2" ~ "allohex hybrid",
    Genotype == "N1C1J1.N1C2J1" ~ "allohex hybrid",
    Genotype == "N5C2J2.N7C1J1" ~ "allohex hybrid",
    Genotype == "N5C2J2.N5C2J2" ~ "allohex hybrid",
    Genotype == "N1C1J1. N6C2J2" ~ "allohex hybrid",
    Genotype == "N5C2J2.N6C2J2" ~ "allohex hybrid",
    Genotype == "N6C2J2.O1J3" ~ "allohex hybrid",
    Genotype == "N6C2J2.N4C2J1" ~ "allohex hybrid",
    Genotype == "N6C2J2.N7C1J1" ~ "allohex hybrid"))
view(ANOVA_Tukey_2X4)


# boxplot plant_height##############################################################
#Input-objekte:
view(ANOVA_Tukey_2X4)
view(comp_2X4ord)

#ohne facet_wrap ###############
ggplot(comp_2X4ord, aes(Genotype, plant_height, color = Generation)) + 
  geom_boxplot() +
  labs(x="Genotype", y="plant_height (cm)") +
  geom_text(data = ANOVA_Tukey_2X4, aes(x = Genotype, y = quant, label = cld), color = "black", size = 2, vjust = 0.3, hjust = 7) +
  coord_flip() +
  theme(axis.title = element_text(size = 14),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6)) +
  scale_color_viridis(discrete = TRUE, option = "D") 


#mit facet_wrap##############
# Relevel group factor (Generation)
comp_2X4ord$Generation <- factor(comp_2X4ord$Generation,                 
                              levels = c("P1", "P2", "P3", "novel allohex", "allohex hybrid"))
ANOVA_Tukey_2X4$Generation <- factor(ANOVA_Tukey_2X4$Generation,                 
                                 levels = c("P1", "P2", "P3", "novel allohex", "allohex hybrid"))

ggplot(comp_2X4ord, aes(Genotype, plant_height, color = Generation)) + 
  geom_boxplot() +
  labs(x="Genotype", y="plant_height (cm)") +
  geom_text(data = ANOVA_Tukey_2X4, aes(x = Genotype, y = quant, label = cld), size = 2, vjust = 0.3, hjust = 3.5) +
  coord_flip() +
  theme(axis.title = element_text(size = 14),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6)) +
facet_wrap(~ Generation, nrow = 5, scales = "free_y") 




# test test test farben ##################################################################
ggplot(data, aes(x, y, fill = x)) +     # Manually specifying colors
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("A" = "#353436",
                               "B" = "#1b98e0",
                               "C" = "red",
                               "D" = "red",
                               "E" = "green"))


ggplot(comp_2X4ord, aes(Genotype, plant_height, fill = Genotype)) + 
  geom_boxplot() +
  labs(x="Genotype", y="plant_height (cm)") +
  geom_text(data = ANOVA_Tukey_2X4, aes(x = Genotype, y = quant, label = cld), size = 2, vjust = 0.3, hjust = 3.5) +
  coord_flip() +
  theme(axis.title = element_text(size = 14),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6)) +
  scale_fill_manual(values = c("J1"="deepskyblue", 
                               "J2" = "darkblue", 
                               "C1" = "#FFCC33", 
                               "C2" = "#FFFF99", 
                               "N1" = "#660000", 
                               "N4" = "#CC0000", 
                               "N5" = "#FF3333", 
                               "N6" = "#FF3399", 
                               "N7" = "#FF99CC")) +
  facet_wrap(~ Generation, nrow = 5, scales = "free_y") 

"N1C1J1.N1C2J1", "N1C1J1.N5C2J2", "N1C1J1. N6C2J2", 
"N5C2J2.N7C1J1", "N5C2J2.N6C2J2", "N5C2J2.N5C2J2", 
"N6C2J2.N4C2J1", "N6C2J2.N7C1J1", "N6C2J2.O1J3",
"N1C1J1_PH25", "N1C1J1_PH36","N1C1J1_PH31", "N1C1J1_PH39", 
"N1C2J1_PH26", "N1C2J1_PH40", 
"N4C2J1_PH45","N4C2J1_PH27",
"N6C2J2_PH24", "N6C2J2_PH35", "N6C2J2_PH44", "N6C2J2_PH48", 
"N5C2J1_PH42", "N5C2J1_PH22", 
"N5C2J2_PH30", "N5C2J2_PH23", 
"N7C1J1"
