#Regular updates of R siftware
#load package
install.packages("installr")
library(installr)
#update
updateR()

# Comparison 2X4: Novel allohexaploids

#load packages 
install.packages("multcompView")
install.packages("viridis")
library(viridis)

library(tidyverse)
library(dplyr)
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
#N5C2J1_PH42 removed!!!######################################################## 
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
#N5C2J1_PH42 removed!!!##
comp_2X4ord <- comp_2X4                             
comp_2X4ord$Genotype <- factor(comp_2X4ord$Genotype,     
                             levels = c("N1C1J1.N1C2J1", "N1C1J1.N5C2J2", "N1C1J1. N6C2J2", 
                               "N5C2J2.N7C1J1", "N5C2J2.N6C2J2", "N5C2J2.N5C2J2", 
                               "N6C2J2.N4C2J1", "N6C2J2.N7C1J1", "N6C2J2.O1J3",
                               
                               "N1C1J1_PH25", "N1C1J1_PH36","N1C1J1_PH31", "N1C1J1_PH39", 
                               "N1C2J1_PH26", "N1C2J1_PH40", 
                               "N4C2J1_PH45","N4C2J1_PH27",
                               "N6C2J2_PH24", "N6C2J2_PH35", "N6C2J2_PH44", "N6C2J2_PH48", 
                               "N5C2J1_PH22", 
                               "N5C2J2_PH30", "N5C2J2_PH23", 
                               "N7C1J1",
                               
                               "J1", "J2", "C1", "C2", "N1", "N4", "N5", "N6", "N7"))

# ANOVA, Tukey & Boxplot with CLD of branch_number according to Rosane Rech##########################################################
# loading the appropriate libraries
library(datasets)

# analysis of variance
anova <- aov(branch_number ~ Genotype, data = comp_2X4ord)
summary(anova)

# Tukey's test
tukey <- TukeyHSD(anova)
print(tukey)

# compact letter display
cld <- multcompLetters4(anova, tukey)
print(cld)

# table with factors and 3rd quantile
Tk <- group_by(comp_2X4ord, Genotype) %>%
  summarise(mean = round(mean(branch_number), digits = 1), quant = round(quantile(branch_number, probs = 0.75), digits = 1)) %>%
  arrange(desc(mean)) #WICHTIG: arrange(desc(mean)) muss hier rein!!!

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$Genotype)
Tk$cld <- cld$Letters
print(Tk) # print dient der Überprüfung: Mittelwerte und Letter code 
# in der gleichen Reihenfolge angeordnet?


#create column giving generation###############################################
#N5C2J1_PH42 removed!!!########################################################
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

#boxplot branch_number
ggplot(comp_2X4ord, aes(Genotype, branch_number, fill = Generation, color = Generation)) + 
  geom_boxplot() +
  labs(x="Genotype", y="Branch number") +
  geom_text(data = ANOVA_Tukey_2X4, aes(x = Genotype, y = quant, label = cld), color = "black", size = 2.5, vjust = 0.3, hjust = 3) +
  coord_flip() +
  theme(axis.title = element_text(size = 14),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6)) +
  scale_color_viridis(discrete = TRUE, option = "D", direction = 1, guide = "none", alpha = 1) +  #legend removed by guide = "none" 
  scale_fill_viridis(discrete = TRUE, option = "D", direction = 1, alpha = 0.5,
                     breaks=c("P1", "P2", "P3", "novel allohex", "allohex hybrid"),
                     labels=c("P1", "P2", "P3", "novel allohexaploids", "allohexaploid hybrids")) ##reorder legend labels!


