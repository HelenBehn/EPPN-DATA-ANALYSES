# Comparison 2XY: Novel allohexaploids

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
getwd()
DATA <- read_csv("EPPN raw data complete 220419.csv")
View(DATA)


#drop first column
DATA = DATA[,2:37]
View(DATA)

#Comparison 2XY	einschließlich JO allohexaploids
# Aim: to determine if the NCJ allohexaploids have more similarity to each other than to their parent genotypes 

#### subsetting #####
#IMPORTANT: N5C2J2.N5C2J1_PH43 has only one parent genotype: N5C2J2 (PH30 and PH23), 
# N5C2J1_PH42 was wrong, deleted from subsetting group 4

comp_2XY4 <- DATA[DATA$Genotype %in% c("N5", "N6", "N1", "N4", "N7","C1", "C2", "J1", "J2", 
                                      "O1", "J3",
                                      
                                      "N6C2J2_PH24", "N6C2J2_PH35", "N6C2J2_PH44", "N6C2J2_PH48", 
                                     "N5C2J1_PH22", "N5C2J2_PH30", "N5C2J2_PH23", "N1C1J1_PH25", "N1C1J1_PH36", 
                                     "N1C1J1_PH31", "N1C1J1_PH39", "N1C2J1_PH26", "N1C2J1_PH40", "N4C2J1_PH45",
                                     "N4C2J1_PH27", "N7C1J1",
                                     "O1J3_PH29", "O1J3_PH47",
                                     
                                     "N1C1J1.N5C2J2", "N6C2J2.N7C1J1", "N5C2J2.N7C1J1", "N1C1J1. N6C2J2",
                                     "N5C2J2.N6C2J2","N1C1J1.N1C2J1", "N5C2J2.N5C2J2", "N6C2J2.N4C2J1", "N6C2J2.O1J3"), ]
head(comp_2XY4)
View(comp_2XY4)

#create column giving generation###############################################
#N5C2J1_PH42 removed!!!######################################################## 
comp_2XY4 <- comp_2XY4 %>%
  mutate(Generation = case_when(
  Genotype == "O1" ~ "P1",
  Genotype == "J3" ~ "P2",
  Genotype == "N1" ~ "P1",
  Genotype == "N4" ~ "P1",
  Genotype == "N5" ~ "P1",
  Genotype == "N6" ~ "P1",
  Genotype == "N7" ~ "P1",
  Genotype == "C1" ~ "P2",
  Genotype == "C2" ~ "P2",
  Genotype == "J1" ~ "P3",
  Genotype == "J2" ~ "P3",
  Genotype == "O1J3_PH29" ~ "novel allohexaploid",
  Genotype == "O1J3_PH47" ~ "novel allohexaploid",
  Genotype == "N5C2J1_PH22" ~ "novel allohexaploid",
  Genotype == "N5C2J2_PH23" ~ "novel allohexaploid",
  Genotype == "N6C2J2_PH24" ~ "novel allohexaploid",
  Genotype == "N1C1J1_PH25" ~ "novel allohexaploid",
  Genotype == "N1C2J1_PH26" ~ "novel allohexaploid",
  Genotype == "N4C2J1_PH27" ~ "novel allohexaploid",
  Genotype == "N5C2J2_PH30" ~ "novel allohexaploid",
  Genotype == "N1C1J1_PH31" ~ "novel allohexaploid",
  Genotype == "N6C2J2_PH35" ~ "novel allohexaploid",
  Genotype == "N1C1J1_PH36" ~ "novel allohexaploid",
  Genotype == "N1C1J1_PH39" ~ "novel allohexaploid",
  Genotype == "N1C2J1_PH40" ~ "novel allohexaploid",
  Genotype == "N6C2J2_PH44" ~ "novel allohexaploid",
  Genotype == "N4C2J1_PH45" ~ "novel allohexaploid",
  Genotype == "N6C2J2_PH48" ~ "novel allohexaploid",
  Genotype == "N7C1J1" ~ "novel allohexaploid",
  Genotype == "N1C1J1.N5C2J2" ~ "allohexaploid hybrid",
  Genotype == "N1C1J1.N1C2J1" ~ "allohexaploid hybrid",
  Genotype == "N5C2J2.N7C1J1" ~ "allohexaploid hybrid",
  Genotype == "N5C2J2.N5C2J2" ~ "allohexaploid hybrid",
  Genotype == "N1C1J1. N6C2J2" ~ "allohexaploid hybrid",
  Genotype == "N5C2J2.N6C2J2" ~ "allohexaploid hybrid",
  Genotype == "N6C2J2.O1J3" ~ "allohexaploid hybrid",
  Genotype == "N6C2J2.N4C2J1" ~ "allohexaploid hybrid",
  Genotype == "N6C2J2.N7C1J1" ~ "allohexaploid hybrid"))
view(comp_2XY4)

# ANOVA, Tukey & Boxplot with CLD according to Rosane Rech##########################################################
# loading the appropriate libraries
library(datasets)

# analysis of variance
anova <- aov(TGW_g ~ Genotype, data = comp_2XY4)
summary(anova)

# Tukey's test
tukey <- TukeyHSD(anova)
print(tukey)

# compact letter display
cld <- multcompLetters4(anova, tukey)
print(cld)

# table with factors and 3rd quantile
Tk <- group_by(comp_2XY4, Genotype) %>%
  summarise(mean = round(mean(TGW_g, na.rm = TRUE), digits = 1), quant = round(quantile(TGW_g, probs = 0.75, na.rm = TRUE), digits = 1)) %>%
  arrange(desc(mean)) #WICHTIG: arrange(desc(mean)) muss hier rein!!!

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$Genotype)
Tk$cld <- cld$Letters
print(Tk) # print dient der Überprüfung: Mittelwerte und Letter code 
# in der gleichen Reihenfolge angeordnet?


#create column giving generation###############################################
#N5C2J1_PH42 removed!!!########################################################
ANOVA_Tukey_2XY4 <- Tk %>%
  mutate(Generation = case_when(
    Genotype == "O1" ~ "P1",
    Genotype == "J3" ~ "P2",
    Genotype == "N1" ~ "P1",
    Genotype == "N4" ~ "P1",
    Genotype == "N5" ~ "P1",
    Genotype == "N6" ~ "P1",
    Genotype == "N7" ~ "P1",
    Genotype == "C1" ~ "P2",
    Genotype == "C2" ~ "P2",
    Genotype == "J1" ~ "P3",
    Genotype == "J2" ~ "P3",
    Genotype == "O1J3_PH29" ~ "novel allohexaploid",
    Genotype == "O1J3_PH47" ~ "novel allohexaploid",
    Genotype == "N5C2J1_PH22" ~ "novel allohexaploid",
    Genotype == "N5C2J2_PH23" ~ "novel allohexaploid",
    Genotype == "N6C2J2_PH24" ~ "novel allohexaploid",
    Genotype == "N1C1J1_PH25" ~ "novel allohexaploid",
    Genotype == "N1C2J1_PH26" ~ "novel allohexaploid",
    Genotype == "N4C2J1_PH27" ~ "novel allohexaploid",
    Genotype == "N5C2J2_PH30" ~ "novel allohexaploid",
    Genotype == "N1C1J1_PH31" ~ "novel allohexaploid",
    Genotype == "N6C2J2_PH35" ~ "novel allohexaploid",
    Genotype == "N1C1J1_PH36" ~ "novel allohexaploid",
    Genotype == "N1C1J1_PH39" ~ "novel allohexaploid",
    Genotype == "N1C2J1_PH40" ~ "novel allohexaploid",
    Genotype == "N6C2J2_PH44" ~ "novel allohexaploid",
    Genotype == "N4C2J1_PH45" ~ "novel allohexaploid",
    Genotype == "N6C2J2_PH48" ~ "novel allohexaploid",
    Genotype == "N7C1J1" ~ "novel allohexaploid",
    Genotype == "N1C1J1.N5C2J2" ~ "allohexaploid hybrid",
    Genotype == "N1C1J1.N1C2J1" ~ "allohexaploid hybrid",
    Genotype == "N5C2J2.N7C1J1" ~ "allohexaploid hybrid",
    Genotype == "N5C2J2.N5C2J2" ~ "allohexaploid hybrid",
    Genotype == "N1C1J1. N6C2J2" ~ "allohexaploid hybrid",
    Genotype == "N5C2J2.N6C2J2" ~ "allohexaploid hybrid",
    Genotype == "N6C2J2.O1J3" ~ "allohexaploid hybrid",
    Genotype == "N6C2J2.N4C2J1" ~ "allohexaploid hybrid",
    Genotype == "N6C2J2.N7C1J1" ~ "allohexaploid hybrid"))

view(ANOVA_Tukey_2XY4)

# Reorder factor levels in comp_2XY4
#N5C2J1_PH42 removed!!!##
comp_2XY4ord <- comp_2XY4                             
comp_2XY4ord$Genotype <- factor(comp_2XY4ord$Genotype,     
                                levels = c("N1C1J1.N1C2J1", "N1C1J1.N5C2J2", "N1C1J1. N6C2J2", 
                                           "N5C2J2.N7C1J1", "N5C2J2.N6C2J2", "N5C2J2.N5C2J2", 
                                           "N6C2J2.N4C2J1", "N6C2J2.N7C1J1", "N6C2J2.O1J3",
                                           
                                           "O1J3_PH29", "O1J3_PH47",
                                           "N1C1J1_PH25", "N1C1J1_PH36","N1C1J1_PH31", "N1C1J1_PH39", 
                                           "N1C2J1_PH26", "N1C2J1_PH40", 
                                           "N4C2J1_PH45","N4C2J1_PH27",
                                           "N6C2J2_PH24", "N6C2J2_PH35", "N6C2J2_PH44", "N6C2J2_PH48", 
                                           "N5C2J1_PH22", 
                                           "N5C2J2_PH30", "N5C2J2_PH23", 
                                           "N7C1J1",
                                           
                                           "J1", "J2", "J3", "C1", "C2", "O1", "N1", "N4", "N5", "N6", "N7"))

ANOVA_Tukey_2XY4ord <- ANOVA_Tukey_2XY4
ANOVA_Tukey_2XY4ord$Genotype <- factor(ANOVA_Tukey_2XY4ord$Genotype,     
                                  levels = c("N1C1J1.N1C2J1", "N1C1J1.N5C2J2", "N1C1J1. N6C2J2", 
                                             "N5C2J2.N7C1J1", "N5C2J2.N6C2J2", "N5C2J2.N5C2J2", 
                                             "N6C2J2.N4C2J1", "N6C2J2.N7C1J1", "N6C2J2.O1J3",
                                             
                                             "O1J3_PH29", "O1J3_PH47",
                                             "N1C1J1_PH25", "N1C1J1_PH36","N1C1J1_PH31", "N1C1J1_PH39", 
                                             "N1C2J1_PH26", "N1C2J1_PH40", 
                                             "N4C2J1_PH45","N4C2J1_PH27",
                                             "N6C2J2_PH24", "N6C2J2_PH35", "N6C2J2_PH44", "N6C2J2_PH48", 
                                             "N5C2J1_PH22", 
                                             "N5C2J2_PH30", "N5C2J2_PH23", 
                                             "N7C1J1",
                                             
                                             "J1", "J2", "J3", "C1", "C2", "O1", "N1", "N4", "N5", "N6", "N7"))


#boxplot
ggplot(comp_2XY4ord, aes(Genotype, TGW_g, fill = Generation, color = Generation)) + 
  geom_boxplot() +
  labs(x="Genotype", y="Thousand seed weight (g)") +
  geom_text(data = ANOVA_Tukey_2XY4, aes(x = Genotype, y = quant, label = cld), color = "black", size = 2.5, vjust = 0.3, hjust = 3) +
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


