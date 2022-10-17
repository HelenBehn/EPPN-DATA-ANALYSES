# Comparison 1X: Resynth. B.napus

#install packages 
install.packages("multcompView")
install.packages("viridis")
install.packages("viridisLite")
install.packages("viridis_pal")
#load
library(viridis)
library(viridis_pal)
library(viridisLite)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(datasets)
library(multcompView)

# vollständige Rohdatentabelle laden ##########################################
#Im Projekt EPPN-DATA-ANALYSES ist dies das wd. Kann nicht geändert werden. 
# Daher Rohdatentabelle direkt von dort laden. 
getwd()
DATA <- read_csv("EPPN raw data complete 220419.csv")

#drop first column
DATA = DATA[,2:37]
View(DATA)

str(DATA)

#Correct genotype names of synth. B. napus (was not possible in raw data table without destroying formatting)

# 1.Convert factor to character (nicht nötig, weil Character)
#DATA$Genotype <- as.character(DATA$Genotype) 

# 2. Correct genotype names
DATA$Genotype[DATA$Genotype == "R102"] <- "A4C36"
DATA$Genotype[DATA$Genotype == "R103"] <- "A4C37"
DATA$Genotype[DATA$Genotype == "R204"] <- "A16C46"

DATA$Genotype[DATA$Genotype == "R1"] <- "A04"
DATA$Genotype[DATA$Genotype == "R2"] <- "A16"

DATA$Genotype[DATA$Genotype == "O1"] <- "O1"
DATA$Genotype[DATA$Genotype == "O4"] <- "C46"
print(DATA$Genotype)
View(DATA)

# 3.Convert character to factor
DATA$Genotype <- as.factor(DATA$Genotype)   


#Comparison 1X	
# subsetting
comp_1X <- DATA[DATA$Genotype %in% c("A16", "A04", "O1", "C46", "N5", "N6", 
                                     "N1", "N4", "N7", "A16C46", "A4C37", "A4C36"), ]
View(comp_1X)

#create column giving generation###############################################
comp_1X <- comp_1X %>%
  mutate(Type = case_when(
    Genotype == "A04" ~ "B. rapa (P1)",
    Genotype == "A16" ~ "B. rapa (P1)",
    Genotype == "O1" ~ "B. oleracea (P2)",
    Genotype == "C46" ~ "B. oleracea (P2)",
    Genotype == "A16C46" ~ "Resynth. B. napus",
    Genotype == "A4C37" ~ "Resynth. B. napus",
    Genotype == "A4C36" ~ "Resynth. B. napus",
    Genotype == "N1" ~ "Natural B. napus",
    Genotype == "N4" ~ "Natural B. napus",
    Genotype == "N5" ~ "Natural B. napus",
    Genotype == "N6" ~ "Natural B. napus",
    Genotype == "N7" ~ "Natural B. napus"))
view(comp_1X)


# Boxplot with CLD nach Rosane Rech##########################################################
#remove
rm(anova)
rm(tukey)
rm(Tk)
rm(cld)
rm(ANOVA_Tukey_1X)

# analysis of variance
anova <- aov(seeds_per_pod ~ Genotype, data = comp_1X)
summary(anova)

# Tukey's test
tukey <- TukeyHSD(anova)
print(tukey)

# compact letter display
cld <- multcompLetters4(anova, tukey)
print(cld)

# table with factors and 3rd quantile
Tk <- group_by(comp_1X, Genotype) %>%
  summarise(mean = round(mean(seeds_per_pod, na.rm = TRUE), digits = 1), quant = round(quantile(seeds_per_pod, probs = 0.75, na.rm = TRUE), digits = 1)) %>%
  arrange(desc(mean)) #WICHTIG: arrange(desc(mean)) muss hier rein!!!

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$Genotype)
Tk$cld <- cld$Letters

#create column giving generation###############################################
ANOVA_Tukey_1X <- Tk %>%
  mutate(Type = case_when(
    Genotype == "A04" ~ "B. rapa (P1)",
    Genotype == "A16" ~ "B. rapa (P1)",
    Genotype == "O1" ~ "B. oleracea (P2)",
    Genotype == "C46" ~ "B. oleracea (P2)",
    Genotype == "A16C46" ~ "Resynth. B. napus",
    Genotype == "A4C37" ~ "Resynth. B. napus",
    Genotype == "A4C36" ~ "Resynth. B. napus",
    Genotype == "N1" ~ "Natural B. napus",
    Genotype == "N4" ~ "Natural B. napus",
    Genotype == "N5" ~ "Natural B. napus",
    Genotype == "N6" ~ "Natural B. napus",
    Genotype == "N7" ~ "Natural B. napus"))
view(ANOVA_Tukey_1X)

#Reordering
# Reorder factor levels in comp_1X and ANOVA_Tukey_1X, ohne Umbenennung
comp_1X$Genotype <- factor(comp_1X$Genotype,     
                             levels = c("N1", "N4", "N5", "N6", "N7", "A16C46", "A4C36", "A4C37", "O1", "C46", "A04", "A16"))


ANOVA_Tukey_1X$Genotype <- factor(ANOVA_Tukey_1X$Genotype,     
                                    levels = c("N1", "N4", "N5", "N6", "N7", "A16C46", "A4C36", "A4C37", "O1", "C46", "A04", "A16"))


  
####SO GEHT'S!!! NUR ALPHA-LEGENDE LÖSCHEN   ###
ggplot(comp_1X, aes(Genotype, seeds_per_pod, fill = Type, color = Type, alpha = 0.5)) + 
  geom_boxplot() +
  labs(x="Genotype", y="Seeds/pod") +
  geom_text(data = ANOVA_Tukey_1X, aes(x = Genotype, y = quant, label = cld), color = "black", size = 3.5, vjust = 0.3, hjust = 3, alpha = 1) +
  coord_flip() +
  theme(axis.title = element_text(size = 14),
        legend.text = element_text(size = 8),
        legend.title= element_blank(),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8)) +
  scale_color_manual(aesthetics = c("colour", "fill"), values = c("#440154FF", "#1F968BFF", "#FDE725FF", "#287D8EFF"), 
                     breaks = c("B. rapa (P1)", "B. oleracea (P2)", "Resynth. B. napus", "Natural B. napus"),
                     labels = c("B. rapa (P1)", "B. oleracea (P2)", "Resynth. B. napus", "Natural B. napus")) 
  

                    
