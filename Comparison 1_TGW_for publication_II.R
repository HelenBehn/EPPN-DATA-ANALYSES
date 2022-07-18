# Comparison 1X: TGW_g in Resynth. B.napus

#install packages 
install.packages("multcompView")
install.packages("viridis")

#load
library(viridis)
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
DATA$Genotype[DATA$Genotype == "R102"] <- "R1O2"
DATA$Genotype[DATA$Genotype == "R103"] <- "R1O3"
DATA$Genotype[DATA$Genotype == "R204"] <- "R2O4"

# 3.Convert character to factor
DATA$Genotype <- as.factor(DATA$Genotype)   


#Comparison 1X	
# subsetting
comp_1X <- DATA[DATA$Genotype %in% c("R2", "R1", "O1", "O4", "N5", "N6", 
                                     "N1", "N4", "N7", "R2O4", "R1O3", "R1O2"), ]
View(comp_1X)

#create column giving generation###############################################
comp_1X <- comp_1X %>%
  mutate(Generation = case_when(
    Genotype == "R1" ~ "B. rapa (P1)",
    Genotype == "R2" ~ "B. rapa (P1)",
    Genotype == "O1" ~ "B. oleracea (P2)",
    Genotype == "O4" ~ "B. oleracea (P2)",
    Genotype == "R2O4" ~ "Resynth. B. napus",
    Genotype == "R1O3" ~ "Resynth. B. napus",
    Genotype == "R1O2" ~ "Resynth. B. napus",
    Genotype == "N1" ~ "Natural B. napus",
    Genotype == "N4" ~ "Natural B. napus",
    Genotype == "N5" ~ "Natural B. napus",
    Genotype == "N6" ~ "Natural B. napus",
    Genotype == "N7" ~ "Natural B. napus"))
view(comp_1X)


# Boxplot with CLD nach Rosane Rech##########################################################

# analysis of variance
anova <- aov(TGW_g ~ Genotype, data = comp_1X)
summary(anova)

# Tukey's test
tukey <- TukeyHSD(anova)
print(tukey)

# compact letter display
cld <- multcompLetters4(anova, tukey)
print(cld)

# table with factors and 3rd quantile
Tk <- group_by(comp_1X, Genotype) %>%
  summarise(mean = round(mean(TGW_g, na.rm = TRUE), digits = 1), quant = round(quantile(TGW_g, probs = 0.75, na.rm = TRUE), digits = 1)) %>%
  arrange(desc(mean)) #WICHTIG: arrange(desc(mean)) muss hier rein!!!

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$Genotype)
Tk$cld <- cld$Letters

#create column giving generation###############################################
ANOVA_Tukey_1X <- Tk %>%
  mutate(Generation = case_when(
    Genotype == "R1" ~ "B. rapa (P1)",
    Genotype == "R2" ~ "B. rapa (P1)",
    Genotype == "O1" ~ "B. oleracea (P2)",
    Genotype == "O4" ~ "B. oleracea (P2)",
    Genotype == "R2O4" ~ "Resynth. B. napus",
    Genotype == "R1O3" ~ "Resynth. B. napus",
    Genotype == "R1O2" ~ "Resynth. B. napus",
    Genotype == "N1" ~ "Natural B. napus",
    Genotype == "N4" ~ "Natural B. napus",
    Genotype == "N5" ~ "Natural B. napus",
    Genotype == "N6" ~ "Natural B. napus",
    Genotype == "N7" ~ "Natural B. napus"))
view(ANOVA_Tukey_1X)

#Reordering
# Reorder factor levels in comp_1X and ANOVA_Tukey_1X, ohne Umbenennung
comp_1X$Genotype <- factor(comp_1X$Genotype,     
                             levels = c("N1", "N4", "N5", "N6", "N7", "R2O4", "R1O2", "R1O3", "O1", "O4", "R1", "R2"))


ANOVA_Tukey_1X$Genotype <- factor(ANOVA_Tukey_1X$Genotype,     
                                    levels = c("N1", "N4", "N5", "N6", "N7", "R2O4", "R1O2", "R1O3", "O1", "O4", "R1", "R2"))

#boxplot ## TGW_g ###
ggplot(comp_1X, aes(Genotype, TGW_g, fill = Generation, color = Generation)) + 
  geom_boxplot() +
  labs(x="Genotype", y="Thousand seed weight (g)") +
  geom_text(data = ANOVA_Tukey_1X, aes(x = Genotype, y = quant, label = cld), color = "black", size = 2.5, vjust = 0.3, hjust = 3) +
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
                     breaks = c("B. rapa (P1)", "B. oleracea (P2)", "Resynth. B. napus", "Natural B. napus"),
                     labels = c("B. rapa (P1)", "B. oleracea (P2)", "Resynth. B. napus", "Natural B. napus"))   ##reorder legend labels!



