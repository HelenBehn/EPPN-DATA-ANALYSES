# Comparison 1X: Resynth. B.napus

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
DATA <- read_csv("EPPN raw data complete 220419_corr genotype codes.csv")
View(DATA)

#drop first column
DATA = DATA[,2:37]
View(DATA)

#Comparison 1X	

# subsetting
comp_1X <- DATA[DATA$Genotype %in% c("R2", "R1", "O1", "O4", "N5", "N6", 
                                     "N1", "N4", "N7", "R204", "R103", "R102"), ]
View(comp_1X)

#create column giving generation###############################################
comp_1X <- comp_1X %>%
  mutate(Generation = case_when(
    Genotype == "R1" ~ "B. rapa (P1)",
    Genotype == "R2" ~ "B. rapa (P1)",
    Genotype == "O1" ~ "B. oleracea (P2)",
    Genotype == "O4" ~ "B. oleracea (P2)",
    Genotype == "R204" ~ "Resynth. B. napus",
    Genotype == "R103" ~ "Resynth. B. napus",
    Genotype == "R102" ~ "Resynth. B. napus",
    Genotype == "N1" ~ "Natural B. napus",
    Genotype == "N4" ~ "Natural B. napus",
    Genotype == "N5" ~ "Natural B. napus",
    Genotype == "N6" ~ "Natural B. napus",
    Genotype == "N7" ~ "Natural B. napus"))
view(comp_1X)


# Boxplot with CLD nach Rosane Rech##########################################################

# analysis of variance
anova <- aov(plant_height ~ Genotype, data = comp_1X)
summary(anova)

# Tukey's test
tukey <- TukeyHSD(anova)
print(tukey)

# compact letter display
cld <- multcompLetters4(anova, tukey)
print(cld)

# table with factors and 3rd quantile
Tk <- group_by(comp_1X, Genotype) %>%
  summarise(mean = round(mean(plant_height, na.rm = TRUE), digits = 1), quant = round(quantile(plant_height, probs = 0.75, na.rm = TRUE), digits = 1)) %>%
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
    Genotype == "R204" ~ "Resynth. B. napus",
    Genotype == "R103" ~ "Resynth. B. napus",
    Genotype == "R102" ~ "Resynth. B. napus",
    Genotype == "N1" ~ "Natural B. napus",
    Genotype == "N4" ~ "Natural B. napus",
    Genotype == "N5" ~ "Natural B. napus",
    Genotype == "N6" ~ "Natural B. napus",
    Genotype == "N7" ~ "Natural B. napus"))
view(ANOVA_Tukey_1X)

#Reordering
# Reorder factor levels in comp_1X and ANOVA_Tukey_1X, ohne Umbenennung
comp_1X$Genotype <- factor(comp_1X$Genotype,     
                             levels = c("N1", "N4", "N5", "N6", "N7", "R204", "R102", "R103", "O1", "O4", "R1", "R2"))


ANOVA_Tukey_1X$Genotype <- factor(ANOVA_Tukey_1X$Genotype,     
                                    levels = c("N1", "N4", "N5", "N6", "N7", "R204", "R102", "R103", "O1", "O4", "R1", "R2"))

#boxplot ## plant_height ###
ggplot(comp_1X, aes(Genotype, plant_height, fill = Generation, color = Generation)) + 
  geom_boxplot() +
  labs(x="Genotype", y="Plant height (cm)") +
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



