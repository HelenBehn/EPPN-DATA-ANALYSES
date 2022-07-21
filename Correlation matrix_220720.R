#install
install.packages("corrplot")
library(corrplot)

#load
library(knitr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(purrr)
library(tibble)


# vollständige Rohdatentabelle laden ##########################################
#Im Projekt EPPN-DATA-ANALYSES ist dies das wd. Kann nicht geändert werden. 
# Daher Rohdatentabelle direkt von dort laden. 
getwd()
DATA <- read_csv("EPPN raw data complete 220419.csv")

#drop columns
DATA = DATA[,-c(1:2, 4:5)]
View(DATA)

#create column giving accessions/lines
DATA <- DATA %>%
  mutate(Accession = case_when(
    Genotype == "R1" ~ "B. rapa",
    Genotype == "R2" ~ "B. rapa",
    Genotype == "O1" ~ "B. oleracea",
    Genotype == "O4" ~ "B. oleracea",
    
    Genotype == "R204" ~ "Resynth. B. napus", #ACHTUNG: in Rohdatentabelle "0" statt "O"
    Genotype == "R103" ~ "Resynth. B. napus",
    Genotype == "R102" ~ "Resynth. B. napus",
    Genotype == "N1" ~ "Natural B. napus",
    Genotype == "N4" ~ "Natural B. napus",
    Genotype == "N5" ~ "Natural B. napus",
    Genotype == "N6" ~ "Natural B. napus",
    Genotype == "N7" ~ "Natural B. napus",
    Genotype == "C1" ~ "B. carinata",
    Genotype == "C2" ~ "B. carinata",
    Genotype == "J1" ~ "B. juncea",
    Genotype == "J2" ~ "B. juncea",
    Genotype == "J3" ~ "B. juncea",
    
    Genotype == "J1C1" ~ "JC hybrid",
    Genotype == "J1C2" ~ "JC hybrid",
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
    Genotype == "N6C2J2.N7C1J1" ~ "allohexaploid hybrid"
    ))

#Reorder traits
#1. Accession nach vorne
DATA = DATA[, c(1, 34, 2:33)]
View(DATA)
#2. Group traits as growth, development and fertility traits
DATA = DATA[, c(1:4, 18:29, 33:34, 10:17, 5:9, 30:32)]
View(DATA)

#Rename traits
DATA_renamed <- DATA %>% rename(Plant_height = plant_height,
                Branch_number = branch_number,
                Pod_weight_plant = pod_weight_plant,
                Pod_number_branch = main,
                Pods_per_plant = pods_plant,
                Pods_per_branch = pods_branch,
                Single_pod_weight = weight_pod,
                First_flowers_DAS = first_flowers_DAS,
                Maximum_flower_pixels_DAS = peak_flowers_DAS,
                Flowering_days_to_peak = flowering_days_to_peak,
                Total_flowering_days = total_flowering_days,
                Pixel_number_at_peak_flowering = pixel.day_peak,
                Sum_of_flower_pixels_to_peak = pixel.days_to_peak,
                Total_pixel_days = total_pixel.days,
                Mean_pixels_to_peak_flowering = mean_pixel.days_to_peak,
                Start_of_shoot_extension_DAS = R01_raceme_start,
                Mean_shoot_extension_rate =  R02_mean_rate,
                Max_shoot_extension_rate = R03_max_rate,
                DAS_of_max_shoot_extension_rate = R04_DAS_of_max_rate,
                Days_to_90_percent_max_height = R05_main_ext_period, 
                Days_to_50_percent_max_height = R06_50ht_period,
                Days_to_75_percent_max_height = R07_75ht_period, 
                Growth_rate_50_percent_max = R08_50ht_rate,
                Growth_rate_75_percent_max = R09_75ht_rate,
                Maximum_plant_height = R10_max_height, 
                DAS_of_max_plant_height = R11_max_height_DAS,
                Extension_days_to_max = R12_extension_days_to_max,
                Seed_weight_10_pods_g = "seed wt_10p_g", 
                Seeds_per_pod = seeds_per_pod,
                Thousand_seed_weight_g = TGW_g)
view(DATA_renamed)

# correlation matrix across all genotypes
# cor function
str(DATA)
cor(DATA[,2:33], DATA[,2:33], method = "pearson") #geht nur für numerische Variablen
cor(DATA_renamed[,3:34], DATA_renamed[,3:34], method = "pearson")

#corr.test
install.packages("psych")
library(psych)

corr.test(DATA[,2:33], DATA[,2:33], method = "pearson")
Corr_coeff_p <- corr.test(DATA_renamed[,3:34], DATA_renamed[,3:34], method = "pearson")

# correlation matrix of complete data set by genotype
#drop first column
DATA %>% 
  group_by(Genotype) %>%
  do(data.frame(Cor=t(cor(.[,2:33], .[,2:33]))))

#visualization of bivariate relationships
install.packages("GGally")
library(GGally)
ggpairs(DATA[,2:33])

#visualization by genotype
DATA %>% 
  group_by(Genotype) %>%
  do(data.frame(ggpairs(DATA[,2:33]))) #geht so nicht

#Subsetting ###
typeof(DATA_renamed$Accession)
DATA_renamed$Accession <- as.factor(DATA_renamed$Accession)
DATA_renamed$Genotype <- as.factor(DATA_renamed$Genotype)
str(DATA_renamed)

#subsetting
data_new1 <- data[data$x1 == "A", ]  

Nov_Allohex <- DATA_renamed[DATA_renamed$Accession == "novel allohexaploid", ]#created mysterious NA rows
View(Nov_Allohex)

#SUBSETTING######################################################################
# so geht's ohne NA bug
nov_allohex <- DATA_renamed[which(DATA_renamed$Accession == "novel allohexaploid"),]
View(nov_allohex)

allohex_hybrids <- DATA_renamed[which(DATA_renamed$Accession == "allohexaploid hybrid"),]
View(allohex_hybrids)

Res_B_napus <- DATA_renamed[which(DATA_renamed$Accession == "Resynth. B. napus"),]
View(Res_B_napus)

Natural_B_napus <- DATA_renamed[which(DATA_renamed$Accession == "Natural B. napus"),]
View(Natural_B_napus)

B_rapa <- DATA_renamed[which(DATA_renamed$Accession == "B. rapa"),]
View(B_rapa)

B_oleracea <- DATA_renamed[which(DATA_renamed$Accession == "B. oleracea"),]
View(B_oleracea)

B_carinata <- DATA_renamed[which(DATA_renamed$Accession == "B. carinata"),]
View(B_carinata)

B_juncea <- DATA_renamed[which(DATA_renamed$Accession == "B. juncea"),]
View(B_juncea)

JC_hybrid <- DATA_renamed[which(DATA_renamed$Accession == "JC hybrid"),]
View(JC_hybrid)


#Nach The Outlier (https://www.youtube.com/watch?v=2yLpEeO0QNc)
install.packages("metan")
library(metan)

bivar.corr.EPPN <- corr_coef(DATA_renamed[ , 3:34])
plot(bivar.corr.EPPN,
     reorder = FALSE,)

plot(bivar.corr.EPPN,
     reorder = FALSE,
     insig = 'blank')

plot.corr_coef(bivar.corr.EPPN,
               reorder = FALSE,
               insig = 'blank')

#plot.corr_coef auch aus metan package
plot.corr_coef(DATA_renamed[ , 3:34])



