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
    
    Genotype == "R2O4" ~ "Resynth. B. napus",
    Genotype == "R1O3" ~ "Resynth. B. napus",
    Genotype == "R1O2" ~ "Resynth. B. napus",
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
#Accession nach vorne
DATA = DATA[, c(1, 34, 2:33)]
#Group traits as growth, development and fertility traits
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

#Nach The Outlier (https://www.youtube.com/watch?v=2yLpEeO0QNc)
install.packages("metan")
library(metan)

bivar.corr.EPPN <- corr_coef(DATA[ , 2:33])
plot(bivar.corr.EPPN)

colnames(DATA)

#plot.corr_coef auch aus metan package
plot.corr_coef(DATA_renamed[ , 3:34])



#NEU#############################################################################
# ggcorrplot: Visualization of a correlation matrix using ggplot2 nach STHDA
# (http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2)

#ggcorrplot can be installed from CRAN as follow:
install.packages("ggcorrplot")

#Or, install the latest version from GitHub:
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggcorrplot")

#Loading:
library(ggcorrplot)

# Compute a correlation matrix
corr_matrix <- round(cor(DATA_renamed[ , 3:34]), 1)
head(corr_matrix[, 1:6])
print(corr_matrix) #warum Werte NA?

# cor function with different arguments # still NAs
cor(DATA_renamed[ , 3:34], y = NULL, use = "everything",
    method = c("pearson", "kendall", "spearman"))

# Compute a matrix of correlation p-values
corr_matrix_p <- cor_pmat(DATA_renamed[ , 3:34])
head(corr_matrix_p[, 1:4])

# Visualize the correlation matrix
# method = "square" (default)
ggcorrplot(corr_matrix)

# method = "circle"
ggcorrplot(corr_matrix, method = "circle")

# Types of correlogram layout
# Get the lower triangle
ggcorrplot(corr_matrix, 
           type = "lower",
           outline.col = "white")

# Argument colors # ggcorrplot BRAUCHT MATRIX ODER DF!!!
ggcorrplot(corr_matrix, 
           type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))

# Add correlation coefficients
# argument lab = TRUE
ggcorrplot(corr_matrix, 
           type = "lower",
           lab = TRUE)

# Add correlation significance level
# Argument p.mat, barring the no significant coefficient
ggcorrplot(corr_matrix_p, 
           type = "lower", 
           p.mat = corr_matrix_p)

# Add correlation coefficients and leave non-significant values blank
# argument lab = TRUE
ggcorrplot(corr_matrix_p, 
           p.mat = corr_matrix_p, 
           type = "lower", 
           insig = "blank",
           lab = TRUE)

#NEU##########################################################################
#nach STHDA mit ggplot2######################################################

cormat <- round(cor(DATA_renamed[ , 3:34]),2)
head(cormat)

#The package reshape is required to melt the correlation matrix :
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

#The function geom_tile()[ggplot2 package] is used to visualize the correlation matrix :
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()