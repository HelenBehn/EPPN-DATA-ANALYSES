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



########################################################################
# nochmal mit corrplot (https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html)

library(corrplot)

Corr_mat1 <- cor(DATA_renamed[ , 3:34]) #viele NAs in Ergebnis-Matrix
print(Corr_mat1)

# use = "pairwise.complete.obs", weil fehlende Werte zu NAs führen 
Corr_mat2 <- cor(x = as.matrix(DATA_renamed[ , 3:34]), method = "pearson", use = "pairwise.complete.obs") 
print(Corr_mat2)

# visualize with corrplot
corrplot(Corr_mat2, method = 'number')

#get p-value matrix and confidence intervals matrix by cor.mtest() which returns a list containing:
    # 1. p is the p-values matrix.
    # 2. lowCI is the lower bound of confidence interval matrix.
    # 3. uppCI is the lower bound of confidence interval matrix.
P_mat_corr <- cor.mtest(DATA_renamed[ , 3:34], conf.level = 0.95)


## add significant level stars 
corrplot(Corr_mat2, #correlation matrix
         p.mat = P_mat_corr$p, # Quelle der P-Werte 
         tl.pos = 'd', 
         insig = 'blank', 
         sig.level = c(0.001, 0.01, 0.05),
         pch.cex = 0.9, 
         pch.col = 'grey20')

## add significant level stars
corrplot(Corr_mat2, 
         p.mat = P_mat_corr$p, 
         method = 'color', 
         diag = FALSE, 
         type = 'lower',
         sig.level = c(0.001, 0.01, 0.05), 
         pch.cex = 0.9,
         insig = 'blank', 
         pch.col = 'grey20')

## leave blank on non-significant coefficient
## add all correlation coefficients
corrplot(Corr_mat2, 
         p.mat = P_mat_corr$p, 
         method = 'color', 
         type = 'lower', 
         insig='blank',
         diag = FALSE)$corrPos -> p1
text(p1$x, p1$y, round(p1$corr, 2))

## leave blank on non-significant coefficient
## add all correlation coefficients
corrplot(Corr_mat2, 
         p.mat = P_mat_corr$p, 
         method = 'circle', 
         type = 'lower', 
         insig='blank',
         order = 'AOE', 
         diag = FALSE)$corrPos -> p1
text(p1$x, p1$y, round(p1$corr, 2))

## leave blank on non-significant coefficient
## add all correlation coefficients
library(corrplot)

Corr_mat2 <- cor(x = as.matrix(DATA_renamed[ , 3:34]), method = "pearson", use = "pairwise.complete.obs") 
print(Corr_mat2)

P_mat_corr <- cor.mtest(DATA_renamed[ , 3:34], conf.level = 0.95)

corrplot(Corr_mat2, 
         p.mat = P_mat_corr$p,
         method = 'circle', 
         type = 'lower', 
         insig='blank',
         addCoef.col ='black', 
         number.cex = 0.8, 
         diag=FALSE)

#gut!
corrplot(Corr_mat2, 
         p.mat = P_mat_corr$p,
         method = 'color', 
         type = 'lower',
         sig.level = 0.05,
         insig='blank',
         addCoef.col ='black',
         number.cex = 0.6,
         tl.cex = 0.6,
         cl.cex = 0.6,
         tl.col = "black",
         diag=FALSE)

#change colour of text labels #nimmt er nicht, schade
corrplot(Corr_mat2, 
         p.mat = P_mat_corr$p,
         method = 'color', 
         type = 'lower',
         sig.level = 0.05,
         insig='blank',
         addCoef.col ='black',
         number.cex = 0.6,
         tl.cex = 0.7,
         cl.cex = 0.7,
         tl.col = "black",
         diag=TRUE,
         tl.col = c(Plant_height = "black",
                    Branch_number = "black",
                    Pod_weight_plant = "black",
                    Pod_number_branch = "black",
                    Pods_per_plant = "black",
                    Pods_per_branch = "black",
                    Single_pod_weight = "black",
                    First_flowers_DAS = "grey",
                    Maximum_flower_pixels_DAS = "grey",
                    Flowering_days_to_peak = "grey",
                    Total_flowering_days = "grey",
                    Pixel_number_at_peak_flowering = "grey",
                    Sum_of_flower_pixels_to_peak = "grey",
                    Total_pixel_days = "grey",
                    Mean_pixels_to_peak_flowering = "grey",
                    Start_of_shoot_extension_DAS = "grey",
                    Mean_shoot_extension_rate =  "grey",
                    Max_shoot_extension_rate = "grey",
                    DAS_of_max_shoot_extension_rate = "grey",
                    Days_to_90_percent_max_height = "grey", 
                    Days_to_50_percent_max_height = "grey",
                    Days_to_75_percent_max_height = "grey", 
                    Growth_rate_50_percent_max = "grey",
                    Growth_rate_75_percent_max = "grey",
                    Maximum_plant_height = "grey", 
                    DAS_of_max_plant_height = "grey",
                    Extension_days_to_max = "grey",
                    Seed_weight_10_pods_g = "black", 
                    Seeds_per_pod = "black",
                    Thousand_seed_weight_g = "black"))

#################################################################################
#FERTIGSTELLUNG #################################################################
library(corrplot)

#FINALE VERSION: Kompletter Datensatz

Corr_mat2 <- cor(x = as.matrix(DATA_renamed[ , 3:34]), method = "pearson", use = "pairwise.complete.obs") 
print(Corr_mat2)

P_mat_corr <- cor.mtest(DATA_renamed[ , 3:34], conf.level = 0.95)

corrplot(Corr_mat2, 
         p.mat = P_mat_corr$p,
         method = 'color', 
         type = 'lower',
         sig.level = 0.05,
         insig='blank',
         addCoef.col ='black',
         number.cex = 0.6,
         tl.cex = 0.7,
         cl.cex = 0.7,
         tl.col = "black",
         diag=TRUE)

# nov_allohex
Corr_nov_allohex <- cor(x = as.matrix(nov_allohex[ , 3:34]), method = "pearson", use = "pairwise.complete.obs") 
print(Corr_nov_allohex)

P_nov_allohex <- cor.mtest(nov_allohex[ , 3:34], conf.level = 0.95)

corrplot(Corr_nov_allohex, 
         p.mat = P_nov_allohex$p,
         method = 'color', 
         type = 'lower',
         sig.level = 0.05,
         insig='blank',
         addCoef.col ='black',
         number.cex = 0.6,
         tl.cex = 0.7,
         cl.cex = 0.7,
         tl.col = "black",
         diag=TRUE)

# allohex_hybrids
Corr_allohex_hybrids <- cor(x = as.matrix(allohex_hybrids[ , 3:34]), method = "pearson", use = "pairwise.complete.obs") 
print(Corr_allohex_hybrids)

P_allohex_hybrids <- cor.mtest(allohex_hybrids[ , 3:34], conf.level = 0.95)

corrplot(Corr_allohex_hybrids, 
         p.mat = P_allohex_hybrids$p,
         method = 'color', 
         type = 'lower',
         sig.level = 0.05,
         insig='blank',
         addCoef.col ='black',
         number.cex = 0.6,
         tl.cex = 0.7,
         cl.cex = 0.7,
         tl.col = "black",
         diag=TRUE)

# Res_B_napus
Corr_Res_B_napus <- cor(x = as.matrix(Res_B_napus[ , 3:34]), method = "pearson", use = "pairwise.complete.obs") 
print(Corr_Res_B_napus)

P_Res_B_napus <- cor.mtest(Res_B_napus[ , 3:34], conf.level = 0.95)

corrplot(Corr_Res_B_napus, 
         p.mat = P_Res_B_napus$p,
         method = 'color', 
         type = 'lower',
         sig.level = 0.05,
         insig='blank',
         addCoef.col ='black',
         number.cex = 0.6,
         tl.cex = 0.7,
         cl.cex = 0.7,
         tl.col = "black",
         diag=TRUE)

plot.new()
dev.off()

# Natural_B_napus # didn't work
Corr_Natural_B_napus <- cor(x = as.matrix(Natural_B_napus[ , 3:34]), method = "pearson", use = "pairwise.complete.obs") 
print(Corr_Natural_B_napus)

P_Natural_B_napus <- cor.mtest(Natural_B_napus[ , 3:34], conf.level = 0.95)

corrplot(Corr_Natural_B_napus, 
         p.mat = P_Natural_B_napus$p,
         method = 'color', 
         type = 'lower',
         sig.level = 0.05,
         insig='blank',
         addCoef.col ='black',
         number.cex = 0.6,
         tl.cex = 0.7,
         cl.cex = 0.7,
         tl.col = "black",
         diag=TRUE)


# B_rapa
Corr_B_rapa <- cor(x = as.matrix(B_rapa[ , 3:34]), method = "pearson", use = "pairwise.complete.obs") 
print(Corr_B_rapa)

P_B_rapa <- cor.mtest(B_rapa[ , 3:34], conf.level = 0.95)

corrplot(Corr_B_rapa, 
         p.mat = P_B_rapa$p,
         method = 'color', 
         type = 'lower',
         sig.level = 0.05,
         insig='blank',
         addCoef.col ='black',
         number.cex = 0.6,
         tl.cex = 0.7,
         cl.cex = 0.7,
         tl.col = "black",
         diag=TRUE)


# B_oleracea
Corr_B_oleracea <- cor(x = as.matrix(B_oleracea[ , 3:34]), method = "pearson", use = "pairwise.complete.obs") 
print(Corr_B_oleracea)

P_B_oleracea <- cor.mtest(B_oleracea[ , 3:34], conf.level = 0.95)

corrplot(Corr_B_oleracea, 
         p.mat = P_B_oleracea$p,
         method = 'color', 
         type = 'lower',
         sig.level = 0.05,
         insig='blank',
         addCoef.col ='black',
         number.cex = 0.6,
         tl.cex = 0.7,
         cl.cex = 0.7,
         tl.col = "black",
         diag=TRUE)


# B_juncea
Corr_B_juncea <- cor(x = as.matrix(B_juncea[ , 3:34]), method = "pearson", use = "pairwise.complete.obs") 
print(Corr_B_juncea)

P_B_juncea <- cor.mtest(B_juncea[ , 3:34], conf.level = 0.95)

corrplot(Corr_B_juncea, 
         p.mat = P_B_juncea$p,
         method = 'color', 
         type = 'lower',
         sig.level = 0.05,
         insig='blank',
         addCoef.col ='black',
         number.cex = 0.6,
         tl.cex = 0.7,
         cl.cex = 0.7,
         tl.col = "black",
         diag=TRUE)


# B_carinata
Corr_B_carinata <- cor(x = as.matrix(B_carinata[ , 3:34]), method = "pearson", use = "pairwise.complete.obs") 
print(Corr_B_carinata)

P_B_carinata <- cor.mtest(B_carinata[ , 3:34], conf.level = 0.95)

corrplot(Corr_B_carinata, 
         p.mat = P_B_carinata$p,
         method = 'color', 
         type = 'lower',
         sig.level = 0.05,
         insig='blank',
         addCoef.col ='black',
         number.cex = 0.6,
         tl.cex = 0.7,
         cl.cex = 0.7,
         tl.col = "black",
         diag=TRUE)


# JC_hybrid
Corr_JC_hybrid <- cor(x = as.matrix(JC_hybrid[ , 3:34]), method = "pearson", use = "pairwise.complete.obs") 
print(Corr_JC_hybrid)

P_JC_hybrid <- cor.mtest(JC_hybrid[ , 3:34], conf.level = 0.95)

corrplot(Corr_JC_hybrid, 
         p.mat = P_JC_hybrid$p,
         method = 'color', 
         type = 'lower',
         sig.level = 0.05,
         insig='blank',
         addCoef.col ='black',
         number.cex = 0.6,
         tl.cex = 0.7,
         cl.cex = 0.7,
         tl.col = "black",
         diag=TRUE)

