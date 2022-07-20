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
View(DATA)

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
                Max_shoot_extension_rate_px_day = R03_max_rate,
                DAS_of_max_shoot_extension_rate = R04_DAS_of_max_rate,
                Days_to_90_percent_max_height = R05_main_ext_period, 
                Days_to_50_percent_max_height = R06_50ht_period,
                Days_to_75_percent_max_height = R07_75ht_period, 
                Growth_rate_50_percent_max = R08_50ht_rate,
                Growth_rate_75_percent_max = R09_75ht_rate,
                Maximum_plant_height = R10_max_height, 
                Maximum_plant_height_DAS = R11_max_height_DAS,
                Extension_days_to_max = R12_extension_days_to_max,
                Seed_weight_10_pods_g = "seed wt_10p_g", 
                Seeds_pod = seeds_per_pod,
                Thousand_seed_weight_g = TGW_g)
view(DATA_renamed)

# correlation matrix across all genotypes
# cor function
str(DATA)
cor(DATA[,2:33], DATA[,2:33], method = "pearson") #geht nur für numerische Variablen

#corr.test
install.packages("psych")
library(psych)

corr.test(DATA[,2:33], DATA[,2:33], method = "pearson")

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

#Nach KHstats (https://www.khstats.com/blog/corr-plots/corr-plots/#just-the-code)
install.packages("Hmisc")
library(Hmisc)

str(DATA)
typeof(DATA)

# get r, p and n von KH ##geht nicht
rcorr()

#alternative #calculate correlation coefficient (r), P-value and number of observations 
corrDATA <- corr.test(DATA[,3:34], DATA[,3:34], method = "pearson")
#check: Does corr.test drop missing pairs?
print(corrDATA)
head(corrDATA)
str(corrDATA)
typeof(corrDATA)

#turn into df
Corr_r <- data.frame(corrDATA$r)
view(Corr_r)

print(print(corrDATA$p))
Corr_p <- data.frame(corrDATA$p)
view(Corr_p)

Corr_n <- data.frame(corrDATA$n)
view(Corr_n)

#Let’s turn each matrix into a data frame and look at the top six rows with head and kable.
data.frame(corrDATA$r) %>% head(n=3) %>% kable()
data.frame(corrDATA$p) %>% head(n=3) %>% kable()
data.frame(corrDATA$n) %>% head(n=3) %>% kable()

typeof(corrDATA)
#write function
cors <- function(df) {
  M <- Hmisc::rcorr(as.matrix(df))
# turn all three matrices (r, n, and P into a data frame)
  Mdf <- map(M, ~data.frame(.x))
# return the three data frames in a list
  return(Mdf)
}

#use function #### geht nicht weil liste
cors(DATA) %>% first() %>% head() %>% kable()
#unlist object
str(corrDATA)
corrMATRIX <- matrix(unlist(corrDATA), ncol = 32, nrow = 20483)
#use function #### geht nicht weil liste
cors(corrMATRIX) %>% first() %>% head() %>% kable()

formatted_cors <- function(df){
  cors(df) %>%
    map(~rownames_to_column(.x, var="measure1")) %>%
    map(~pivot_longer(.x, -measure1, "measure2")) %>%
    bind_rows(.id = "id") %>%
    pivot_wider(names_from = id, values_from = value) %>%
    rename(p = P) %>%
    mutate(sig_p = ifelse(p < .05, T, F),
           p_if_sig = ifelse(sig_p, p, NA),
           r_if_sig = ifelse(sig_p, r, NA)) 
}

formatted_cors(mtcars) %>%
  ggplot(aes(measure1, measure2, fill=r, label=round(r_if_sig,2))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Pearson's\nCorrelation", title="Correlations in Mtcars",
       subtitle="Only significant Pearson's correlation coefficients shown") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1)) +
  geom_text() +
  theme_classic() +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  theme(text=element_text(family="Roboto"))