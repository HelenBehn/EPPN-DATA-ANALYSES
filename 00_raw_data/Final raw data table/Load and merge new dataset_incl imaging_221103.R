# packages
library(tidyverse)
library(readr)

#OLD
#set wd
#getwd()
# setwd("C:/Users/Helen Behn/Documents/R/EPPN Data Analysis/Final raw data table")# former location
setwd("C:/Users/Helen Behn/Documents/R/EPPN-DATA-ANALYSES/00_raw_data/Final raw data table")#new location

# read csv
# Als csv UTF-8 speichern, Semikolons durch Kommata ersetzen und Endung .csv anfügen!!!
Seed <- read_csv("Raw data seed number per pod_FINAL_CSV.csv")
PhenMan <- read_csv("RawFinal_Manual_Phenotypic data_Fiona.csv")
PhenImage <- read_csv("BR024_plant_id_phenotypic_Imaging_Kevin.csv")
PhenRaceme <- read_csv("BR024_raceme_data_Imaging_Kevin_revised_20220308.csv")
BBCHman <- read_csv("FINAL Raw BBCH manual CSV.csv") ### kann in anderes R Markdown Dok.
View(PhenMan)
View(PhenImage)
View(PhenRaceme)
View(Seed)

#merge all 3 data frames
PhenCompleteNew <-cbind(PhenMan, PhenImage, PhenRaceme, Seed)
View(PhenCompleteNew)

#Gesamtdatentabelle als csv expotieren für Kollegen
write.csv(PhenComplCleanR, "C:/Users/Helen Behn/Documents/R/EPPN Data Analysis/Final raw data table/EPPN raw data complete 220419.csv")

# Rohdatensatz laden
setwd("Z:/Projects/EPPN Project_Ploidy_Heterozygosity/EPPN-DATA-ANALYSES/00_raw_data/Final raw data table")
data <- read_csv("EPPN raw data complete 220419.csv")
View(data)

data <- data[ , -1]

#NEW
#set wd ### attention! This time wd on server Z.
setwd("Z:/Projects/EPPN Project_Ploidy_Heterozygosity/EPPN-DATA-ANALYSES/00_raw_data/20221101_BR024_data_for_Annaliese_and_Helen_NEU")
getwd()

# read csv###
# Als csv UTF-8 speichern, Semikolons durch Kommata ersetzen und Endung .csv anfügen!
pla_phenotypes <- read_csv("BR024_all_pla_phenotypes_by_plant.csv")
View(pla_phenotypes)
plat_phenotypes <- read_csv("BR024_all_plat_phenotypes_by_plant.csv")
View(plat_phenotypes)
raceme_data <- read_csv("BR024_raceme_data_by_plant_revised_20220308.csv") ### old dataset!!!
View(raceme_data)
sv_flowering_phenotype_data <- read_csv("BR024_sv_flowering_phenotype_data_by_plant.csv")
View(sv_flowering_phenotype_data)
tv_flowering_start_data <- read_csv("BR024_tv_flowering_start_data_by_plant.csv")
View(tv_flowering_start_data)

# check types###
typeof(pla_phenotypes)
str(pla_phenotypes)

#covert into data frames
pla_phenotypes_df <- as.data.frame(pla_phenotypes)
plat_phenotypes_df <- as.data.frame(plat_phenotypes)
raceme_data_df <- as.data.frame(raceme_data)
sv_flowering_phenotype_data_df <- as.data.frame(sv_flowering_phenotype_data)
tv_flowering_start_data_df <- as.data.frame(tv_flowering_start_data)

#merge df #test
# 1.
Image_Analyses_221102_A <- merge(sv_flowering_phenotype_data_df, tv_flowering_start_data, by = "plant_id")
View(Image_Analyses_221102_A)

# 2. 
Image_Analyses_221102_C <- merge(Image_Analyses_221102_A, plat_phenotypes_df, by = "plant_id")
View(Image_Analyses_221102_C)

Image_Analyses_221026_B <- merge(pla_phenotypes_df, raceme_data_df, by = "plant_id")
Image_Analyses_221026_B

#join df with different lengths
library(plyr)
Image_Analyses_221102_compl <- join(Image_Analyses_221102_C, Image_Analyses_221026_B, type = "full", by = "plant_id")
View(Image_Analyses_221102_compl)

#drop redundant columns
drop <- c("genotype_name", "genotype_name.y", "genotype")
ImAnalysesClean221102 = Image_Analyses_221102_compl[,!(names(Image_Analyses_221102_compl) %in% drop)]
View(ImAnalysesClean221102)

# Umbenennung in neuen Code - PROGENY SETS UNTERSCHIEDEN
ImAnalysesClean221102[ImAnalysesClean221102 == "PH01"] <- "A16" #formerly R2 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH02"] <- "A04" # R1 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH03"] <- "O1"  # O1
ImAnalysesClean221102[ImAnalysesClean221102 == "PH04"] <- "C36" # O2
ImAnalysesClean221102[ImAnalysesClean221102 == "PH05"] <- "C37" # O3
ImAnalysesClean221102[ImAnalysesClean221102 == "PH06"] <- "C46" # O4
ImAnalysesClean221102[ImAnalysesClean221102 == "PH07"] <- "C1" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH08"] <- "C2" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH09"] <- "J1" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH10"] <- "J2" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH11"] <- "J3" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH12"] <- "N5" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH13"] <- "N6" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH14"] <- "N1" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH15"] <- "N4" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH16"] <- "N7" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH17"] <- "A16C46" ##### "R204" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH18"] <- "A4C37"  ##### "R103" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH19"] <- "A4C36"  ##### "R102" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH20"] <- "J1C1" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH21"] <- "J1C2" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH22"] <- "N5C2J1_PH22" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH23"] <- "N5C2J2_PH23" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH24"] <- "N6C2J2_PH24" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH25"] <- "N1C1J1_PH25" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH26"] <- "N1C2J1_PH26" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH27"] <- "N4C2J1_PH27" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH28"] <- "N7C1J1" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH29"] <- "O1J3_PH29" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH30"] <- "N5C2J2_PH30" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH31"] <- "N1C1J1_PH31" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH32"] <- "N1C1J1.N5C2J2" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH33"] <- "N6C2J2.N7C1J1" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH34"] <- "N5C2J2.N7C1J1" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH35"] <- "N6C2J2_PH35" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH36"] <- "N1C1J1_PH36" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH37"] <- "N1C1J1. N6C2J2" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH38"] <- "N5C2J2.N6C2J2" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH39"] <- "N1C1J1_PH39" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH40"] <- "N1C2J1_PH40" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH41"] <- "N1C1J1.N1C2J1" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH42"] <- "N5C2J1_PH42" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH43"] <- "N5C2J2.N5C2J2" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH44"] <- "N6C2J2_PH44" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH45"] <- "N4C2J1_PH45" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH46"] <- "N6C2J2.N4C2J1" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH47"] <- "O1J3_PH47" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH48"] <- "N6C2J2_PH48" 
ImAnalysesClean221102[ImAnalysesClean221102 == "PH49"] <- "N6C2J2.O1J3" 

str(ImAnalysesClean221102)

ImAnalysesClean221102[is.na(ImAnalysesClean221102) | ImAnalysesClean221102 == "Inf"] <- NA   # Replace NaN & Inf with NA
View(ImAnalysesClean221102)

#CSV schreiben
#CSV schreiben
#CSV schreiben
write.csv(ImAnalysesClean221102, "Z:/Projects/EPPN Project_Ploidy_Heterozygosity/EPPN-DATA-ANALYSES/00_raw_data/ImAnalysesClean221102.csv")
# Mit dieser vollständigen Rohdatentabelle in Zukunft weiterarbeiten!######
setwd("Z:/Projects/EPPN Project_Ploidy_Heterozygosity/EPPN-DATA-ANALYSES/00_raw_data")
imaging_data <- read_csv("ImAnalysesClean221102.csv")
View(imaging_data)

imaging_data <- imaging_data[, 2:16]
View(imaging_data)
