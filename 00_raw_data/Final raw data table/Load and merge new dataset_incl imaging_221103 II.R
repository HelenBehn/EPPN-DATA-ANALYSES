# packages
library(tidyverse)
library(readr)

setwd("Z:/Projects/EPPN Project_Ploidy_Heterozygosity/EPPN Data Analysis/Final raw data table/Individual raw data tables")#new location

# read csv
# Als csv UTF-8 speichern, Semikolons durch Kommata ersetzen und Endung .csv anfügen!!!
#old
Seed <- read_csv("Raw data seed number per pod_FINAL_CSV.csv")
View(Seed)
PhenMan <- read_csv("RawFinal_Manual_Phenotypic data_Fiona.csv")
View(PhenMan)
PhenRaceme <- read_csv("BR024_raceme_data_Imaging_Kevin_revised_20220308.csv")
View(PhenRaceme)
BBCHman <- read_csv("FINAL Raw BBCH manual CSV.csv") ### kann in anderes R Markdown Dok.
View(BBCHman)
#new
pla_phenotypes <- read_csv("BR024_all_pla_phenotypes_by_plant.csv")
View(pla_phenotypes)
plat_phenotypes <- read_csv("BR024_all_plat_phenotypes_by_plant.csv")
View(plat_phenotypes)
sv_flowering_phenotype_data <- read_csv("BR024_sv_flowering_phenotype_data_by_plant.csv")
View(sv_flowering_phenotype_data)
tv_flowering_start_data <- read_csv("BR024_tv_flowering_start_data_by_plant.csv")
View(tv_flowering_start_data)

# introduce / rename plant_id
names(Seed)[names(Seed) == "bc"] <- "plant_id"
names(PhenMan)[names(PhenMan) == "barcode"] <- "plant_id"
names(BBCHman)[names(BBCHman) == "barcode"] <- "plant_id"
names(Seed)[names(Seed) == "bc"] <- "plant_id"
names(Seed)[names(Seed) == "bc"] <- "plant_id"


#join df with different lengths
library(tidyverse)
#put all data frames into list
df_list <- list(PhenMan, pla_phenotypes, plat_phenotypes, PhenRaceme, BBCHman, 
                sv_flowering_phenotype_data, tv_flowering_start_data, Seed)
df_list
#merge all data frames in list
Data_complete_221103 <- df_list %>% reduce(full_join, by='plant_id')
View(Data_complete_221103)

#drop redundant columns
drop <- c("code genotype long", "c", "genotype_name.x", "L", "C5", "genotype.x", "location")
Data_complete_221103 = Data_complete_221103[,!(names(Data_complete_221103) %in% drop)]
View(Data_complete_221103)

#rearrange
Data_complete_221103 <- Data_complete_221103[ , c(1:2, 27:28, 3:26, 29:75)]
View(Data_complete_221103)

#rename genotye column
names(Data_complete_221103)[names(Data_complete_221103) == "code.x"] <- "Genotype"

# Remove the rows in Data frame containing X or Y in plant id
#remove ids containing X
Data_comp_clean_221103 = Data_complete_221103[!grepl("X",Data_complete_221103$plant_id),]
print(Data_comp_clean_221103)
#remove ids containing Y
DataCompClean_221103 = Data_comp_clean_221103[!grepl("Y",Data_comp_clean_221103$plant_id),]
View(DataCompClean_221103)

# Replace NA in Genotype by value from code.y 
library(dplyr)
DataCompClean_221103 <- DataCompClean_221103 %>% 
                  mutate(Genotype = coalesce(Genotype,code.y))

# Umbenennung in neuen Code - PROGENY SETS UNTERSCHIEDEN
DataCompClean_221103[DataCompClean_221103 == "PH01"] <- "A16" #formerly R2 
DataCompClean_221103[DataCompClean_221103 == "PH02"] <- "A04" # R1 
DataCompClean_221103[DataCompClean_221103 == "PH03"] <- "O1"  # O1
DataCompClean_221103[DataCompClean_221103 == "PH04"] <- "C36" # O2
DataCompClean_221103[DataCompClean_221103 == "PH05"] <- "C37" # O3
DataCompClean_221103[DataCompClean_221103 == "PH06"] <- "C46" # O4
DataCompClean_221103[DataCompClean_221103 == "PH07"] <- "C1" 
DataCompClean_221103[DataCompClean_221103 == "PH08"] <- "C2" 
DataCompClean_221103[DataCompClean_221103 == "PH09"] <- "J1" 
DataCompClean_221103[DataCompClean_221103 == "PH10"] <- "J2" 
DataCompClean_221103[DataCompClean_221103 == "PH11"] <- "J3" 
DataCompClean_221103[DataCompClean_221103 == "PH12"] <- "N5" 
DataCompClean_221103[DataCompClean_221103 == "PH13"] <- "N6" 
DataCompClean_221103[DataCompClean_221103 == "PH14"] <- "N1" 
DataCompClean_221103[DataCompClean_221103 == "PH15"] <- "N4" 
DataCompClean_221103[DataCompClean_221103 == "PH16"] <- "N7" 
DataCompClean_221103[DataCompClean_221103 == "PH17"] <- "A16C46" ##### "R204" 
DataCompClean_221103[DataCompClean_221103 == "PH18"] <- "A4C37"  ##### "R103" 
DataCompClean_221103[DataCompClean_221103 == "PH19"] <- "A4C36"  ##### "R102" 
DataCompClean_221103[DataCompClean_221103 == "PH20"] <- "J1C1" 
DataCompClean_221103[DataCompClean_221103 == "PH21"] <- "J1C2" 
DataCompClean_221103[DataCompClean_221103 == "PH22"] <- "N5C2J1_PH22" 
DataCompClean_221103[DataCompClean_221103 == "PH23"] <- "N5C2J2_PH23" 
DataCompClean_221103[DataCompClean_221103 == "PH24"] <- "N6C2J2_PH24" 
DataCompClean_221103[DataCompClean_221103 == "PH25"] <- "N1C1J1_PH25" 
DataCompClean_221103[DataCompClean_221103 == "PH26"] <- "N1C2J1_PH26" 
DataCompClean_221103[DataCompClean_221103 == "PH27"] <- "N4C2J1_PH27" 
DataCompClean_221103[DataCompClean_221103 == "PH28"] <- "N7C1J1" 
DataCompClean_221103[DataCompClean_221103 == "PH29"] <- "O1J3_PH29" 
DataCompClean_221103[DataCompClean_221103 == "PH30"] <- "N5C2J2_PH30" 
DataCompClean_221103[DataCompClean_221103 == "PH31"] <- "N1C1J1_PH31" 
DataCompClean_221103[DataCompClean_221103 == "PH32"] <- "N1C1J1.N5C2J2" 
DataCompClean_221103[DataCompClean_221103 == "PH33"] <- "N6C2J2.N7C1J1" 
DataCompClean_221103[DataCompClean_221103 == "PH34"] <- "N5C2J2.N7C1J1" 
DataCompClean_221103[DataCompClean_221103 == "PH35"] <- "N6C2J2_PH35" 
DataCompClean_221103[DataCompClean_221103 == "PH36"] <- "N1C1J1_PH36" 
DataCompClean_221103[DataCompClean_221103 == "PH37"] <- "N1C1J1. N6C2J2" 
DataCompClean_221103[DataCompClean_221103 == "PH38"] <- "N5C2J2.N6C2J2" 
DataCompClean_221103[DataCompClean_221103 == "PH39"] <- "N1C1J1_PH39" 
DataCompClean_221103[DataCompClean_221103 == "PH40"] <- "N1C2J1_PH40" 
DataCompClean_221103[DataCompClean_221103 == "PH41"] <- "N1C1J1.N1C2J1" 
DataCompClean_221103[DataCompClean_221103 == "PH42"] <- "N5C2J1_PH42" 
DataCompClean_221103[DataCompClean_221103 == "PH43"] <- "N5C2J2.N5C2J2" 
DataCompClean_221103[DataCompClean_221103 == "PH44"] <- "N6C2J2_PH44" 
DataCompClean_221103[DataCompClean_221103 == "PH45"] <- "N4C2J1_PH45" 
DataCompClean_221103[DataCompClean_221103 == "PH46"] <- "N6C2J2.N4C2J1" 
DataCompClean_221103[DataCompClean_221103 == "PH47"] <- "O1J3_PH47" 
DataCompClean_221103[DataCompClean_221103 == "PH48"] <- "N6C2J2_PH48" 
DataCompClean_221103[DataCompClean_221103 == "PH49"] <- "N6C2J2.O1J3" 
View(DataCompClean_221103)


#CSV schreiben
write.csv(DataCompClean_221103, "Z:/Projects/EPPN Project_Ploidy_Heterozygosity/EPPN Data Analysis/Final raw data table/Complete raw data table/DataCompClean_221103.csv")

# Mit dieser vollständigen Rohdatentabelle in Zukunft weiterarbeiten!######
setwd("Z:/Projects/EPPN Project_Ploidy_Heterozygosity/EPPN Data Analysis/Final raw data table/Complete raw data table")
Data <- read_csv("DataCompClean_221103.csv")
View(Data)

Data <- Data[, -1]
View(Data)
