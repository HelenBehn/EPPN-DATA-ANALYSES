#load
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)

# vollständige Rohdatentabelle laden ##########################################
#Im Projekt EPPN-DATA-ANALYSES ist dies das wd. Kann nicht geändert werden. 
# Daher Rohdatentabelle direkt von dort laden. 
getwd()
DATA <- read_csv("EPPN raw data complete 220419.csv")

#drop columns
DATA = DATA[,-c(1:2, 4:5)]
View(DATA)



#PCA - first attempts ##########################################################
# Quelle: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
# Load factoextra for visualization
install.packages("factoextra")
library(factoextra)

# Compute PCA - with rownames instead of factor column ### nachSTHDA ###
# na.omit only works, if you apply it directly to the dataframe
PCA_results <- prcomp(na.omit(DATA[ ,-1]), center = TRUE, scale = TRUE) 
print(PCA_results)                                                                             

# Visualize eigenvalues (scree plot). Show the percentage of variances 
# explained by each principal component.
fviz_eig(PCA_results)


# Graph of individuals. Individuals with a similar profile are grouped together.
fviz_pca_ind(PCA_results)

View(DATA)

#create column giving accessions/lines
DATA_accession <- DATA %>%
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
    Genotype == "N5C2J1_PH42" ~ "novel allohexaploid", 
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
View(DATA_accession)



##Nach Hefin Rhys############################################################
PCA_results <- prcomp(na.omit(DATA[ ,-1]), center = TRUE, scale = TRUE)
PCA_results

summary(PCA_results)

biplot(PCA_results)

#Check result
str(PCA_results)

# Check PCs
PCA_results$x 

#Zeilenzahl checken
PCA_results$x     # 174 Zeilen
DATA_accession    #370
View(na.omit(DATA_accession))   #172 #jetzt 174, Problem war gelöscheter genotyp N5C2J1_PH42
View(na.omit(DATA))             #174 INTERESSANT! DATA ud DATA_accession unterschiedlich viele zeilen!

#Plot by Genotype ##################################################################
#cbind ## mit na.omit geht's, weil Objekte dann die gleiche Anzahl Zeilen haben
DATA_pca2 <- cbind(na.omit(DATA), PCA_results$x[,1:2]) 
View(DATA_pca2)

library(ggplot2)

ggplot(DATA_pca2, aes(PC1, PC2, col = Genotype, fill = Genotype)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")


#Plot by Accession ##################################################################
#cbind ## mit na.omit geht's, weil Objekte dann die gleiche Anzahl Zeilen haben
DATA_pca2 <- cbind(na.omit(DATA), PCA_results$x[,1:2]) 
View(DATA_pca2)

library(ggplot2)

ggplot(DATA_pca2, aes(PC1, PC2, col = Genotype, fill = Genotype)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")



##### https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html##
install.packages("ggfortify")
library(ggfortify)


#rownames
remove_rownames(DATA_accession) %>% has_rownames()
column_to_rownames(DATA_accession, var = "Accession")
rownames(DATA_accession)

#plot PCA
autoplot(PCA_results)
autoplot(PCA_results, data = DATA_accession, colour = 'Accession')
autoplot(PCA_results, data = DATA_accession[,2:33], colour = 'Accession')

autoplot(PCA_results, data = na.omit(DATA_accession[,2:33]), colour = 'Accession')
autoplot(PCA_results, data = na.omit(DATA_accession[,2:33]))

autoplot(prcomp(na.omit(DATA[ ,-1]), center = TRUE, scale = TRUE), data = na.omit(DATA_accession), colour = 'Accession')


View(na.omit(DATA_accession))
View(PCA_results$x)
