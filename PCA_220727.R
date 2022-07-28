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


# Compute PCA
str(DATA)
DATA$Genotype <- as.factor(DATA$Genotype)
typeof(DATA$Genotype)

#recode first column as row names
DATA2 <- DATA[,-1]
rownames(DATA2) <- DATA[,1]
View(DATA2)

# Compute PCA - with rownames instead of factor column
# na.omit only works, if you apply it directly to the dataframe
PCA_results <- prcomp(na.omit(DATA2), center = TRUE, scale = TRUE) #Was macht scale = TRUE nochmal?
PCA_results2 <- prcomp(na.omit(DATA2), scale = TRUE)                                                                                #Was macht center = TRUE nochmal?

# Visualize eigenvalues (scree plot). Show the percentage of variances 
# explained by each principal component.
fviz_eig(PCA_results)


# Graph of individuals. Individuals with a similar profile are grouped together.
fviz_pca_ind(PCA_results2)
