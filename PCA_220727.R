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

#recode first column as row names ##
remove_rownames(DATA) %>% has_rownames()
DATA_PCA <- column_to_rownames(DATA, var = "Genotype")
View(DATA_PCA)
#
rownames(DATA) <- DATA[,1]
DATA[,1] <- NULL
str(DATA)
#
DATA2 <- DATA[,-1]
rownames(DATA2) <- DATA[,1]
View(DATA2)
# Geht nicht, offenbar weil nicht mehrere Zeilen denselben Namen haben dürfen##


# Compute PCA - with rownames instead of factor column ### nachSTHDA ###
# na.omit only works, if you apply it directly to the dataframe
PCA_results <- prcomp(na.omit(DATA2), center = TRUE, scale = TRUE) 
print(PCA_results)                                                                             

# Visualize eigenvalues (scree plot). Show the percentage of variances 
# explained by each principal component.
fviz_eig(PCA_results)


# Graph of individuals. Individuals with a similar profile are grouped together.
fviz_pca_ind(PCA_results)


##Nach Hefin Rhys############################################################
PCA_results <- prcomp(na.omit(DATA[ ,-1]), center = TRUE, scale = TRUE)
PCA_results

summary(PCA_results)

biplot(PCA_results)

str(PCA_results)
head(PCA_results)
tail(PCA_results)

View(PCA_results)
PCA_results
PCA_results$x 

DATA_pca <- cbind(DATA[ ,-1], PCA_results$x[,1:2])
DATA_pca

library(ggplot2)

ggplot(iris2, aes(PC1, PC2, col = Species, fill = Species)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")