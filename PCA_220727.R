#load
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(factoextra)

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
PCA_results$rotation

#Zeilenzahl checken
PCA_results$x     # 174 Zeilen
DATA_accession    #370
View(na.omit(DATA_accession))   #172 #jetzt 174, Problem war gelöscheter genotyp N5C2J1_PH42
View(na.omit(DATA))             #174 INTERESSANT! DATA ud DATA_accession unterschiedlich viele zeilen!

#Plot by Genotype ##################################################################
#cbind ## mit na.omit geht's, weil Objekte dann die gleiche Anzahl Zeilen haben
DATA_pca2 <- cbind(na.omit(DATA), PCA_results$x[,1:2]) 
View(DATA_pca2)

ggplot(DATA_pca2, aes(PC1, PC2, col = Genotype, fill = Genotype)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")

#ohne elipse
ggplot(DATA_pca2, aes(PC1, PC2, col = Genotype, fill = Genotype)) +
    geom_point(shape = 21, col = "black")

#Plot by Accession ##################################################################
#cbind ## mit na.omit geht's, weil Objekte dann die gleiche Anzahl Zeilen haben
DATA_pca2 <- cbind(na.omit(DATA_accession), PCA_results$x[,1:2]) 
View(DATA_pca2)

ggplot(DATA_pca2, aes(PC1, PC2, col = Accession, fill = Accession)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")

ggplot(DATA_pca2, aes(PC1, PC2, col = Accession, fill = Accession)) +
  geom_point(shape = 21, col = "black")

# correlation PCs and Trait Variables###
cor(na.omit(DATA[,-1]), na.omit(DATA_pca2[,35:36]))




#Da ich mehr als 2 relevant PCs habe und abbilden möchte, hier ein Versuch nach STHDA####
#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

#Install the two packages as follow:
install.packages(c("FactoMineR", "factoextra"))

#Load them in R, by typing this:
library("FactoMineR")
library("factoextra")

#functions in factoextra
#1. get_eigenvalue(res.pca): Extract the eigenvalues/variances of principal components
#2. fviz_eig(res.pca): Visualize the eigenvalues
#3. get_pca_ind(res.pca), get_pca_var(res.pca): Extract the results for individuals and variables, respectively.
#4. fviz_pca_ind(res.pca), fviz_pca_var(res.pca): Visualize the results individuals and variables, respectively.
#5. fviz_pca_biplot(res.pca): Make a biplot of individuals and variables.

#1. get_eigenvalue
eig.val <- get_eigenvalue(PCA_results)
eig.val
eig.val2 <- get_eig(PCA_results)
eig.val2

#2. fviz_eig erzeugt scree plot
fviz_eig(PCA_results, addlabels = TRUE, ylim = c(0, 50))

#3. get_pca_ind
var <- get_pca_var(PCA_results)
var
var$cos2

# correlation circle
# Coordinates of variables
head(var$coord, 4)

#plot variables
fviz_pca_var(PCA_results, col.var = "black", repel = TRUE)

#quality of representation
head(var$cos2, 4)

#You can visualize the cos2 of variables on all the dimensions using the corrplot package:
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)

# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(PCA_results, choice = "var", axes = 1:2)

# Color by cos2 values: quality on the factor map
fviz_pca_var(PCA_results, 
             col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE
)

#Contributions of variables to PCs
#The larger the value of the contribution, the more the variable contributes to the component
head(var$contrib, 4)
corrplot(var$contrib, is.corr=FALSE) 

# Contributions of variables to PC1
fviz_contrib(PCA_results, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(PCA_results, choice = "var", axes = 2, top = 10)
# The total contribution to PC1 and PC2 is obtained with the following R code:
fviz_contrib(PCA_results, choice = "var", axes = 1:2, top = 10)

#colour by group
# Create a grouping variable using kmeans
# Create 3 groups of variables (centers = 3)
set.seed(123)
res.km <- kmeans(var$coord, centers = 3, nstart = 25)
grp <- as.factor(res.km$cluster)
# Color variables by groups
fviz_pca_var(PCA_results, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")
#
fviz_pca_var(PCA_results, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

# dimension description #geht nicht # inconvenient data
res.desc <- dimdesc(PCA_results, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1

#Biplot
#To make a simple biplot of individuals and variables, type this:
fviz_pca_biplot(PCA_results, repel = TRUE,
                  col.var = "#2E9FDF", # Variables color
                  col.ind = "#696969"  # Individuals color
  )



#NEU 28.08.22 ## with trait arrows and elipses ################################
# Change the color by groups, add ellipses
fviz_pca_biplot(PCA_results, label="var", 
                ggtheme = theme_gray(), 
                col.var = "black", 
                habillage=DATA_pca2$Accession,
                addEllipses=TRUE, 
                ellipse.level=0.95,
                max.overlaps = 0.1)