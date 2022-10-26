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
View(DATA)

#drop columns
DATA = DATA[,-c(1:2, 4:5)]
View(DATA)

# correlation matrix across all genotypes
# cor function
str(DATA)
cor(DATA[,2:33], DATA[,2:33], method = "pearson") #geht nur für numerische Variblen

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