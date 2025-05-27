# Inudstry_PCA

# Library
library('corrr')
library(ggcorrplot)
library("FactoMineR")
library('factoextra')
library(dplyr)
library(readr)
library(tidyr)
library(readxl)

# Wd# Wddplyr
setwd('/Data')

# Data
data <- read_xlsx('data_19industries.xlsx')
str(data)
colSums(is.na(data))

industries <- data[,4:(length(data)-1)]
industries_norm <- scale(industries)

data.pca <- princomp(industries_norm)
summary(data.pca)

# Calculate eigenvalues
eigenvalues <- data.pca$sdev^2
print(eigenvalues)
# According to Kaiser's criterion, five components are appropriate

data.pca$loadings[, 1:5]
write.csv(data.pca$loadings[, 1:5], file = 'Industries_PCA_comp5.csv')

fviz_eig(data.pca, addlabels = TRUE)

# Graph of the variables
fviz_pca_var(data.pca, col.var = "black")

fviz_cos2(data.pca, choice = "var", axes = 2)

fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

