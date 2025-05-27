# PCR

# Library
library(stats)
library(dplyr)
library(readxl)

# Data load
setwd("/Data")
data <- read_xlsx("data_19industries.xlsx")

# PCA
predictors <- data[,4:(length(data)-1)]
pca_result <- prcomp(predictors, scale.=TRUE)
pc_scores <- pca_result$x



# Model - Young
n <- 5
data_combined <- data.frame(pc_scores, uni = data$uni_st_num, young = data$ratio_2034, middle = data$ratio_4064, old = data$ratio_65)
lm_combined <- lm(young~PC1 + PC2 + PC3 + PC4 + PC5 + uni, data = data_combined)
summary(lm_combined)

# Model - Middle
lm_combined <- lm(middle~PC1 + PC2 + PC3 + PC4 + PC5, data = data_combined)
summary(lm_combined)

# Model - Old
lm_combined <- lm(old~PC1 + PC2 + PC3 + PC4 + PC5, data = data_combined)
summary(lm_combined)
