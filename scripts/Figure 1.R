

# CORRELACION ENTRE TRAITS


library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(corrplot)
library(vegan)
library(factoextra)
library(ggrepel)


# data
source('scripts/2. production x traits.R')

data <- traits %>%
  dplyr::select(SLA, height, seed_mass,
                pollen_size, anther_length,
                n_infl, infl_length, n_flowers_x_infl,
                n_grains_flower,
                n_grains_infl, n_grains_ind)

# names
colnames(data) = c("SLA", "height", "seed mass", "pollen size", "anther length", "n. infl.", "infl. length", "n. flowers per infl.", "n. grains per flower", "n. grains per infl.", "n, grains per ind.")

# Correlations
cor_matrix <- cor(data,
                  method = "spearman",
                  use = "pairwise.complete.obs")

# P-values
p_matrix <- cor.mtest(data, method = "spearman")

# Function to convert p-values to significance stars
stars <- function(p){
  ifelse(p < 0.001, "***",
         ifelse(p < 0.01, "**",
                ifelse(p < 0.05, "*", "")))
}

# Labels
labels <- matrix(
  paste0(sprintf("%.2f", cor_matrix), stars(p_matrix)),
  nrow = nrow(cor_matrix)
)

labels[lower.tri(labels)] <- ""

par(mfrow = c(1,1), mar = c(4,4,4,4))

corrplot(cor_matrix,
         method = "square",
         type = "upper",
         diag = TRUE,
         col = "white",
         border = "grey80",
         tl.col = "black",
         tl.cex = 0.8,
         cl.pos = "n")

n <- nrow(cor_matrix)

for(i in 1:n){
  for(j in i:n){
    text(j, n - i + 1,
         labels[i, j],
         cex = 0.75)
  }
}

