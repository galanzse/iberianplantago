

# Relaciono dendrogramas funcionales con la filogenia

library(tidyverse)
library(dendextend)
library(vegan)


# traits
source('scripts/2. production x traits.R')
traits = merge(taxonomy[,c('code','species')], traits)
rownames(traits) = gsub(' ', '_', traits$species)


# dendrograma ecologico
vegetative_mat = traits %>%
  dplyr::select(height, seed_mass, SLA) %>%
           cluster::daisy(stand=T, metric="euclidean")
vegetative_den = hclust(vegetative_mat, method='average') %>% as.dendrogram()

# dendrograma reproductivo
reproductive_mat = traits %>%
  dplyr::select(anther_length, pollen_size, n_infl, infl_length, n_flowers_x_infl, n_grains_anther) %>%
  cluster::daisy(stand=T, metric="euclidean")
reproductive_den = hclust(reproductive_mat, method='average') %>% as.dendrogram()

# # dendrograma integral
# integral_mat  = traits %>% 
#   dplyr::select(height, seed_mass, SLA, biotype, anther_length, pollen_size, n_infl, infl_length, n_flowers_x_infl, n_grains_anther) %>%
#   mutate(biotype = factor(biotype)) %>%
#   cluster::daisy(stand=T, metric="gower")
# integral_den = hclust(integral_mat, method='average') %>% as.dendrogram()
  


# basic plot
par(mfrow=c(2,1), mar=c(2,2,2,10))
plot(vegetative_den, main='Vegetative traits', horiz = TRUE)
plot(reproductive_den, main='Reproductive traits', horiz = TRUE)



# Filogenia
load("C:/Users/javie/OneDrive/ACADEMICO/proyectos/plantago/phylogeny/imputed_tree.RData")
phylogeny = keep.tip(imputed_tree, rownames(traits))
phylogeny_coph <- cophenetic(phylogeny)



# Comparamos rasgos y filogenia
par(mfrow=c(1,2), mar=c(10,2,2,2))
plot(phylogeny_coph, as.matrix(vegetative_mat))
plot(phylogeny_coph, as.matrix(reproductive_mat))
# plot(phylogeny_coph, as.matrix(integral_mat))

cor.test(phylogeny_coph[upper.tri(phylogeny_coph)], vegetative_mat[upper.tri(as.matrix(vegetative_mat))], method="spearman")
cor.test(phylogeny_coph[upper.tri(phylogeny_coph)], reproductive_mat[upper.tri(as.matrix(reproductive_mat))], method="spearman")
# cor.test(phylogeny_coph[upper.tri(phylogeny_coph)], integral_mat[upper.tri(as.matrix(integral_mat))], method="spearman")

mantel(as.dist(phylogeny_coph), as.dist(vegetative_mat), permutations = 9999)
mantel(as.dist(phylogeny_coph), as.dist(reproductive_mat), permutations = 9999)
# mantel(as.dist(phylogeny_coph), as.dist(integral_mat), permutations = 9999)


