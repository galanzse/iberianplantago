

# SEÑAL FILOGENETICA DE LOS RASGOS

library(readxl)
library(tidyverse)
library(phytools)   
library(ape)
# library(Rphylopars)



# filogenia
load("phylogeny/imputed_tree.RData")


# rasgos
source('scripts/2. production x traits.R')

traits = traits %>% merge(taxonomy[,c('code','species')]) %>% dplyr::select(species, SLA, height, seed_mass, anther_length, pollen_size, n_infl, infl_length, n_flowers_x_infl, n_grains_flower, n_grains_infl, n_grains_ind)

rownames(traits) <- traits$species
traits$species <- NULL
rownames(traits) = gsub(' ', '_', rownames(traits))


# match filogenia x rasgos
imputed_tree = keep.tip(imputed_tree, rownames(traits))
traits <- traits[imputed_tree$tip.label, ]



# CALCULO SEÑAL FILOGENETICA
results <- data.frame()

for(tr in colnames(traits)) {
  
  # selecciono rasgos
  x <- traits[, tr]
  names(x) = rownames(traits)
  
  # Pagel lambda
  lambda_res <- phylosig(imputed_tree, x, method="lambda", test=TRUE )
  
  # Blomberg K
  K_res <- phylosig(imputed_tree, x, method="K", test=TRUE, nsim=999)
  
  # almaceno resultados
  results <- rbind(results,
                   data.frame(trait=tr, lambda=lambda_res$lambda, p_lambda=lambda_res$P,
                              K=K_res$K, p_K_WN=K_res$P)
                   )
}


# comparación con un modelo BM: creo funcion y aplico
BM_test <- function(tree, x, nsim = 999){
  
  K_obs <- phylosig(tree, x, method="K", test=FALSE)
  
  K_BM <- replicate(nsim, {
    # simulacion de la evolución de un rasgo sobre el árbol bajo movimiento browniano
    sim_trait <- fastBM(tree)
    phylosig(tree, sim_trait, method = "K", test = FALSE)
  })
  
  p_BM <- mean(K_BM >= K_obs)
  
  c(K_obs = K_obs, mean_BM = mean(K_BM), p_BM = p_BM)
}

BM_results <- lapply(traits, BM_test, tree = imputed_tree, nsim = 999)
BM_results <- do.call(rbind, BM_results)


# 
final_results <- results
final_results$mean_K_BM <- BM_results[, "mean_BM"]
final_results$p_K_BM <- BM_results[, "p_BM"]


# Continuous traits showed generally weak phylogenetic signal. Pagel's λ was close to zero for most traits, and Blomberg's K values were consistently below one. None of the traits exhibited significant phylogenetic signal relative to a white-noise expectation. Seed mass showed the strongest tendency toward phylogenetic conservatism (λ ≈ 1, K = 0.68), although support was only marginal (P = 0.07).The limited number of taxa may reduce statistical power to detect phylogenetic signal.


