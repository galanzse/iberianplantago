

# FILOGENIA DE PLANTAGO

# library(devtools)
# pak::pak("jinyizju/V.PhyloMaker2")
# library(V.PhyloMaker2)
library(tidyverse)
library(ape)
library(vegan)
# pak::pak("iramosgutierrez/randtip")
library(randtip)



# FILOGENIA BASE CON V.PhyloMaker2 ####

powo_spain = read.csv("phylogeny/plantago_powo_spain.csv", sep=";")

# # species
# df_taxa <- data.frame(
#   species = powo_spain$species,
#   genus = "Plantago",
#   family = "Plantaginaceae"
# )
# 
# # phylogeny
# phylo_plantago <- phylo.maker(sp.list = df_taxa, tree = GBOTB.extended.TPL, nodes = nodes.info.1.TPL, scenarios = "S3")
# save(phylo_plantago, file = "phylogeny/phylo_plantago.RData")

load("phylogeny/phylo_plantago.RData")
phylo_plantago$species.list
tree_plantago <- phylo_plantago$scenario.3
is.null(tree_plantago$edge.length)

# fix subulata to holosteum
tree_plantago$tip.label[tree_plantago$tip.label == "Plantago_subulata"] <- "Plantago_holosteum"

class(tree_plantago)
is.ultrametric(tree_plantago)

# plot
plot(tree_plantago, no.margin = TRUE, cex = 1)

# elimino especies imputadas
dt = phylo_plantago$species.list$species[phylo_plantago$species.list$status=='bind']
dt = c(dt, "Plantago bellardii")
tree_plantago = drop.tip(tree_plantago, gsub(' ', '_', dt))



# RESOLVER CON LITERATURA: Ronsted et al. 2002; Hassemera et al. 2019; Shipunov et al. 2021 ####

# cambiamos la info de genero por la de seccion
powo_spain$species[powo_spain$species=='Plantago subulata'] <- "Plantago holosteum"
powo_spain$sect_epith <- paste(powo_spain$section, sub("^Plantago\\s+", "", powo_spain$species))
powo_spain$species = gsub(' ', '_', powo_spain$species)
powo_spain$sect_epith = gsub(' ', '_', powo_spain$sect_epith)
tree_plantago$tip.label = powo_spain$sect_epith[match(tree_plantago$tip.label, powo_spain$species)]

# preparo imputacion: taxones infraspecificos como supraespecificos
imp_species = powo_spain$sect_epith[!(powo_spain$sect_epith %in% tree_plantago$tip.label)]
info_species <- randtip::build_info(species=imp_species, tree=tree_plantago, mode='backbone', find.ranks=FALSE)
# assign subtribes so randtip is able to impute monotypic sections
temp <- unique(setNames(powo_spain[c("section", "subgenus")], c("genus", "subtribe")))
info_species$subtribe <- temp$subtribe[match(info_species$genus, temp$genus)]
# 
check_species <- randtip::check_info(info_species, tree=tree_plantago)
input_species = randtip::info2input(info_species, tree=tree_plantago)

# imputo
imputed_tree <- randtip::rand_tip(input=input_species, tree=tree_plantago, rand.type='random',
                                  respect.mono=T,  prob=F,  use.stem=T,  prune=F, verbose=F)

plot(imputed_tree, cex = 0.7, main='Iberian Plantago')

# recupero nombres originales
imputed_tree$tip.label = powo_spain$species[match(imputed_tree$tip.label, powo_spain$sect_epith)]
# save(imputed_tree, file = "phylogeny/imputed_tree.RData")



# PLOT FINAL ####

load("C:/Users/javie/OneDrive/ACADEMICO/proyectos/plantago/phylogeny/imputed_tree.RData")

par(mar=c(1,1,2,1))
plot(imputed_tree, cex = 0.7, main='Iberian Plantago') # ; nodelabels()

nodelabels("subg. Albicans", node = 44, cex = 0.7, frame = "rect", col = "black")
nodelabels("subg. Coronopus", node = 35,  cex = 0.7, frame = "rect", col = "black")
nodelabels("subg. Plantago", node = 33, cex = 0.7, frame = "rect", col = "black")
nodelabels("subg. Psyllium", node = 56, cex = 0.7, frame = "rect", col = "black")

nodelabels("sect. Albicans", node = 54, pch=17, cex = 0.6, frame = "none", col = "blue")
nodelabels("sect. Lanceifolia", node = 47, pch=17, cex = 0.6, frame = "none", col = "blue")
nodelabels("sect. Montana", node = 50, pch=17, cex = 0.6, frame = "none", col = "blue")
nodelabels("sect. Maritima", node = 36, pch=17, cex = 0.6, frame = "none", col = "blue")
nodelabels("sect. Coronopus", node = 40, pch=17, cex = 0.6, frame = "none", col = "blue")
nodelabels("sect. Plantago", node = 34, pch=17, cex = 0.6, frame = "none", col = "blue")



# Figura para artículo (especies de estudio) ####

load("phylogeny/imputed_tree.RData")
imputed_tree$tip.label = gsub('_', ' ', imputed_tree$tip.label)

# species included in the study
species <- read_excel("data/data.xlsx", sheet='taxonomy') %>% dplyr::select(species) %>% deframe()

# pruned tree
imputed_tree = keep.tip(imputed_tree, species)


par(mfrow=c(1,1), mar=c(1,1,2,1))
plot(imputed_tree, cex = 0.9)
# nodelabels()

nodelabels("subg. Albicans", node = 22, cex = 0.7, frame = "rect", col = "black")
nodelabels("subg. Coronopus", node = 18,  cex = 0.7, frame = "rect", col = "black")
nodelabels("subg. Plantago", node = 17, cex = 0.7, frame = "rect", col = "black")
nodelabels("subg. Psyllium", node = 27, cex = 0.7, frame = "rect", col = "black")

nodelabels("sect. Lanceifolia", node = 25, pch=17, cex = 0.6, frame = "none", col = "blue")
nodelabels("sect. Maritima", node = 19, pch=17, cex = 0.6, frame = "none", col = "blue")


