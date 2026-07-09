

# Algunos plots mas avanzados

source('scripts/2. production x traits.R')
library(ggpubr)


# boxplots para cada categoria
traits_long = traits %>% pivot_longer(c(4:6,11:19), names_to = 'trait')

traits_long$trait = factor(traits_long$trait, levels=c("SLA", "height", "seed_mass", "anther_length", "pollen_size", "n_infl", "infl_length", "n_flowers_x_infl", "n_grains_anther", "n_grains_flower", "n_grains_infl", "n_grains_ind"))

ggplot(aes(y=value, x=subgenus), data=traits_long) +
  geom_boxplot(aes(fill=subgenus)) +
  geom_jitter(width=0.10, alpha=0.5) +
  facet_wrap(~trait, scales='free_y') +
  theme_bw() +
  theme(legend.position = 'none')

ggplot(aes(y=value, x=lifespan), data=traits_long) +
  geom_boxplot(aes(fill=lifespan)) +
  geom_jitter(aes(shape=subgenus), width=0.10, alpha=0.7) +
  stat_compare_means(method = "wilcox.test", label = "p.signif", vjust = 0.9, hjust=-2) +
  facet_wrap(~trait, scales='free_y') +
  theme_bw() +
  theme(legend.position = 'top')

ggplot(aes(y=value, x=habitat), data=traits_long) +
  geom_boxplot(aes(fill=habitat)) +
  geom_jitter(aes(shape=subgenus), width=0.10, alpha=0.7) +
  stat_compare_means(method = "wilcox.test", label = "p.signif", vjust = 0.9, hjust=-2) +
  facet_wrap(~trait, scales='free_y') +
  theme_bw() +
  theme(legend.position = 'top')



# vamos a colorear filogenia y dendrograma por factores: section, subgenus, biotype, habitat ####

# data
source('scripts/3.1. phylogeny x dendrogram.R')
par(mfrow=c(1,1))

vegetative_den
reproductive_den
integral_den
phylogeny = as.dendrogram(phylogeny)


# SUBGENUS
groups <- traits$subgenus
names(groups) <- rownames(traits)
tip_order <- labels(integral_den) # reorder groups to match dendrogram tip order
group_ordered <- groups[tip_order]
label_colors <- c("Albicans"="yellow", "Coronopus"="brown", "Plantago"="green", "Psyllium"="blue")
labels_colors(integral_den) <- label_colors[group_ordered]
plot(integral_den)


# BIOTYPE
groups <- traits$biotype
names(groups) <- rownames(traits)
tip_order <- labels(integral_den) # reorder groups to match dendrogram tip order
group_ordered <- groups[tip_order]
label_colors <- c("therophyte"="green", "hemicryptophyte"="blue", "chamaephyte"="brown")
labels_colors(integral_den) <- label_colors[group_ordered]
plot(integral_den)


# HABITAT
groups <- traits$habitat
names(groups) <- rownames(traits)
tip_order <- labels(integral_den) # reorder groups to match dendrogram tip order
group_ordered <- groups[tip_order]
label_colors <- c("disturbed"="brown", "natural"="green")
labels_colors(integral_den) <- label_colors[group_ordered]
plot(integral_den)


