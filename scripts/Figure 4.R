

# DEDNROGRAMAS

library(tidyverse)
library(ape)

source("scripts/3.1 phylogeny x dendrogram.R")

# plot
par(mfrow=c(1,2), mar=c(2,2,2,10), cex = 0.8)

# coloreo por subgenero
taxonomy <- read_excel("data/data.xlsx", sheet='taxonomy') %>% dplyr::select(species, subgenus)
subgenus = taxonomy$subgenus
names(subgenus) = taxonomy$species
cols <- c(Plantago="darkgreen", Psyllium="black", Coronopus="orange", Albicans="purple")

# dendrograma ecologico
labels(vegetative_den) <- gsub("_", " ", labels(vegetative_den))
labs <- labels(vegetative_den)
label_cols <- cols[subgenus[labs]]
vegetative_den <- set(vegetative_den, "labels_col", value = label_cols)
plot(vegetative_den, main='Ecological traits', cex=2, horiz=TRUE)

# legend
legend("topleft", legend=names(cols), fill=cols, bty="n", title="Subgenus", cex=1)

# dendrograma reproductivo
labels(reproductive_den) <- gsub("_", " ", labels(reproductive_den))
labs <- labels(reproductive_den)
label_cols <- cols[subgenus[labs]]
reproductive_den <- set(reproductive_den, "labels_col", value = label_cols)
plot(reproductive_den, main='Reproductive traits', horiz=TRUE)



# reset par
par(cex = 1)


