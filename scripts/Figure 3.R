

# PCA

library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(vegan)
library(factoextra)
library(ggrepel)


# data
source('scripts/2. production x traits.R')

par(mfrow=c(1,1), mar=c(4,4,4,4))

data <- traits %>%
  dplyr::select(code,
                SLA, height, seed_mass,
                pollen_size, anther_length,
                n_infl, infl_length, n_flowers_x_infl,
                n_grains_anther, n_grains_flower, n_grains_infl, n_grains_ind)


sp_PCA <- data %>% select(-code, -SLA, -seed_mass, -n_grains_anther) %>% prcomp(scale.=TRUE)
eigenvals(sp_PCA)
plot(sp_PCA, type = "l", main = "Scree Plot"); abline(1,0); eigenvals(sp_PCA) # 3 PC relevantes

# Figuras para artículo
pca_scores_sp = taxonomy %>%
  merge(read_excel("data/data.xlsx", sheet='flora_iberica') %>%
          dplyr::select(code, biotype, lifespan, habitat)) %>%
  merge(data.frame(code=data$code, sp_PCA$x[,1:4]))

pca_scores_sp$habitat = as.factor(pca_scores_sp$habitat)
pca_scores_sp$biotype = as.factor(pca_scores_sp$biotype)
pca_scores_sp$subgenus = as.factor(pca_scores_sp$subgenus)

# coordenadas de las flechas
datos_flechas_sp <- fviz_pca_var(sp_PCA)$data
factor_expansion <- 3.5

ggplot(pca_scores_sp, aes(PC1, PC2)) +
  
  geom_point(aes(shape = habitat, color = lifespan), size = 3) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray80") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray80") +

  geom_text_repel(data = pca_scores_sp,
                  aes(x = PC1, y = PC2, label = code),
                  size = 4, fontface = "italic", color = "black",
                  box.padding = 0.4, point.padding = 0.4, max.overlaps = Inf,
                  min.segment.length = Inf) +
  
  geom_segment(data = datos_flechas_sp,
               aes(x = 0, y = 0, xend = x * factor_expansion, yend = y * factor_expansion),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "gray20", linewidth = 0.5) +
  
  geom_text_repel(data = datos_flechas_sp,
                  aes(x = x * factor_expansion, y = y * factor_expansion, label = name),
                  size = 3, color = "blue",
                  box.padding = 0.5,
                  min.segment.length = Inf) +
  
  scale_shape_manual(values = c("disturbed" = 16, "natural" = 17)) +

  labs(
    title = NULL,
    x = paste0("PC1 (", round(sp_PCA$sdev[1]^2 / sum(sp_PCA$sdev^2) * 100, 1), "%)"),
    y = paste0("PC2 (", round(sp_PCA$sdev[2]^2 / sum(sp_PCA$sdev^2) * 100, 1), "%)"),
  ) +
  
  theme_bw() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5) 
  )


