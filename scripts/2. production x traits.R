

# CALCULO LA PRODUCCION POLINICA A NIVEL INDIVIDUAL Y ESPECIFICO


library(tidyverse)
library(readxl)
library(ape)


# taxonomia
taxonomy <- read_excel("data/data.xlsx", sheet='taxonomy') %>% dplyr::select(-species_complete, -scientific_name)


# produccion polinica
pollen_production <- read_excel("data/data.xlsx", sheet='pollen_production')

# media de las 6 anteras x inflorescencia
pollen_production = pollen_production %>%
  reframe(.by=c(code  , id_ind, id_infl),
          n_infl_ind = unique(n_infl_ind),
          length_infl_cm = unique(length_infl_cm),
          n_flowers_x_infl = unique(n_flowers_x_infl),
          n_grains_anther = mean(n_grains_anther))

# numero de granos por flor y por inflorescencia
pollen_production$n_grains_flower = pollen_production$n_grains_anther * 4
pollen_production$n_grains_infl = pollen_production$n_grains_flower * pollen_production$n_flowers_x_infl

# agregamos por individuo
ind_pollen_production = pollen_production %>%
  reframe(.by = c(code, id_ind, n_infl_ind),
          infl_length = mean(length_infl_cm),
          n_flowers_x_infl = mean(n_flowers_x_infl),
          n_grains_anther = mean(n_grains_anther),
          n_grains_flower = mean(n_grains_flower),
          n_grains_infl = mean(n_grains_infl)) %>%
  mutate(n_grains_ind = n_grains_infl * n_infl_ind)
 


# rasgos promedio por especie
traits = ind_pollen_production %>%
  reframe(.by = code,
          n_infl = mean(n_infl_ind),
          infl_length = mean(infl_length),
          n_flowers_x_infl = mean(n_flowers_x_infl),
          n_grains_anther = mean(n_grains_anther),
          n_grains_flower = mean(n_grains_flower),
          n_grains_infl = mean(n_grains_infl),
          n_grains_ind = mean(n_grains_ind))

# añado el resto de rasgos
traits <- read_excel("data/data.xlsx", sheet='rasgos_try') %>%
  # taxonomia
  merge(taxonomy, by='species') %>%
  dplyr::select(code, section, subgenus, trait, value) %>%
  # promedio rasgos TRY
  summarise(.by = c(code, section, subgenus, trait),
            value = mean(value, na.rm=T)) %>%
            # value = paste(round(mean(value, na.rm=T), 2), '±', round(sd(value, na.rm=T), 2))) %>%
  pivot_wider(names_from=trait, values_from=value) %>%
  # flora iberica  
  merge(read_excel("data/data.xlsx", sheet='flora_iberica') %>%
          dplyr::select(-height_mean_cm, -original_data), by='code') %>%
  # tamaño de grano de polen  
  merge(read_excel("data/data.xlsx", sheet='pollen_size') %>%
          merge(taxonomy[,c('code','species')]) %>%
          dplyr::select(code, major_axis_µm) %>%
          summarise(.by = code, major_axis_µm = mean(major_axis_µm)), by='code') %>%
  # produccion x especie
  merge(traits, by='code')

# relleno NAs
traits$SLA[traits$code=='PlHo'] = 195.0
traits$SLA[traits$code=='PlSr'] = 103.4
traits$height[traits$code=='PlMo'] = 11
traits$seed_mass[traits$code=='PlSr'] = 0.76

# retengo y ordeno variables interesantes
colnames(traits)
colnames(traits)[colnames(traits)=="mean_anther_length_mm"] = 'anther_length'
colnames(traits)[colnames(traits)=="major_axis_µm"] = 'pollen_size'

# elimino rasgos con muchos NAs
traits = traits %>% dplyr::select(-leaf_length, -LDMC, -leaf_C, -leaf_N, -leaf_P)


