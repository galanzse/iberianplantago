

library(tidyverse)
library(readxl)
library(ggh4x)



# import data
taxonomy <- read_excel("data/data.xlsx", sheet='taxonomy') %>% dplyr::select(-scientific_name)
pollen_size <- read_excel("data/data.xlsx", sheet='pollen_size') %>% dplyr::select(-source, -sample)

# añado taxonomia
pollen_size <- merge(taxonomy, pollen_size, by='species') %>% dplyr::select(-species_complete)

# calculo ratio
pollen_size$ratio = pollen_size$major_axis_µm / pollen_size$minor_axis_µm

# elimino 1 outlier
pollen_size = pollen_size[pollen_size$ratio<1.15,]


# relacion entre ejes
ggplot(aes(y= major_axis_µm, x=minor_axis_µm, color=code), data=pollen_size) +
  geom_point() + geom_abline(intercept=0, slope=1) +
  labs(y="Major axis (µm)", x="Minor axis (µm)") +
  theme_bw()

# long
pollen_size_long = pollen_size %>% pivot_longer(c(5:7), names_to = 'axis', values_to = 'value')
pollen_size_long$axis = factor(pollen_size_long$axis, levels=c("major_axis_µm", "minor_axis_µm", "ratio"))
levels(pollen_size_long$axis) = c("Major axis (µm)", "Minor axis (µm)", "Ratio")

# # 
# pollen_size_long$code = factor(pollen_size_long$code,
#                                levels=c('PlAf', 'PlSe', 'PlAl', 'PlBe', 'PlLa', 'PlLn', 'PlLo',
#                                         'PlMo', 'PlCo', 'PlAp', 'PlHo', 'PlSr', 'PlMj', 'PlMe'))
pollen_size_long$section = factor(pollen_size_long$section, levels=c("Albicans", "Hymenopsyllium", "Lanceifolia", "Montana", "Coronopus", "Maritima", "Lamprosantha", "Plantago", "Psyllium"))

# diferencias entre especies
temp = pollen_size_long[pollen_size_long$axis!="Minor axis (µm)",]
temp$axis = droplevels(temp$axis)
ggplot(aes(x = section, y = value, fill = subgenus), data=temp) +
  geom_boxplot() +
  labs(x=NULL, y=NULL) +
  theme_bw() +
  theme(legend.position = 'top', axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap2(~axis, scales = "free", nrow = 2, axes = "margins")
  

