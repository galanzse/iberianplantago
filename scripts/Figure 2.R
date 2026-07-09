

# FIGURE 2

library(tidyverse)
library(ggpubr)

# data
source('scripts/2. production x traits.R')


# figure 2
data = traits %>% dplyr::select(code, lifespan, habitat, infl_length, n_flowers_x_infl , anther_length, height, n_grains_anther, pollen_size, n_grains_ind)

g1 = ggplot(aes(x=height, y=pollen_size, label=code), data=data) +
  geom_smooth(method='lm', se=F, col='black') +
  geom_point(aes(color=lifespan, shape=habitat), size=3) +
  geom_text_repel(size=3) +
  labs(x='Maximum height (cm)', y='Pollen grain size (µm)') +
  theme_bw() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.95,0.65),
        legend.justification = c(1, 0),
        
        # Transparent background
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent", colour = NA),
        legend.key = element_rect(fill = "transparent", colour = NA),
        
        # Smaller text
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        
        # Less space between legend entries
        legend.spacing.y = unit(0.05, "cm"),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.4, "cm"))

g2 = ggplot(aes(x=anther_length, y=n_grains_anther, label=code), data=data) +
  geom_smooth(method='lm', se=F, col='black') +
  geom_point(aes(color=lifespan, shape=habitat), size=3) +
  geom_text_repel(size=3) +
  labs(x='Anther length (cm)', y='Pollen grains per flower') +
  theme_bw() +
  theme(legend.position='none')

g3 = ggplot(aes(x=infl_length, y=n_flowers_x_infl, label=code), data=data) +
  geom_smooth(method='lm', se=F, col='black') +
  geom_point(aes(color=lifespan, shape=habitat), size=3) +
  geom_text_repel(size=3) +
  labs(x='Inflorescence length (cm)', y='Flowers per inflorescence') +
  theme_bw() +
  theme(legend.position='none')

ggarrange(g1, g2, g3, ncol=3, widths=c(1,1,1))

lm(pollen_size ~ height, data=data) %>% anova()
lm(n_grains_anther ~ anther_length, data=data) %>% anova()
lm(n_flowers_x_infl ~ infl_length, data=data) %>% anova()

sd(data$pollen_size, na.rm = TRUE) / mean(data$pollen_size, na.rm = TRUE) * 100
sd(data$anther_length, na.rm = TRUE) / mean(data$anther_length, na.rm = TRUE) * 100
sd(data$n_grains_anther, na.rm = TRUE) / mean(data$n_grains_anther, na.rm = TRUE) * 100

# 
ggplot(aes(x=lifespan, y=log(n_grains_ind), label=code), data=data) +
  geom_boxplot() +
  geom_jitter(aes(color=lifespan, shape=habitat), size=3, width=0.05) +
  geom_text_repel(size=3) +
  labs(x='Lifespan', y='log(Pollen grains per plant)') +
  theme_bw()


