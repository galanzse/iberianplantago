

# CORRELACION ENTRE TRAITS


library(Hmisc)
library(tidyverse)
library(ggplot2)


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

# Spearman correlation
res <- rcorr(as.matrix(data), type = "spearman")

cor_mat <- res$r
p_mat   <- res$P


# Remove diagonal
diag(cor_mat) <- NA
diag(p_mat) <- NA

cor_mat[lower.tri(cor_mat, diag = TRUE)] <- NA
p_mat[lower.tri(p_mat, diag = TRUE)] <- NA

cor_df <- as.data.frame(as.table(cor_mat))
p_df   <- as.data.frame(as.table(p_mat))

df <- left_join(cor_df, p_df,
                by = c("Var1","Var2"),
                suffix = c(".cor",".p")) %>%
  rename(correlation = Freq.cor,
         p = Freq.p) %>%
  filter(!is.na(correlation))


cor_df <- as.data.frame(as.table(cor_mat))
p_df   <- as.data.frame(as.table(p_mat))

df <- left_join(cor_df, p_df,
                by = c("Var1","Var2"),
                suffix = c(".cor",".p")) %>%
  rename(correlation = Freq.cor,
         p = Freq.p) %>%
  filter(!is.na(correlation))


df <- df %>%
  mutate(sig = case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ ""
  ),
  label = sprintf("%.2f%s", correlation, sig),
  fontface = ifelse(p < 0.05, "bold", "plain"))


ggplot(df, aes(Var1, Var2, fill = correlation)) +
  geom_tile(color = "black") +
  geom_text(aes(label = label,
                fontface = fontface),
            size = 3) +
  scale_fill_gradient2(
    low = "white",
    mid = "white",
    high = "white",
    midpoint = 0,
    limits = c(-1,1),
    name = "Spearman\nρ"
  ) +
  coord_fixed() +
  labs(x='', y='') +
  theme_bw() +
  theme(
    legend.position = 'none',
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )


