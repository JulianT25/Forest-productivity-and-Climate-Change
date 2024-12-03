#####BASAL AREA DYNAMICS AND PRODUCTIVITY
library(ggsignif)
##################################BASAL AREA DYNAMICS
BA_evol <- ClimaSPSW_new %>% 
  mutate( 
    Biome = str_replace(Biome, "Temp_north", "Temperate \n north"),
    Biome = str_replace(Biome, "Temp_south", "Temperate \n south")
  ) %>% 
  rename(
    NFI1 = ba_ha2,
    NFI2 = ba_ha3,
    NFI3 = ba_ha4
  ) %>%
  mutate(
    Biome = as.factor(Biome)) %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean", "Temperate south", "Temperate north", "Boreal")) %>% 
  select(NFI1, NFI2, NFI3, Biome) %>% 
  pivot_longer(cols = starts_with("NFI"), names_to = "years", values_to = "values") %>%
  ggplot(aes(x = years, y = values, fill = years)) +
  geom_boxplot(aes(fill = Biome, alpha = factor(years)), outlier.shape = NA) +
  scale_alpha_manual(values = c(0.2, 0.4, 0.7)) +
  scale_fill_manual(values = c("#FFEB3B", "#7CB342", "#2E7D32", "#2196F3")) +
  coord_cartesian(ylim = c(0, 70)) +
  facet_wrap(~Biome, nrow = 1) +
  labs(
    y = expression("Stand basal area (m"^2 * "/ha"^{-1} * ")"),
    x = ""
  ) + 
  scale_x_discrete(labels = c(
    "NFI1" = expression(NFI[1]),
    "NFI2" = expression(NFI[2]),
    "NFI3" = expression(NFI[3])
  )) +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(size = 1, color = "black"),
    axis.line.y.right = element_line(size = 1, color = "black"),
    axis.line.x.top = element_line(size = 1, color = "black"),
    axis.line.x.bottom = element_line(size = 1, color = "black"),
    axis.text = element_text(size = 22, family = "TT Arial"),
    strip.text.x = element_text(size = 22, family = "TT Arial"),
    strip.text.y = element_text(size = 22, family = "TT Arial"),
    axis.title = element_text(size = 20, family = "TT Arial"),
    legend.position = "none"
  )

BA_evol


########################BASAL AREA ABSOLUTE RATES 

ratesBA_evol <- ClimaSPSW_new %>% 
  mutate( 
    Biome = str_replace(Biome, "Temp_north", "Temperate \n north"),
    Biome = str_replace(Biome, "Temp_south", "Temperate \n south")
  ) %>% 
  rename(
    P1 = rAB32rel,
    P2 = rAB43rel
  ) %>%
  mutate(
    Biome = as.factor(Biome)) %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean", "Temperate \n south", "Temperate \n north", "Boreal")) %>% 
  select(P1, P2, Biome) %>% 
  pivot_longer(cols = starts_with("P"), names_to = "years", values_to = "values") %>%
  ggplot(aes(x = years, y = values, fill = years)) +
  geom_boxplot(aes(fill = Biome, alpha = factor(years)), outlier.shape = NA) +
  scale_alpha_manual(values = c(0.4, 0.7)) +
  scale_fill_manual(values = c("#FFEB3B", "#7CB342", "#2E7D32", "#2196F3")) +
   coord_cartesian(ylim = c(-10, 20)) +
  facet_wrap(~Biome, nrow = 1) +
  labs(
    y = expression("Productivity (% ha"  ^{-1} * " yr"  ^{-1} * ")"),
    x = ""
  ) + 
  scale_x_discrete(labels = c(
    "P1" = expression(P[1]),
    "P2" = expression(P[2])
  )) +
  theme_minimal() +
  theme(
    axis.line.y.left = element_line(size = 1, color = "black"),
    axis.line.y.right = element_line(size = 1, color = "black"),
    axis.line.x.top = element_line(size = 1, color = "black"),
    axis.line.x.bottom = element_line(size = 1, color = "black"),
    axis.text = element_text(size = 22, family = "TT Arial"),
    strip.text.x = element_text(size = 22, family = "TT Arial"),
    strip.text.y = element_text(size = 22, family = "TT Arial"),
    axis.title = element_text(size = 20, family = "TT Arial"),
    legend.position = "none"
  )

ratesBA_evol

