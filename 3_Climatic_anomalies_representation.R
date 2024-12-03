library(patchwork)
library(tidyverse)
#####CLIMATIC VARIABLES DYNAMICS AND ANOMALIES

####Vars representation 

###################################################ANOMALIES######################################
#Temperature changes between periods (Biome)
#####
Temp_12_23_B <- ClimaSPSW_new %>% 
  mutate( 
    Biome = str_replace(Biome, "Temp_north", "Temperate \n north"),
    Biome = str_replace(Biome, "Temp_south", "Temperate \n south")
  ) %>% 
  rename(
    P1 = TempCC12,
    P2 = TempCC23) %>%
  mutate(
    Biome = as.factor(Biome)) %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean", "Temperate \n south", "Temperate \n north", "Boreal")) %>% 
  select(P1, P2,  Biome) %>% 
  pivot_longer(cols = starts_with("P"), names_to = "years", values_to = "values") %>%
  ggplot(aes(x = years, y = values, fill = years)) +
  geom_boxplot(aes(fill = Biome, alpha = factor(years)), outlier.shape = NA) +
  scale_alpha_manual(values = c(0.4, 0.9)) +
  scale_fill_manual(values = c("#FFEB3B", "#7CB342", "#2E7D32", "#2196F3")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_cartesian(ylim = c(-0.5, 2)) +
  facet_wrap(~Biome, nrow = 1) +
  labs(y = "", x = "") + 
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
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 17),
    strip.text.x = element_text(size = 17),
    strip.text.y = element_text(size = 17),
    axis.title = element_text(size = 17),
    legend.position = "none"
  )

Temp_12_23_B


############################################################################

#Precipitation changes between periods (FG/Biome) 

#############
#Precipitation changes between periods (Biome)
#####
Prcp_12_23_B <- ClimaSPSW_new %>% 
  # filter(FG1 %in% c("bldec", "bleve", "nleve")) %>% 
  #filter(Country=="SP") %>%
  mutate( 
    # FG1 = str_replace(FG1, "bldec", "Broad-leved \n deciduous"),
    # FG1 = str_replace(FG1, "bleve", "Broad-leaved \n evergreen"),
    # FG1 = str_replace(FG1, "nleve", "Needle-leaved \n evergreen"),
    Biome = str_replace(Biome, "Temp_north", "Temperate \n north"),
    Biome = str_replace(Biome, "Temp_south", "Temperate \n south")
  ) %>% 
  rename(
    P1 = PrcpCC12,
    P2 = PrcpCC23) %>%
  mutate(
    Biome = as.factor(Biome)) %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean", "Temperate \n south", "Temperate \n north", "Boreal")) %>% 
  select(P1, P2, Biome) %>% 
  filter(P1> -500) %>%
  filter(P2> -500) %>% 
  pivot_longer(cols = starts_with("P"), names_to = "years", values_to = "values") %>%
  ggplot(aes(x=years, y=values, fill = years)) +
  geom_boxplot(aes(fill = Biome, alpha = factor(years)), outlier.shape = NA) +
  scale_alpha_manual(values = c(0.4, 0.9)) +
  scale_fill_manual(values = c("#FFEB3B","#7CB342","#2E7D32","#2196F3")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_signif(comparisons = list(c(2,3),c(3,4),c(2,4)), y_position = c(1700, 1900, 2100), textsize = 4, na.rm = T,
  #            test = "wilcox.test", map_signif_level = c("***"=0.001, "**"=0.01, "*"=0.05))+
  #coord_cartesian(ylim= c(-200,200))+
  facet_wrap(~Biome, nrow = 1)+
  #stat_summary(fun = mean, geom = "point", shape=18, size = 3, color = "black")+
  labs(y = "", x = "")+ 
  scale_x_discrete(labels = c(
    "P1" = expression(P[1]),
    "P2" = expression(P[2])
  )) +
  theme_minimal()+
  theme(
    axis.line.y.left = element_line(size = 1, color = "black"),
    axis.line.y.right = element_line(size = 1, color = "black"),
    axis.line.x.top =  element_line(size = 1, color = "black"),
    axis.line.x.bottom = element_line(size = 1, color = "black"),
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 17),
    strip.text.x = element_text(size =17),
    strip.text.y = element_text(size = 17),
    axis.title = element_text(size = 17),
    legend.position = "none")

Prcp_12_23_B 
############################################################################

##SPEImin changes between periods (FG/Biome) 


#############
#SPEImin changes between periods (Biome)
#####
SPEImin_12_23_B <- ClimaSPSW_new %>% 
  # filter(FG1 %in% c("bldec", "bleve", "nleve")) %>% 
  #filter(Country=="SP") %>%
  mutate( 
    # FG1 = str_replace(FG1, "bldec", "Broad-leved \n deciduous"),
    # FG1 = str_replace(FG1, "bleve", "Broad-leaved \n evergreen"),
    # FG1 = str_replace(FG1, "nleve", "Needle-leaved \n evergreen"),
    Biome = str_replace(Biome, "Temp_north", "Temperate \n north"),
    Biome = str_replace(Biome, "Temp_south", "Temperate \n south")
  ) %>% 
  rename(
    P1 = SPEIminCC12,
    P2 = SPEIminCC23) %>%
  mutate(
    Biome = as.factor(Biome)) %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean", "Temperate \n south", "Temperate \n north", "Boreal")) %>% 
  select(P1, P2, Biome) %>% 
  pivot_longer(cols = starts_with("P"), names_to = "years", values_to = "values") %>%
  ggplot(aes(x=years, y=values, fill = years)) +
  geom_boxplot(aes(fill = Biome, alpha = factor(years)), outlier.shape = NA) +
  scale_alpha_manual(values = c(0.4, 1)) +
  scale_fill_manual(values = c("#FFEB3B","#7CB342","#2E7D32","#2196F3")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_signif(comparisons = list(c(2,3),c(3,4),c(2,4)), y_position = c(1700, 1900, 2100), textsize = 4, na.rm = T,
  #            test = "wilcox.test", map_signif_level = c("***"=0.001, "**"=0.01, "*"=0.05))+
  #coord_cartesian(ylim= c(-0.5,2.2))+
  facet_wrap(~Biome, nrow = 1)+
  #stat_summary(fun = mean, geom = "point", shape=18, size = 3, color = "black")+
  labs(y = "", x = "")+ 
  scale_x_discrete(labels = c(
    "P1" = expression(P[1]),
    "P2" = expression(P[2])
  )) +
  theme_minimal()+
  theme(
    axis.line.y.left = element_line(size = 1, color = "black"),
    axis.line.y.right = element_line(size = 1, color = "black"),
    axis.line.x.top =  element_line(size = 1, color = "black"),
    axis.line.x.bottom = element_line(size = 1, color = "black"),
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 17),
    strip.text.x = element_text(size =17),
    strip.text.y = element_text(size = 17),
    axis.title = element_text(size = 17),
    legend.position = "none")

SPEImin_12_23_B 



############################################################################

#############
#Heatwaves changes between periods (Biome)
######### 

Hw_12_23_B <- ClimaSPSW_new %>% 
  # filter(FG1 %in% c("bldec", "bleve", "nleve")) %>% 
  #filter(Country=="SP") %>%
  mutate( 
    # FG1 = str_replace(FG1, "bldec", "Broad-leved \n deciduous"),
    # FG1 = str_replace(FG1, "bleve", "Broad-leaved \n evergreen"),
    # FG1 = str_replace(FG1, "nleve", "Needle-leaved \n evergreen"),
    Biome = str_replace(Biome, "Temp_north", "Temperate \n north"),
    Biome = str_replace(Biome, "Temp_south", "Temperate \n south")
  ) %>% 
  rename(
    P1 = HwCC12,
    P2 = HwCC23) %>%
  mutate(
    Biome = as.factor(Biome)) %>%
  mutate(Biome = fct_relevel(Biome, "Mediterranean", "Temperate \n south", "Temperate \n north", "Boreal")) %>% 
  select(P1, P2, Biome) %>% 
  pivot_longer(cols = starts_with("P"), names_to = "years", values_to = "values") %>%
  ggplot(aes(x=years, y=values, fill = years)) +
  geom_boxplot(aes(fill = Biome, alpha = factor(years)), outlier.shape = NA) +
  scale_alpha_manual(values = c(0.4, 0.9)) +
  scale_fill_manual(values = c("#FFEB3B","#7CB342","#2E7D32","#2196F3")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_signif(comparisons = list(c(2,3),c(3,4),c(2,4)), y_position = c(1700, 1900, 2100), textsize = 4, na.rm = T,
  #            test = "wilcox.test", map_signif_level = c("***"=0.001, "**"=0.01, "*"=0.05))+
  #coord_cartesian(ylim= c(-0.35,0.75))+
  facet_wrap(~Biome, nrow = 1)+
  #stat_summary(fun = mean, geom = "point", shape=18, size = 3, color = "black")+
  labs(y = "", x = "")+ 
  scale_x_discrete(labels = c(
    "P1" = expression(P[1]),
    "P2" = expression(P[2])
  )) +
  theme_minimal()+
  theme(
    axis.line.y.left = element_line(size = 1, color = "black"),
    axis.line.y.right = element_line(size = 1, color = "black"),
    axis.line.x.top =  element_line(size = 1, color = "black"),
    axis.line.x.bottom = element_line(size = 1, color = "black"),
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 17),
    strip.text.x = element_text(size =17),
    strip.text.y = element_text(size = 17),
    axis.title = element_text(size = 17),
    legend.position = "none")

Hw_12_23_B

((Temp_12_23_B) + (Prcp_12_23_B)) /
  ((SPEImin_12_23_B) + (Hw_12_23_B))

