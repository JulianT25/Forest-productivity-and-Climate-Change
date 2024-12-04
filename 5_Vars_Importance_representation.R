
Variable_Impoortance <- VarImp_GEB %>% 
  #filter(Biome == "Boreal") %>%
  #filter(Prelative > 0) %>%
  mutate(
    Variable = fct_relevel(Variable,
                           "Heatwave anomaly","Drought anomaly","Prcp anomaly", "Temp anomaly",
                           "Heatwave", "Drought intensity","Precipitation","Temperature","Basal_area"), 
    Period = fct_relevel(Period,
                         "P2","P1"),
    Biome = fct_relevel(Biome,
                        "Boreal",
                        "Temp_North",
                        "Temp_South",
                        "Mediterranean"))  %>% 
  arrange(Period) %>% 
  ggplot( aes(x = Variable, y = Prelative)) +
  geom_bar(aes(fill = Biome, alpha = factor(Period)), stat = "identity",  color = "black", width = 0.81, 
           position = position_dodge2(width = 0.1)) +
  scale_fill_manual(values = c("#2196F3","#2E7D32","#7CB342","#FFEB3B")) +
  scale_alpha_manual(values = c(0.9,0.25))+
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5), size = 0.8, color = "gray")+
  #geom_text(aes(label = Period), hjust = -1, position = position_dodge2(width = .8) ) +
  facet_wrap(~Biome)+
  xlab('') +
  ylab('Relative Importance') +
  theme_minimal() + 
  theme(
    axis.line.y.left = element_line(),
    axis.line.y.right = element_line(),
    axis.line.x.top =  element_line(),
    axis.line.x.bottom = element_line(),
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 17),
    strip.text.x = element_text(size = 17),
    strip.text.y = element_text(size = 17),
    axis.title = element_text(size = 17),
    legend.position = "none") +
  coord_flip()


Variable_Impoortance
