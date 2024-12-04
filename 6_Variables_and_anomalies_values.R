Table_Biome_Temp <- ClimaSPSW_new %>% 
  mutate(
    Biome = as.factor(Biome), 
    Biome = fct_relevel(Biome, c("Mediterranean", "Temp_south", "Temp_north", "Boreal"))
  ) %>% 
  group_by(Biome) %>%  
  summarise(
    Temp1_sd = round(sd(meanTemp_t1_p1, na.rm = TRUE), 2),
    Temp1 = round(mean(meanTemp_t1_p1, na.rm = TRUE), 2),
    Temp1max = round(max(meanTemp_t1_p1, na.rm = TRUE), 2),
    Temp1min = round(min(meanTemp_t1_p1, na.rm = TRUE), 2),
    Temp2_sd = round(sd(meanTemp_t1_p2, na.rm = TRUE), 2),
    Temp2 = round(mean(meanTemp_t1_p2, na.rm = TRUE), 2),
    Temp2max = round(max(meanTemp_t1_p2, na.rm = TRUE), 2),
    Temp2min = round(min(meanTemp_t1_p2, na.rm = TRUE), 2),
    Temp3_sd = round(sd(meanTemp_t1_p3, na.rm = TRUE), 2),
    Temp3 = round(mean(meanTemp_t1_p3, na.rm = TRUE), 2),
    Temp3max = round(max(meanTemp_t1_p3, na.rm = TRUE), 2),
    Temp3min = round(min(meanTemp_t1_p3, na.rm = TRUE), 2),
    TempLP_sd = round(sd(meanTemp_t_ref1_GEB, na.rm = TRUE), 2),
    TempLP = round(mean(meanTemp_t_ref1_GEB, na.rm = TRUE), 2),
    TempLPmax = round(max(meanTemp_t_ref1_GEB, na.rm = TRUE), 2),
    TempLPmin = round(min(meanTemp_t_ref1_GEB, na.rm = TRUE), 2)
  )
Table_Biome_Temp



Table_Rates_Temp <- ClimaSPSW_new %>% 
  mutate(
    Biome = as.factor(Biome), 
    Biome = fct_relevel(Biome, c("Mediterranean", "Temp_south", "Temp_north", "Boreal"))
  ) %>%
  group_by(Biome) %>% 
  summarise(
    Temp12_sd = round(sd(TempCC12, na.rm = TRUE), 2),
    Temp12 = round(mean(TempCC12, na.rm = TRUE), 2),
    Temp23_sd = round(sd(TempCC23, na.rm = TRUE), 2),
    Temp23 = round(mean(TempCC23, na.rm = TRUE), 2)
  )
Table_Rates_Temp  

##########################################
###############################

Table_Biome_Prcp <- ClimaSPSW_new %>% 
  mutate(
    Biome = as.factor(Biome), 
    Biome = fct_relevel(Biome, c("Mediterranean", "Temp_south", "Temp_north", "Boreal"))
  ) %>% 
  group_by(Biome) %>% 
  summarise(
    Prcp1_sd = round(sd(sumPrcp_p1, na.rm = TRUE), 2),
    Prcp1 = round(mean(sumPrcp_p1, na.rm = TRUE), 2),
    Prcp1max = round(max(sumPrcp_p1, na.rm = TRUE), 2),
    Prcp1min = round(min(sumPrcp_p1, na.rm = TRUE), 2),
    Prcp2_sd = round(sd(sumPrcp_p2, na.rm = TRUE), 2),
    Prcp2 = round(mean(sumPrcp_p2, na.rm = TRUE), 2),
    Prcp2max = round(max(sumPrcp_p2, na.rm = TRUE), 2),
    Prcp2min = round(min(sumPrcp_p2, na.rm = TRUE), 2),
    Prcp3_sd = round(sd(sumPrcp_p3, na.rm = TRUE), 2),
    Prcp3 = round(mean(sumPrcp_p3, na.rm = TRUE), 2),
    Prcp3max = round(max(sumPrcp_p3, na.rm = TRUE), 2),
    Prcp3min = round(min(sumPrcp_p3, na.rm = TRUE), 2),
    PrcpLP_sd = round(sd(sumPrcp_ref_GEB, na.rm = TRUE), 2),
    PrcpLP = round(mean(sumPrcp_ref_GEB, na.rm = TRUE), 2),
    PrcpLPmax = round(max(sumPrcp_ref_GEB, na.rm = TRUE), 2),
    PrcpLPmin = round(min(sumPrcp_ref_GEB, na.rm = TRUE), 2)
  )
Table_Biome_Prcp

Table_Rates_Prcp <- ClimaSPSW_new %>% 
  mutate(
    Biome = as.factor(Biome), 
    Biome = fct_relevel(Biome, c("Mediterranean", "Temp_south", "Temp_north", "Boreal"))
  ) %>% 
  group_by(Biome) %>%   
  summarise(
    Prcp12_sd = round(sd(PrcpCC12, na.rm = TRUE), 2),
    Prcp12 = round(mean(PrcpCC12, na.rm = TRUE), 2),
    Prcp23_sd = round(sd(PrcpCC23, na.rm = TRUE), 2),
    Prcp23 = round(mean(PrcpCC23, na.rm = TRUE), 2)
  )
Table_Rates_Prcp

##########################################
###############################

Table_Biome_SPEImin <- ClimaSPSW_new %>% 
  mutate(
    Biome = as.factor(Biome), 
    Biome = fct_relevel(Biome, c("Mediterranean", "Temp_south", "Temp_north", "Boreal"))
  ) %>% 
  group_by(Biome) %>%     
  summarise(
    SPEImin1_sd = round(sd(minSPEI_sp_1, na.rm = TRUE), 2),
    SPEImin1 = round(mean(minSPEI_sp_1, na.rm = TRUE), 2),
    SPEImin1max = round(max(minSPEI_sp_1, na.rm = TRUE), 2),
    SPEImin1min = round(min(minSPEI_sp_1, na.rm = TRUE), 2),
    SPEImin2_sd = round(sd(minSPEI_sp_2, na.rm = TRUE), 2),
    SPEImin2 = round(mean(minSPEI_sp_2, na.rm = TRUE), 2),
    SPEImin2max = round(max(minSPEI_sp_2, na.rm = TRUE), 2),
    SPEImin2min = round(min(minSPEI_sp_2, na.rm = TRUE), 2),
    SPEImin3_sd = round(sd(minSPEI_sp_3, na.rm = TRUE), 2),
    SPEImin3 = round(mean(minSPEI_sp_3, na.rm = TRUE), 2),
    SPEImin3max = round(max(minSPEI_sp_3, na.rm = TRUE), 2),
    SPEImin3min = round(min(minSPEI_sp_3, na.rm = TRUE), 2),
    SPEIminLP_sd = round(sd(minSPEI_sp_ref_GEB, na.rm = TRUE), 2),
    SPEIminLP = round(mean(minSPEI_sp_ref_GEB, na.rm = TRUE), 2),
    SPEIminLPmax = round(max(minSPEI_sp_ref_GEB, na.rm = TRUE), 2),
    SPEIminLPmin = round(min(minSPEI_sp_ref_GEB, na.rm = TRUE), 2)
  )
Table_Biome_SPEImin

Table_Rates_SPEImin <- ClimaSPSW_new %>% 
  mutate(
    Biome = as.factor(Biome), 
    Biome = fct_relevel(Biome, c("Mediterranean", "Temp_south", "Temp_north", "Boreal"))
  ) %>% 
  group_by(Biome) %>%   
  summarise(
    SPEImin12_sd = round(sd(SPEIminCC12, na.rm = TRUE), 2),
    SPEImin12 = round(mean(SPEIminCC12, na.rm = TRUE), 2),
    SPEImin23_sd = round(sd(SPEIminCC23, na.rm = TRUE), 2),
    SPEImin23 = round(mean(SPEIminCC23, na.rm = TRUE), 2)
  )
Table_Rates_SPEImin


##########################################
###############################

Table_Biome_Hw <- ClimaSPSW_new %>% 
  mutate(
    Biome = as.factor(Biome), 
    Biome = fct_relevel(Biome, c("Mediterranean", "Temp_south", "Temp_north", "Boreal"))
  ) %>% 
  group_by(Biome) %>%   
  summarise(
    Hw1_sd = round(sd(nEventsHeatwave_t2_p1, na.rm = TRUE), 2),
    Hw1 = round(mean(nEventsHeatwave_t2_p1, na.rm = TRUE), 2),
    Hw1max = round(max(nEventsHeatwave_t2_p1, na.rm = TRUE), 2),
    Hw1min = round(min(nEventsHeatwave_t2_p1, na.rm = TRUE), 2),
    Hw2_sd = round(sd(nEventsHeatwave_t2_p2, na.rm = TRUE), 2),
    Hw2 = round(mean(nEventsHeatwave_t2_p2, na.rm = TRUE), 2),
    Hw2max = round(max(nEventsHeatwave_t2_p2, na.rm = TRUE), 2),
    Hw2min = round(min(nEventsHeatwave_t2_p2, na.rm = TRUE), 2),
    Hw3_sd = round(sd(nEventsHeatwave_t2_p3, na.rm = TRUE), 2),
    Hw3 = round(mean(nEventsHeatwave_t2_p3, na.rm = TRUE), 2),
    Hw3max = round(max(nEventsHeatwave_t2_p3, na.rm = TRUE), 2),
    Hw3min = round(min(nEventsHeatwave_t2_p3, na.rm = TRUE), 2),
    HwLP_sd = round(sd(nEventsHeatwave_t_ref2_GEB, na.rm = TRUE), 2),
    HwLP = round(mean(nEventsHeatwave_t_ref2_GEB, na.rm = TRUE), 2),
    HwLPmax = round(max(nEventsHeatwave_t_ref2_GEB, na.rm = TRUE), 2),
    HwLPmin = round(min(nEventsHeatwave_t_ref2_GEB, na.rm = TRUE), 2)
  )
Table_Biome_Hw

Table_Rates_Hw <- ClimaSPSW_new %>% 
  mutate(
    Biome = as.factor(Biome), 
    Biome = fct_relevel(Biome, c("Mediterranean", "Temp_south", "Temp_north", "Boreal"))
  ) %>% 
  group_by(Biome) %>% 
  summarise(
    Hw12_sd = round(sd(HwCC12, na.rm = TRUE), 2),
    Hw12 = round(mean(HwCC12, na.rm = TRUE), 2),
    Hw23_sd = round(sd(HwCC23, na.rm = TRUE), 2),
    Hw23 = round(mean(HwCC23, na.rm = TRUE), 2)
  )
Table_Rates_Hw

##########################################
###############################

Table_Biome_BA <- ClimaSPSW_new %>% 
  mutate(
    Biome = as.factor(Biome), 
    Biome = fct_relevel(Biome, c("Mediterranean", "Temp_south", "Temp_north", "Boreal"))
  ) %>% 
  group_by(Biome) %>%   
  summarise(
    ba1_sd = round(sd(ba_ha2, na.rm = TRUE), 2),
    ba1 = round(mean(ba_ha2, na.rm = TRUE), 2),
    ba1max = round(max(ba_ha2, na.rm = TRUE), 2),
    ba1min = round(min(ba_ha2, na.rm = TRUE), 2),
    ba2_sd = round(sd(ba_ha3, na.rm = TRUE), 2),
    ba2 = round(mean(ba_ha3, na.rm = TRUE), 2),
    ba2max = round(max(ba_ha3, na.rm = TRUE), 2),
    ba2min = round(min(ba_ha3, na.rm = TRUE), 2),
    ba3_sd = round(sd(ba_ha4, na.rm = TRUE), 2),
    ba3 = round(mean(ba_ha4, na.rm = TRUE), 2),
    ba3max = round(max(ba_ha4, na.rm = TRUE), 2),
    ba3min = round(min(ba_ha4, na.rm = TRUE), 2)
  )
Table_Biome_BA

##########################################
###############################

Table_Biome_BArates <- ClimaSPSW_new %>% 
  mutate(
    Biome = as.factor(Biome), 
    Biome = fct_relevel(Biome, c("Mediterranean", "Temp_south", "Temp_north", "Boreal"))
  ) %>% 
  group_by(Biome) %>%   
  summarise(
    ba1_sd = round(sd(ratesAB32, na.rm = TRUE), 2),
    ba1 = round(mean(ratesAB32, na.rm = TRUE), 2),
    ba1max = round(max(ratesAB32, na.rm = TRUE), 2),
    ba1min = round(min(ratesAB32, na.rm = TRUE), 2),
    ba2_sd = round(sd(ratesAB43, na.rm = TRUE), 2),
    ba2 = round(mean(ratesAB43, na.rm = TRUE), 2),
    ba2max = round(max(ratesAB43, na.rm = TRUE), 2),
    ba2min = round(min(ratesAB43, na.rm = TRUE), 2)
  )
Table_Biome_BArates


##########################################
###############################

Table_Biome_percProduct <- ClimaSPSW_new %>% 
  mutate(
    Biome = as.factor(Biome), 
    Biome = fct_relevel(Biome, c("Mediterranean", "Temp_south", "Temp_north", "Boreal"))
  ) %>% 
  group_by(Biome) %>%   
  summarise(
    ba1_sd = round(sd(rAB32rel, na.rm = TRUE), 2),
    ba1 = round(mean(rAB32rel, na.rm = TRUE), 2),
    ba1max = round(max(rAB32rel, na.rm = TRUE), 2),
    ba1min = round(min(rAB32rel, na.rm = TRUE), 2),
    ba2_sd = round(sd(rAB43rel, na.rm = TRUE), 2),
    ba2 = round(mean(rAB43rel, na.rm = TRUE), 2),
    ba2max = round(max(rAB43rel, na.rm = TRUE), 2),
    ba2min = round(min(rAB43rel, na.rm = TRUE), 2)
  )
Table_Biome_percProduct


#################Test de wilcoxon

Mediterranean <- ClimaSPSW_new %>% 
  select(Biome, rAB32rel, rAB43rel) %>% 
  filter(Biome == "Mediterranean")
wilcox.test(Mediterranean$rAB32rel, Mediterranean$rAB43rel)

Mediterranean <- ClimaSPSW_new %>% 
  select(Biome, ba_ha2, ba_ha3) %>% 
  filter(Biome == "Mediterranean")
wilcox.test(Mediterranean$ba_ha2, Mediterranean$ba_ha3)


Mediterranean <- ClimaSPSW_new %>% 
  select(Biome, ba_ha3, ba_ha4) %>% 
  filter(Biome == "Mediterranean")
wilcox.test(Mediterranean$ba_ha3, Mediterranean$ba_ha4)


Mediterranean <- ClimaSPSW_new %>% 
  select(Biome, ba_ha2, ba_ha4) %>% 
  filter(Biome == "Mediterranean")
wilcox.test(Mediterranean$ba_ha2, Mediterranean$ba_ha4)


############################################################################



Temp_south <- ClimaSPSW_new %>% 
  select(Biome, rAB32rel, rAB43rel) %>% 
  filter(Biome == "Temp_south")
wilcox.test(Temp_south$rAB32rel, Temp_south$rAB43rel)


Temp_south <- ClimaSPSW_new %>% 
  select(Biome, ba_ha2, ba_ha3)%>% 
  filter(Biome == "Temp_south")

wilcox.test(Temp_south$ba_ha2, Temp_south$ba_ha3)

Temp_south <- ClimaSPSW_new %>% 
  select(Biome, ba_ha3, ba_ha4) %>% 
  filter(Biome == "Temp_south")
wilcox.test(Temp_south$ba_ha3, Temp_south$ba_ha4)

Temp_south <- ClimaSPSW_new %>% 
  select(Biome, ba_ha2, ba_ha4) %>% 
  filter(Biome == "Temp_south")
wilcox.test(Temp_south$ba_ha2, Temp_south$ba_ha4)


############################################################################


Temp_north <- ClimaSPSW_new %>% 
  select(Biome, rAB32rel, rAB43rel) %>% 
  filter(Biome == "Temp_north")
wilcox.test(Temp_north$rAB32rel, Temp_north$rAB43rel)


Temp_north <- ClimaSPSW_new %>% 
  select(Biome, ba_ha2, ba_ha3)%>% 
  filter(Biome == "Temp_north")

wilcox.test(Temp_north$ba_ha2, Temp_north$ba_ha3)

Temp_north <- ClimaSPSW_new %>% 
  select(Biome, ba_ha3, ba_ha4) %>% 
  filter(Biome == "Temp_north")
wilcox.test(Temp_north$ba_ha3, Temp_north$ba_ha4)

Temp_north <- ClimaSPSW_new %>% 
  select(Biome, ba_ha2, ba_ha4) %>% 
  filter(Biome == "Temp_north")
wilcox.test(Temp_north$ba_ha2, Temp_north$ba_ha4)



#################################################################################


Boreal <- ClimaSPSW_new %>% 
  select(Biome, rAB32rel, rAB43rel) %>% 
  filter(Biome == "Boreal")

wilcox.test(Boreal$rAB32rel, Boreal$rAB43rel)



Boreal <- ClimaSPSW_new %>% 
  select(Biome, ba_ha2, ba_ha3)%>% 
  filter(Biome == "Boreal")

wilcox.test(Boreal$ba_ha2, Boreal$ba_ha3)

Boreal <- ClimaSPSW_new %>% 
  select(Biome, ba_ha3, ba_ha4) %>% 
  filter(Biome == "Boreal")
wilcox.test(Boreal$ba_ha3, Boreal$ba_ha4)

Boreal <- ClimaSPSW_new %>% 
  select(Biome, ba_ha2, ba_ha4) %>% 
  filter(Biome == "Boreal")
wilcox.test(Boreal$ba_ha2, Boreal$ba_ha4)





##########################################
###############################

Table_Biome_BA <- ClimaSPSW_new %>% 
  mutate(
    Biome = as.factor(Biome), 
    Biome = fct_relevel(Biome, c("Mediterranean", "Temp_south", "Temp_north", "Boreal"))
  ) %>% 
  group_by(Biome) %>%   
  summarise(
    ba1_sd = round(sd(ba_ha2, na.rm = TRUE), 2),
    ba1 = round(mean(ba_ha2, na.rm = TRUE), 2),
    ba1max = round(max(ba_ha2, na.rm = TRUE), 2),
    ba1min = round(min(ba_ha2, na.rm = TRUE), 2),
    ba2_sd = round(sd(ba_ha3, na.rm = TRUE), 2),
    ba2 = round(mean(ba_ha3, na.rm = TRUE), 2),
    ba2max = round(max(ba_ha3, na.rm = TRUE), 2),
    ba2min = round(min(ba_ha3, na.rm = TRUE), 2),
    ba3_sd = round(sd(ba_ha4, na.rm = TRUE), 2),
    ba3 = round(mean(ba_ha4, na.rm = TRUE), 2),
    ba3max = round(max(ba_ha4, na.rm = TRUE), 2),
    ba3min = round(min(ba_ha4, na.rm = TRUE), 2)
  )
Table_Biome_BA

##########################################
###############################

Table_Biome_BArates <- ClimaSPSW_new %>% 
  mutate(
    Biome = as.factor(Biome), 
    Biome = fct_relevel(Biome, c("Mediterranean", "Temp_south", "Temp_north", "Boreal"))
  ) %>% 
  group_by(Biome) %>%   
  summarise(
    ba1_sd = round(sd(ratesAB32, na.rm = TRUE), 2),
    ba1 = round(mean(ratesAB32, na.rm = TRUE), 2),
    ba1max = round(max(ratesAB32, na.rm = TRUE), 2),
    ba1min = round(min(ratesAB32, na.rm = TRUE), 2),
    ba2_sd = round(sd(ratesAB43, na.rm = TRUE), 2),
    ba2 = round(mean(ratesAB43, na.rm = TRUE), 2),
    ba2max = round(max(ratesAB43, na.rm = TRUE), 2),
    ba2min = round(min(ratesAB43, na.rm = TRUE), 2)
  )

Table_Biome_BArates


##########################################
###############################

Table_Biome_percProduct <- ClimaSPSW_new %>% 
  mutate(
    Biome = as.factor(Biome), 
    Biome = fct_relevel(Biome, c("Mediterranean", "Temp_south", "Temp_north", "Boreal"))
  ) %>% 
  group_by(Biome) %>%   
  summarise(
    ba1_sd = round(sd(rAB32rel, na.rm = TRUE), 2),
    ba1 = round(mean(rAB32rel, na.rm = TRUE), 2),
    ba1max = round(max(rAB32rel, na.rm = TRUE), 2),
    ba1min = round(min(rAB32rel, na.rm = TRUE), 2),
    ba2_sd = round(sd(rAB43rel, na.rm = TRUE), 2),
    ba2 = round(mean(rAB43rel, na.rm = TRUE), 2),
    ba2max = round(max(rAB43rel, na.rm = TRUE), 2),
    ba2min = round(min(rAB43rel, na.rm = TRUE), 2)
  ) 
Table_Biome_percProduct

