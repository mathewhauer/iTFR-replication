## @knitr plot-historical-hmd-estimates

# Data are produced from 
results <- read_csv('../R/DATA-PROCESSED/HMDHFD-allbTFR_06172017.csv') %>%
  filter(Code %in% c("NLD", "SWE", "FRATNP", "ITA")) %>%
  dplyr::select(-lagTFR)

historical <- read_csv("../R/DATA-PROCESSED/001-LoadCleanHFDHMDData.csv") %>%
  dplyr::select(Code, Year, TFR, lagTFR)

# Getting data from the HFC
z <- rbind(read_csv("https://www.fertilitydata.org/data/SWE/SWE_TFRMAB_TOT.txt"),
           read_csv("https://www.fertilitydata.org/data/FRA/FRA_TFRMAB_TOT.txt"),
           read_csv("https://www.fertilitydata.org/data/ITA/ITA_TFRMAB_TOT.txt"),
           read_csv("https://www.fertilitydata.org/data/NLD/NLD_TFRMAB_TOT.txt")) %>%
  filter(RefCode %in% c("SWE_02", "FRA_03", "ITA_13", "NLD_02")) %>%
  mutate(Country = if_else(Country == "FRA", "FRATNP", Country)) %>%
  group_by(Country, AgeDef) %>%
  mutate(length = Year2 - Year1 +1,
         lagTFR = if_else(length == 1, mean_lag5(TFR), TFR )) %>%
  dplyr::select(Code = Country, Year = Year2, HFC = lagTFR) 

results2 <- results %>% 
  left_join(., historical) %>%
  mutate(country = case_when(
    Code == "SWE" ~ "Sweden",
    Code == "NLD" ~ "Netherlands",
    Code == "FRATNP" ~ "France",
    Code == "ITA" ~ "Italy"
  )) %>%
  left_join(., z)


ggplot(data = results2) +
  geom_ribbon(aes(x=Year, ymin=Q10, ymax=Q90), fill = "gray", alpha=0.6) +
  geom_line(aes(x=Year, y = post_mean, linetype = "bTFR")) +
  geom_point(aes(x=Year, y = lagTFR, shape = "Observed", color = "Observed"), alpha= 0.3, size =2) +
  geom_point(aes(x=Year, y = HFC, shape = "HFC", color = "HFC"), alpha= 0.5, size =2) +
  scale_y_continuous(limits = c(1,7), expand = c(0, 0)) +
  theme(text=element_text(face='bold', size = 8),
        legend.text=element_text(size=5))+
  theme(legend.position=c(0.22,0.17),
        legend.key.size = unit(0.25, "cm")) +
  scale_shape_manual("", 
                     values = c(42,1)) +
  scale_color_manual("",
                     values = c("red", "black")) +
  scale_linetype_manual("", values = "solid") +
  theme_bw() +
  theme(legend.position="bottom") +
  labs(x='Year', y='TFR') +
  scale_x_continuous(breaks = c(1750,1800,1850,1900,1950,2000)) +
  facet_wrap(~country) 