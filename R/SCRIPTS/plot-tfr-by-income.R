## @knitr plot-tfr-by-income

#  Output from Data ferret. Post-processing here.
inctest <- read_csv("DATA-RAW/INCOME-income_agedetail.csv")

children <- inctest %>%
  filter(SEX=="TOTAL") %>%
  dplyr::select(INCOME, a0004) 

women <- inctest %>%
  filter(SEX == "Female") %>%
  dplyr::select(INCOME, a1519:a4549)

income <- left_join(children, women) %>%
  filter(!INCOME %in% c("Total RECODE2", "Between -59998 and -1", "Not Elsewhere Classified (nec.)"))  %>%
  dplyr::mutate(W = (a1519 + a2024 + a2529 + a3034 + a3539 + a4044 + a4549), # estimating total number of Women
         CWR = (a0004) / W, # estimating Child-woman ratio
         p2534 = (a2529 + a3034) / W, # estimating the proportion of women aged 25-34
         xTFR = (coef(reg)[1] + coef(reg)[2] * p2534) * CWR,
         Income.Bracket = row_number())
# Plotting panel C

incomelabels <- c("2-24", "25-49", "50-74", "75-99", "100-124", "125-149", "150-199", "200-249", "250-499", "500-999", "1000+")

f3c <- ggplot() +
  geom_line(data=income, aes(x=Income.Bracket, y=xTFR)) +
  ylim(0,+3) +
  theme_bw() +
  labs(x="Income (in 000s)",
       y = "TFR")+
  theme(text=element_text(face='bold'), axis.text.x = element_text(angle=45, hjust=1)) +
  scale_x_discrete(limits=incomelabels)

f3c