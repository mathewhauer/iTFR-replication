## @knitr merge-results

# Load the bTFR results.
bayesTFR <- read_csv("../R/DATA-PROCESSED/HFD-bayesitfr.csv") %>%
  dplyr::select(Code, Year, iTFR, lagTFR, bayesTFR=post_mean)

bayesTFR_dhs <- read_csv("../R/DATA-PROCESSED/IPUMS-bayesitfr.csv") %>%
  mutate(Year = as.character(Year)) %>%
  dplyr::select(Code, Year, bTFR=post_mean)


earliest.year = 1891

tmp = read.csv("../R/DATA-PROCESSED/001-LoadCleanHFDHMDData.csv", as.is=TRUE)  %>%
  filter(Year >= earliest.year, !is.na(iTFR)) %>%   
  mutate( CWR    = C/W,
          mult   = lagTFR/CWR,
          p15    = W15/W,
          p20    = W20/W,
          p25    = W25/W,
          p30    = W30/W,
          p35    = W35/W,
          p40    = W40/W,
          p45    = W45/W,
          decade = as.factor( 10 * floor(Year/10)) ) %>%
  filter(!is.na(mult))

within10percent <- mean( abs((tmp$mult-7)/7) < .10)

reg  = lm( mult ~ p2534, data=tmp) 
reg2 = lm( mult ~ -1+p15+p20+p25+p30+p35+p40+p45, data=tmp) 


# Coefficients from Rele 1974. We did not use this analysis in the final paper.
relecoefficients <- tribble(
  ~"e0",  ~"alpha",  ~"deltaa",  ~"beta",  ~"deltab",
  20L,  0.0547, -0.00263,  4.768, -0.04387,
  30L,  0.0284, -0.00155, 4.3293, -0.02676,
  40L,  0.0129, -0.00188, 4.0617, -0.02028,
  50L, -0.0059, -0.00123, 3.8589, -0.01961,
  60L, -0.0182, -0.00127, 3.6628, -0.01799,
  70L, -0.0309,      0, 3.4829,      0,
  80L, -0.0309,      0, 3.4829,      0,
  90L,-0.0309,      0, 3.4829,      0
)


# Calculating the xTFR based on the regression components
xitfr <- tmp %>%
  mutate( "xTFR+" = (coef(reg)[1] + coef(reg)[2] * p2534) * ((C/(1-qx_const*qx))/W),
          "iTFR+" = 7 * ((C/(1-qx_const*qx))/W),
          iTFR = 7* C/W,
          xTFR = (coef(reg)[1] + coef(reg)[2] * p2534) * (C/W),
          e0 = plyr::round_any(ex, 10, floor)) %>%
  left_join(., relecoefficients) %>%
  mutate(eadj = ex-e0,
         eadja = alpha + eadj*deltaa,
         eadjb = beta + eadj*deltab,
         rele = (1+1.05)*(eadja+(eadjb*(C/W)))) %>%
  na.omit

xtfr_alpha = round(coef(reg)[1],2)
xtfr_beta  = round(coef(reg)[2],2)

beta = coef(reg2)

## Getting the DHS data.

# require(RJSONIO)
# # Import DHS Indicator data for TFR for each survey
# tfr        <- fromJSON("http://api.dhsprogram.com/rest/dhs/data/FE_FRTR_W_TFR?perpage=1000")
# under5mort <- fromJSON("http://api.dhsprogram.com/rest/dhs/data/CM_ECMR_C_U5M?perpage=1000")
# f1549      <- fromJSON("http://api.dhsprogram.com/rest/dhs/data/FE_FRTY_W_NPG?perpage=1000")
# c00045     <- fromJSON("http://api.dhsprogram.com/rest/dhs/data/ML_FEVR_C_NUM?perpage=1000")
# 
# # Unlist the JSON file entries
# under5mort <- lapply(under5mort$Data, function(x) { unlist(x) })
# tfr        <- lapply(tfr$Data, function(x) { unlist(x) })
# f1549      <- lapply(f1549$Data, function(x) { unlist(x) })
# c00045     <- lapply(c00045$Data, function(x) { unlist(x) })
# 
# # Convert JSON input to a data frame
# under5mort <- as.data.frame(do.call("rbind", under5mort),stringsAsFactors=FALSE)
# under5mort <- under5mort %>%
#   filter(IsPreferred == 1) %>%# This gets just the 5-years preceding the survey.
#   dplyr::select(SurveyId, u5mort = Value, SurveyYearLabel, SurveyYear, DHS_CountryCode, CountryName)
# 
# tfr <- as.data.frame(do.call("rbind", tfr),stringsAsFactors=FALSE)%>%
#   dplyr::select(SurveyId, TFR = Value, SurveyYearLabel, SurveyYear, DHS_CountryCode, CountryName)
# 
# f1549 <- as.data.frame(do.call("rbind", f1549),stringsAsFactors=FALSE) %>%
#   dplyr::select(SurveyId, W = Value, SurveyYearLabel, SurveyYear, DHS_CountryCode, CountryName)
# 
# c00045 <- as.data.frame(do.call("rbind", c00045),stringsAsFactors=FALSE) %>%
#   dplyr::select(SurveyId, C = Value, SurveyYearLabel, SurveyYear, DHS_CountryCode, CountryName)
# 
# write_csv(tfr, "../R/DATA-PROCESSED/DHS_TFR.csv")
# write_csv(f1549, "../R/DATA-PROCESSED/DHS_f1549.csv")
# write_csv(c00045, "../R/DATA-PROCESSED/DHS_c00045.csv")
# write_csv(under5mort, "../R/DATA-PROCESSED/DHS_under5mort.csv")

# DHS Data reimport
tfr <- read_csv("../R/DATA-PROCESSED/DHS_TFR.csv")
f1549 <- read_csv("../R/DATA-PROCESSED/DHS_f1549.csv")
c00045 <- read_csv("../R/DATA-PROCESSED/DHS_c00045.csv")
under5mort <- read_csv("../R/DATA-PROCESSED/DHS_under5mort.csv")



# Unzip the # of women from
gunzip("../R/DATA-RAW/DHS/idhs_00020.dat.gz", overwrite = TRUE, remove = FALSE)

ddi <- ipumsr::read_ipums_ddi("../R/DATA-RAW/DHS/idhs_00020.xml")

women <- ipumsr::read_ipums_micro(ddi) %>%
  group_by(COUNTRY, YEAR, AGE5YEAR) %>%
  dplyr::summarise(women = sum(PERWEIGHT)) %>%
  spread(AGE5YEAR, women) %>%
  dplyr::select(COUNTRY, Year = YEAR, W15 = `20`, W20 = `30`, W25 = `40`, W30 = `50`, W35 = `60`, W40 = `70`, W45 = `80`) %>%
  ungroup() %>%
  mutate(W2 = W15 + W20 + W25 + W30 + W35 + W40 + W45,
         Year = as.character(Year),
         Name = case_when(
           COUNTRY == '004' ~ 'Afghanistan',
           COUNTRY == '050' ~ 'Bangladesh',
           COUNTRY == '104' ~ 'Myanmar',
           COUNTRY == '108' ~ 'Burundi',
           COUNTRY == '116' ~ 'Cambodia',
           COUNTRY == '120' ~ 'Cameroon',
           COUNTRY == '148' ~ 'Chad',
           COUNTRY == '180' ~ 'Congo Democratic Republic',
           COUNTRY == '204' ~ 'Benin',
           COUNTRY == '231' ~ 'Ethiopia',
           COUNTRY == '288' ~ 'Ghana',
           COUNTRY == '320' ~ 'Guatemala',
           COUNTRY == '324' ~ 'Guinea',
           COUNTRY == '356' ~ 'India',
           COUNTRY == '384' ~ 'Cote dIvoire',
           COUNTRY == '404' ~ 'Kenya',
           COUNTRY == '426' ~ 'Lesotho',
           COUNTRY == '450' ~ 'Madagascar',
           COUNTRY == '454' ~ 'Malawi',
           COUNTRY == '466' ~ 'Mali',
           COUNTRY == '504' ~ 'Morocco',
           COUNTRY == '508' ~ 'Mozambique',
           COUNTRY == '516' ~ 'Namibia',
           COUNTRY == '524' ~ 'Nepal',
           COUNTRY == '562' ~ 'Niger',
           COUNTRY == '586' ~ 'Pakistan',
           COUNTRY == '566' ~ 'Nigeria',
           COUNTRY == '604' ~ 'Peru',
           COUNTRY == '646' ~ 'Rwanda',
           COUNTRY == '686' ~ 'Senegal',
           COUNTRY == '710' ~ 'South Africa',
           COUNTRY == '716' ~ 'Zimbabwe',
           COUNTRY == '729' ~ 'Sudan',
           COUNTRY == '788' ~ 'Tunisia',
           COUNTRY == '792' ~ 'Turkey',
           COUNTRY == '800' ~ 'Uganda',
           COUNTRY == '818' ~ 'Egypt',
           COUNTRY == '834' ~ 'Tanzania',
           COUNTRY == '854' ~ 'Burkina Faso',
           COUNTRY == '887' ~ 'Yemen',
           COUNTRY == '894' ~ 'Zambia',
           COUNTRY == '1' ~ 'Model'
         ),
         Code = case_when(
           COUNTRY == '004' ~ 'AF',
           COUNTRY == '050' ~ 'BD',
           COUNTRY == '104' ~ 'MM',
           COUNTRY == '108' ~ 'BU',
           COUNTRY == '116' ~ 'KH',
           COUNTRY == '120' ~ 'CM',
           COUNTRY == '148' ~ 'TD',
           COUNTRY == '180' ~ 'CD',
           COUNTRY == '204' ~ 'BJ',
           COUNTRY == '231' ~ 'ET',
           COUNTRY == '288' ~ 'GH',
           COUNTRY == '320' ~ 'GU',
           COUNTRY == '324' ~ 'GN',
           COUNTRY == '356' ~ 'IA',
           COUNTRY == '384' ~ 'CI',
           COUNTRY == '404' ~ 'KE',
           COUNTRY == '426' ~ 'LS',
           COUNTRY == '450' ~ 'MD',
           COUNTRY == '454' ~ 'MW',
           COUNTRY == '466' ~ 'ML',
           COUNTRY == '504' ~ 'MA',
           COUNTRY == '508' ~ 'MZ',
           COUNTRY == '516' ~ 'NM',
           COUNTRY == '524' ~ 'NP',
           COUNTRY == '562' ~ 'NI',
           COUNTRY == '586' ~ 'PK',
           COUNTRY == '566' ~ 'NG',
           COUNTRY == '604' ~ 'PE',
           COUNTRY == '646' ~ 'RW',
           COUNTRY == '686' ~ 'SN',
           COUNTRY == '710' ~ 'ZA',
           COUNTRY == '716' ~ 'ZW',
           COUNTRY == '729' ~ 'SD',
           COUNTRY == '788' ~ 'TN',
           COUNTRY == '792' ~ 'TR',
           COUNTRY == '800' ~ 'UG',
           COUNTRY == '818' ~ 'EG',
           COUNTRY == '834' ~ 'TZ',
           COUNTRY == '854' ~ 'BF',
           COUNTRY == '887' ~ 'YE',
           COUNTRY == '894' ~ 'ZM'
           
         ))

dat <- left_join(c00045, under5mort) %>%
  left_join(., tfr) %>%
  left_join(., f1549) %>%
  mutate(iTFR = as.numeric(C) / as.numeric(W) * 7,
         TFR = as.numeric(TFR),
         source  = "DHS",
         xTFR = 0,
         W = as.numeric(W),
         C = as.numeric(C),
         q5 = as.numeric(u5mort)/1000,
         `iTFR+` = 7 * ((C/(1-qx_const*q5))/W),
         `xTFR+` = NA,
         rele = NA,
         SurveyYear = as.character(SurveyYear)) %>%
  dplyr::select(Code = DHS_CountryCode, Year = SurveyYear, iTFR, xTFR, C, lagTFR=TFR, q5, 'iTFR+', 'xTFR+', rele, source,W) %>%
  left_join(., women) %>%
  mutate(p2534 = (W25 + W30)/ W,
    xTFR = (coef(reg)[1] + coef(reg)[2] * p2534) * (C/W),
    `xTFR+` = (coef(reg)[1] + coef(reg)[2] * p2534) * ((C/(1-qx_const*q5))/W)) %>%
  dplyr::select(Code, Year, iTFR, xTFR, C, W, lagTFR, q5, 'iTFR+', 'xTFR+', rele, source) 

dat <- left_join(dat, bayesTFR_dhs)


# Joining the iTFR, xTFR, and BayesTFR together
big = inner_join(xitfr, bayesTFR, c("Code", "Year")) %>%
  dplyr::select(Code, Year, iTFR=iTFR.x, xTFR, C, W, lagTFR=lagTFR.x, q5=qx, bTFR=bayesTFR, 'iTFR+', 'xTFR+', rele) %>%
  mutate(source = "HMD/HFD")

big <- rbind(big, dat) %>%
  mutate(Year = as.numeric(Year))





# Summary Statistics for each method
itfrape50 <- quantile(abs((big$iTFR-big$lagTFR)/big$lagTFR)*100, 0.5, na.rm=T)
itfrape90 <- quantile(abs((big$iTFR-big$lagTFR)/big$lagTFR)*100, 0.9, na.rm=T)
xtfrape50 <- quantile(abs((big$xTFR-big$lagTFR)/big$lagTFR)*100, 0.5, na.rm=T)
xtfrape90 <- quantile(abs((big$xTFR-big$lagTFR)/big$lagTFR)*100, 0.9, na.rm=T)
releape50 <- quantile(abs((big$rele-big$lagTFR)/big$lagTFR)*100, 0.5, na.rm=T)
releape90 <- quantile(abs((big$rele-big$lagTFR)/big$lagTFR)*100, 0.9, na.rm=T)

itfr_hatape50 <- quantile(abs((big$"iTFR+"-big$lagTFR)/big$lagTFR)*100, 0.5, na.rm=T)
itfr_hatape90 <- quantile(abs((big$"iTFR+"-big$lagTFR)/big$lagTFR)*100, 0.9, na.rm=T)
xtfr_hatape50 <- quantile(abs((big$"xTFR+"-big$lagTFR)/big$lagTFR)*100, 0.5, na.rm=T)
xtfr_hatape90 <- quantile(abs((big$"xTFR+"-big$lagTFR)/big$lagTFR)*100, 0.9, na.rm=T)

bayestfrape50 <- quantile(abs((big$bTFR-big$lagTFR)/big$lagTFR)*100, 0.5, na.rm=T)
bayestfrape90 <- quantile(abs((big$bTFR-big$lagTFR)/big$lagTFR)*100, 0.9, na.rm=T)

itfrabs50 <- quantile(abs((big$iTFR-big$lagTFR)), 0.5, na.rm=T)
itfrabs90 <- quantile(abs((big$iTFR-big$lagTFR)), 0.9, na.rm=T)
xtfrabs50 <- quantile(abs((big$xTFR-big$lagTFR)), 0.5, na.rm=T)
xtfrabs90 <- quantile(abs((big$xTFR-big$lagTFR)), 0.9, na.rm=T)

itfr_hatabs50 <- quantile(abs((big$"iTFR+"-big$lagTFR)), 0.5, na.rm=T)
itfr_hatabs90 <- quantile(abs((big$"iTFR+"-big$lagTFR)), 0.9, na.rm=T)
xtfr_hatabs50 <- quantile(abs((big$"xTFR+"-big$lagTFR)), 0.5, na.rm=T)
xtfr_hatabs90 <- quantile(abs((big$"xTFR+"-big$lagTFR)), 0.9, na.rm=T)

bayestfrabs50 <- quantile(abs((big$bTFR-big$lagTFR)), 0.5, na.rm=T)
bayestfrabs90 <- quantile(abs((big$bTFR-big$lagTFR)), 0.9, na.rm=T)



