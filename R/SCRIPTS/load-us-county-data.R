## @knitr load-us-county-data

#   Importing the Natality files from CDC Wonder for US Counties, 2007-2010   ###
tfr_county0710 <- read_tsv("../R/DATA-RAW/USCounties/USCOUNTY-Natality, 2007-2010.txt") %>%
  dplyr::select(County, CountyCode, AgeofMother9Code, Year, Births, FemalePopulation) %>% # selecting specific values
  dplyr::mutate(YearCode = as.numeric(Year)) # Year is sometimes weird, this converts it to numeric

#   Importing the Natality file from CDC Wonder for US Counties, 2006-2010   ###
tfr_county06 <- read_tsv("../R/DATA-RAW/USCounties/USCOUNTY-Natality, 2003-2006.txt")%>%
  dplyr::select(County, CountyCode, AgeofMother9Code, Year, Births, FemalePopulation) %>%
  dplyr::mutate(YearCode = as.numeric(Year)*1)

#   Merging the two Natality files together   ###
tfr_county <- full_join(tfr_county06, tfr_county0710) %>%
  dplyr::filter(!is.na(County))  %>% # dropping all of the extra stuff at the bottom of the CDC's output
  group_by(County, CountyCode, Year) %>%
  dplyr::mutate(fertrat = (Births / as.numeric(FemalePopulation))*5) %>% # Calculating ASFRs
  dplyr::summarize(TFR = sum(na.omit(fertrat))) # Calculating the TFR

#   Calculating the lagged TFR values
tfr_county$lagTFR = NA

for (this.county in unique(tfr_county$CountyCode)) {
  ix = which(tfr_county$CountyCode == this.county)
  tfr_county$lagTFR[ix] = mean_lag5( tfr_county$TFR[ix])  
}

# Downloading the IHME data on e0 for US counties.
# need_county_e0 = FALSE   # FALSE once successfully downloaded (02 Mar 2019)
# if (need_county_e0) {
#   download.file("http://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_USA_COUNTY_LE_MORTALITY_RISK_1980_2014_NATIONAL_XLSX.zip",
#                 "../R/DATA-RAW/USCounties/USCOUNTY-ihme0.zip")
#   unzip(zipfile='../R/DATA-RAW/USCounties/USCOUNTY-ihme0.zip', exdir = "DATA-RAW")
# }

# Processing the IHME data - edited 01 Mar 2019 (file changes at healthdata.org?)
countye0 <- read_excel("../R/DATA-RAW/USCounties/IHME_USA_COUNTY_LE_MORTALITY_RISK_1980_2014_NATIONAL_Y2017M05D08.xlsx", 
                       sheet = 1, skip=1, col_names =TRUE) %>%
  dplyr::select(FIPS,ex=9) %>%    # FIPS code and 2010 e0 values like 78.82 (78.81, 78.84)
  separate(ex, c("ex", "drop"), sep = " " ) %>%
  dplyr::mutate(CountyCode = str_pad(trimws(FIPS), 5, pad = "0"),
                ex = as.numeric(ex)) %>%
  dplyr::select(-drop)

#   Importing the Census 2010 information for US Counties
census_county <- read_csv("../R/DATA-RAW/USCounties/USCENSUS-county2010.csv") %>%
  dplyr::mutate(W         = (W1517 + W1819 + W20 + W21 + W2224 + W2529 + W3034 + W3539 + W4044 + W4549), # estimating total number of Women
               CWR        = (m0004 + W0004) / W, # estimating Child-woman ratio
               p2534      = (W2529 + W3034) / W, # estimating the proportion of women aged 25-34
               xTFR       = (coef(reg)[1] + coef(reg)[2] * p2534) * CWR, # applying the regression coefficients from the HMD/HFD to US counties
               iTFR       = CWR*7,
               CountyCode = sprintf("%05d", KEY),
               Year       = 2010) %>%
  left_join(., tfr_county, by = c("CountyCode", "Year")) %>%
  left_join(., countye0, by = "CountyCode") %>%
  dplyr::mutate(e0 = plyr::round_any(ex, 10, floor)) %>%
  left_join(., relecoefficients) %>%
  dplyr::mutate(eadj  = ex-e0,
                eadja = alpha + eadj*deltaa,
                eadjb = beta + eadj*deltab,
                rele  = (1+1.05)*(eadja+(eadjb*(CWR)))) %>%
  dplyr::select(CountyCode, xTFR, iTFR, rele, lagTFR, Year)

#   Estimating the 50th percentile error rate from the xTFR method
countyeval <- census_county %>%
  dplyr::mutate(num = if_else(lagTFR >0,1,0)) %>%
  dplyr::summarize(tot = sum(num, na.rm = T) ,
                   xlagTFR50     = quantile(abs(xTFR / lagTFR -1), 0.5, na.rm=T)*100,
                   xlagTFR90     = quantile(abs(xTFR / lagTFR -1), 0.9, na.rm=T)*100,
                   xlagTFR_abs50 = quantile(abs(xTFR - lagTFR), 0.5, na.rm=T),
                   xlagTFR_abs90 = quantile(abs(xTFR - lagTFR), 0.9, na.rm=T),
                   ilagTFR50     = quantile(abs(iTFR / lagTFR -1), 0.5, na.rm=T)*100,
                   ilagTFR90     = quantile(abs(iTFR / lagTFR -1), 0.9, na.rm=T)*100,
                   ilagTFR_abs50 = quantile(abs(iTFR - lagTFR), 0.5, na.rm=T),
                   ilagTFR_abs90 = quantile(abs(iTFR - lagTFR), 0.9, na.rm=T),
                   rlagTFR50     = quantile(abs(rele / lagTFR -1), 0.5, na.rm=T)*100,
                   rlagTFR90     = quantile(abs(rele / lagTFR -1), 0.9, na.rm=T)*100,
                   rlagTFR_abs50 = quantile(abs(rele - lagTFR), 0.5, na.rm=T),
                   rlagTFR_abs90 = quantile(abs(rele - lagTFR), 0.9, na.rm=T))

# Selecting the lagged TFR from the 5-year time series for merging with the shapefile.
selectcnties <- census_county %>%
  filter(!is.na(lagTFR))


