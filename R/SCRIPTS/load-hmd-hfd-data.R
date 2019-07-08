## @knitr load-hmd-hfd-data

library(HMDHFDplus)
library(tidyverse)
library(ggplot2)
library(grid)
library(gridExtra)
library(openxlsx) # Microsoft Excel Files

# a function to calculate five-year averages of TFR
# given vector X of length L, returns
#   NA NA NA NA,  mean( X[1:5]), mean(X[2:6]), ..., mean(X[(L-4):L])
mean_lag5 = function(X) {
  if (length(X) > 4) {
    Z = sapply( 5:length(X),  function(i) mean(X[i - 0:4]))
    Z = c( rep(NA,4), Z)
  } else {
    Z = rep(NA, length(X))
  }  
  return(Z)
}

# set up data from Human FERTILITY database files
# remove GBR_NP and DEUTNP observations, because those
# countries are subdivided and we don't want to double count

discard = c('GBR_NP', 'DEUTE', 'DEUTW')

HFD_expos = readHFD('../R/DATA-RAW/HMDHFD/HFD-exposRR.txt') %>%
  filter(Age %in% 15:49, !(Code %in% discard)) %>%
  mutate(AgeGroup = 5 * floor(Age/5)) %>%
  group_by(Code,Year,AgeGroup) %>%
  dplyr::summarize(W = sum(Exposure)) %>%
  spread(key=AgeGroup, value=W, sep='_') %>%
  dplyr::select(Code,Year,W=starts_with('AgeGroup')) %>%
  ungroup()

HFD_tfr = readHFD('../R/DATA-RAW/HMDHFD/HFD-tfrRR.txt') %>%
  filter(!(Code %in% discard)) %>%
  dplyr::select(-TFR40)


HFD_births = readHFD('../R/DATA-RAW/HMDHFD/HFD-totbirthsRR.txt') %>%
  filter(!(Code %in% discard)) %>%
  dplyr::select(Births=Total, Code=Code, Year=Year)

HMD_population = data.frame()
lt_both = data.frame()

# set up data from Human MORTALITY database files
for (this.country in unique(HFD_expos$Code)) {
  filename = paste0(this.country,'.Population5.txt')
  unzip(zipfile='../R/DATA-RAW/HMDHFD/HMD_population.zip', 
        files=paste0('Population5/',filename),
        junkpaths=TRUE)
  
  tmp = read.table(filename, skip=2, stringsAsFactors = FALSE,
                   header=TRUE)
  
  # Some countries have YYYY+ and YYYY- versions of Year, for yrs in
  # which territory changed (AK and HI in 1959 US, NF in 1949 Canada, etc)
  # In all cases we discard the "-" versions, and remove the "+"
  
  minus_ix = grep('-', tmp$Year)
  if (length(minus_ix) > 0)  tmp = tmp[-minus_ix, ]
  
  plus_ix = grep('+', tmp$Year)
  if (length(plus_ix) > 0)  tmp$Year[plus_ix] = substr(tmp$Year,1,4)
  
  
  
  tmp = tmp %>%
    mutate(Code = this.country,
           Year = as.integer(Year))
  
  tmp$Age = c(0,1,seq(5,110,5))   # replace with numeric
  
  tmp = tmp %>%
    group_by(Code, Year) %>%
    summarize( C = sum(Total[Age %in% 0:1]),
               W15 = Female[Age == 15],
               W20 = Female[Age == 20],
               W25 = Female[Age == 25],
               W30 = Female[Age == 30],
               W35 = Female[Age == 35],
               W40 = Female[Age == 40],
               W45 = Female[Age == 45],
               W   = sum(Female[Age %in% seq(15,45,5)]),
               p2534 = (W25+W30)/W,
               iTFR = 7*C/W) %>%
    ungroup()
  
  HMD_population = rbind(HMD_population, tmp)
  
  file.remove(filename)
  
} # for this.country

# Merge
for (this.country in unique(HFD_expos$Code)) {
  filename = paste0(this.country,'.bltper_1x1.txt')
  unzip(zipfile='../R/DATA-RAW/HMDHFD/lt_both.zip', 
        files=paste0('bltper_1x1/',filename),
        junkpaths=TRUE)
  
  tmp = read.table(filename, skip=2, stringsAsFactors = FALSE,
                   header=TRUE)
  
  # Some countries have YYYY+ and YYYY- versions of Year, for yrs in
  # which territory changed (AK and HI in 1959 US, NF in 1949 Canada, etc)
  # In all cases we discard the "-" versions, and remove the "+"
  
  minus_ix = grep('-', tmp$Year)
  if (length(minus_ix) > 0)  tmp = tmp[-minus_ix, ]
  
  plus_ix = grep('+', tmp$Year)
  if (length(plus_ix) > 0)  tmp$Year[plus_ix] = substr(tmp$Year,1,4)
  
  tmp = tmp %>%
    mutate(Code = this.country,
           Year = as.integer(Year))

  tmp = tmp %>%
    filter(Age==0)
  
  lt_both = rbind(lt_both, tmp)
  
  file.remove(filename)
  
} # for this.country

earliest.year = 1891

tmp1 = left_join(HFD_tfr, 
                 dplyr::select(HMD_population, Code, Year, C, contains('W'), iTFR, p2534), 
                 by=c('Code','Year'))

tmp = left_join(tmp1, 
                dplyr::select(lt_both, Code, Year, qx, ex), 
                by=c('Code','Year'))

## calculate the lagged TFR values for each country
tmp$lagTFR = NA

for (this.country in unique(tmp$Code)) {
  ix = which(tmp$Code == this.country)
  tmp$lagTFR[ix] = mean_lag5( tmp$TFR[ix])  
} # for this.country

# write.csv(tmp, file="../R/DATA-PROCESSED/001-LoadCleanHFDHMDData.csv")
