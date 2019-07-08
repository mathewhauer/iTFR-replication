# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

library(tidyverse)

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("DATA-RAW/CPS/cps_00003.xml")

# select March supplement data only

data <- read_ipums_micro(ddi) %>%
          filter(  ASECFLAG==1 ) 

inc_cutoffs = c(-Inf, 25, 50, 75, 100, 150, 200, 250, 300, Inf)
inc_labels = c('<25','25-50','50-75','75-100',
               '100-150','150-200',
               '200-250','250-300','>300')

mydata = data %>%
          mutate(income_cat = cut(HHINCOME/1000, 
                                  breaks=inc_cutoffs,
                                  labels=inc_labels),
                 age_cat    = 5*floor(AGE/5),
                 race_eth   = case_when(
                                RACE == 100 ~ "White",
                                TRUE        ~ "Non-White"
                             )) %>%
          group_by(sex=SEX,race_eth,age=age_cat,income=income_cat) %>%
          summarize(n=sum(ASECWT), unwtd=n()) %>%
          group_by(race_eth,income) %>%
            summarize( C   = sum(n[age==0]),
                       unwtd_W = sum(unwtd[(age %in% 15:45) & (sex==2)]),
                       W15 = sum(n[(age==15) & sex==2]),
                       W20 = sum(n[(age==20) & sex==2]),
                       W25 = sum(n[(age==25) & sex==2]),
                       W30 = sum(n[(age==30) & sex==2]),
                       W35 = sum(n[(age==35) & sex==2]),
                       W40 = sum(n[(age==40) & sex==2]),
                       W45 = sum(n[(age==45) & sex==2]),
                       W = W15+W20+W25+W30+W35+W40+W45,
                       pi2534 = (W25+W30)/W,
                       CWR = C/W,
                       iTFR = 7*CWR,
                       xTFR = (10.65 - 12.55*pi2534) * CWR) %>%
                 dplyr::select(race_eth,income,unwtd_W,C,W:xTFR)
              
## manually construct smoothed lines, just to make them
## look a little nicer over chunky intervals
tmp = mydata %>% 
        dplyr::select(income, xTFR) %>%
        mutate(income = as.numeric(income))

income_grid = seq(1,9,.10)

Wfit = loess(xTFR ~ income, data=tmp, span=1,
             subset= (race_eth=='White')) %>%
       predict(newdata=expand.grid(income=income_grid)) %>%
       as.numeric()

NWfit = loess(xTFR ~ income, data=tmp, span=1, 
             subset= (race_eth=='Non-White')) %>%
  predict(newdata=expand.grid(income=income_grid)) %>%
  as.numeric()

mysmooth = expand.grid( income = income_grid,
                        race_eth = c('White','Non-White'))

mysmooth$xTFR  = c(Wfit, NWfit)

ggplot(data=mydata, 
       aes(x=income,y=xTFR,shape=race_eth, group=race_eth,color=race_eth)) +
    geom_line(linetype=2) +
    geom_point(size=3.5) +
    scale_shape_manual(values=c('N','W'), guide=FALSE) + 
    scale_y_continuous(breaks=seq(1.6,2.6,0.2)) +
    geom_line(data=mysmooth, lwd=2) +
    geom_hline(yintercept = 2) +
    theme_bw() +
    theme(axis.text = element_text(face='bold', size=14),
          axis.title=element_text(size=14,face="bold")) +
    labs(x='Household Income in Thousands',
         y='TFR') +
    scale_color_manual(values=c('red','blue'), guide=FALSE) +
    geom_text(x=7.5,y=2.4,size=7,label='Non-White', color='black') +
    geom_text(x=8.0,y=1.8,size=7,label='White', color='black')  +
  

ggsave(file='FIGURES/CPS-income-race.pdf', width=8.5, height=11, dpi=400)


