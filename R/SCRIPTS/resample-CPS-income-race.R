## @knitr resample-CPS-income-race

# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

library(tidyverse)

# graphics.off()
# windows(record=TRUE)

data.already.processed = TRUE

inc_cutoffs = c(-Inf, 25, 50, 75, 100, 150, 200, 250, 300, Inf)
inc_labels = c('<25','25-50','50-75','75-100',
               '100-150','150-200',
               '200-250','250-300','>300')

# if (!data.already.processed) {
# 
#   if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
#   
#   ddi <- read_ipums_ddi("../DATA-RAW/CPS/cps_00003.xml")
#   
#   # select March supplement data only
#   
#   data <- read_ipums_micro(ddi) %>%
#             filter(  ASECFLAG==1 ) 
#   
# 
#   
#   
#   case = expand.grid(this_race = c('White','Non-White'),
#                      this_income = inc_labels)
#   
#   
#   case$xTFR = NA
#   case$Q05  = NA
#   case$Q95  = NA
#   
#   
#   xTFR_star = function(ix,mydata) {
#     C = sum(mydata$C[ix])
#     W15 = sum(mydata$W15[ix])
#     W20 = sum(mydata$W20[ix])
#     W25 = sum(mydata$W25[ix])
#     W30 = sum(mydata$W30[ix])
#     W35 = sum(mydata$W35[ix])
#     W40 = sum(mydata$W40[ix])
#     W45 = sum(mydata$W45[ix])
#     
#     W = (W15+W20+W25+W30+W35+W40+W45)
#     pi2534 = (W25 + W30) / W 
#     
#     xTFR = (10.65 - 12.55 * pi2534) * C/W
#     
#     return(xTFR)
#   }
#   
#   
#   
#   for (icase in 1:nrow(case)) {
#     
#     this_race = case$this_race[icase]
#     this_income = case$this_income[icase]
#     
#     print(this_race)
#     print(this_income)
#     
#     mydata = data %>%
#               mutate(income_cat = cut(HHINCOME/1000, 
#                                       breaks=inc_cutoffs,
#                                       labels=inc_labels),
#                      age        = 5*floor(AGE/5),
#                      sex        = SEX,
#                      race_eth   = case_when(
#                                     RACE == 100 ~ "White",
#                                     TRUE        ~ "Non-White"
#                                  )) %>%
#               filter(race_eth   == this_race, 
#                      income_cat == this_income) %>%
#               group_by(YEAR,SERIAL) %>%
#               summarize(   income = income_cat[1],
#                            race   = race_eth[1],
#                            C      = sum(ASECWT[ age== 0 ]),
#                            W15    = sum(ASECWT[(age==15) & sex==2]),
#                            W20    = sum(ASECWT[(age==20) & sex==2]),
#                            W25    = sum(ASECWT[(age==25) & sex==2]),
#                            W30    = sum(ASECWT[(age==30) & sex==2]),
#                            W35    = sum(ASECWT[(age==35) & sex==2]),
#                            W40    = sum(ASECWT[(age==40) & sex==2]),
#                            W45    = sum(ASECWT[(age==45) & sex==2])
#                          )
#     
#     HH = nrow(mydata)
#     print(paste(HH,'households'))
#     
#     imat = matrix(NA, nrow(mydata), 1000)
#     
#     for (j in 1:ncol(imat)) { imat[,j] = sample(nrow(mydata), replace=TRUE) }
#     
#     # calculate xTFR from a vector of indices
#     est = xTFR_star(1:nrow(mydata), mydata)
#     
#     case$xTFR[icase] = est
#     
#     z = sapply(1:ncol(imat), function(j) { xTFR_star(imat[,j], mydata)})
#     
#     Q = quantile(z, probs=c(.05,.95))
#     
#     case$Q05[icase] = Q[1]
#     case$Q95[icase] = Q[2]
#     
#     print(case[icase,])
#     
#     plot( density(z, adj=1.5), xlim=c(1,3),
#           main=paste(case$this_race[icase], case$this_income[icase]))
#     abline(v=c(Q,est), lty=2, col='red')
#   
#   } # for icase
#   
#   
#   write.csv(case, file='../DATA-PROCESSED/CPS-income-race.csv')
# 
# } else {
#    case = read.csv(file='../DATA-PROCESSED/CPS-income-race.csv') %>%
#            mutate( this_income = factor(this_income, levels=inc_labels),
#                    this_race   = factor(this_race,levels=c('White','Non-White')))
# }

   case = read_csv(file='../R/DATA-PROCESSED/CPS-income-race.csv') %>%
           mutate( this_income = factor(this_income, levels=inc_labels),
                   this_race   = factor(this_race,levels=c('White','Non-White')))


ggplot(data=case, 
          aes(x=this_income,y=xTFR,
              shape=this_race, group=this_race, color=this_race)) +
     geom_ribbon(aes(ymin = Q05, ymax = Q95, fill = this_race), color = NA, alpha = 0.3) +
     geom_line(linetype=2) +
     geom_point(size=3.5) +
     scale_shape_manual(values=c('W','N'), guide=FALSE) + 
     scale_y_continuous(breaks=seq(1.6,2.6,0.2)) +
     geom_hline(yintercept = 2) +
     theme_bw() +
     theme(axis.title=element_text(size=14,face="bold")) +
     labs(x='Household Income in Thousands',
          y='TFR') +
     scale_color_manual(values=c('red','blue'), guide=FALSE) +
     geom_text(x=7.5,y=2.7,size=7,label='Non-White', color='black') +
     geom_text(x=8.5,y=1.7,size=7,label='White', color='black') +
     guides(fill=FALSE)
   