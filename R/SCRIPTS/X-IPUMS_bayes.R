## @knitr IPUMS_bayes


library(dplyr)
library(rstan)

## Getting the DHS data.

require(RJSONIO)

# Import DHS Indicator data for TFR for each survey
tfr <- fromJSON("http://api.dhsprogram.com/rest/dhs/data/FE_FRTR_W_TFR?perpage=1000")
under5mort <- fromJSON("http://api.dhsprogram.com/rest/dhs/data/CM_ECMR_C_U5M?perpage=1000")
f1549 <- fromJSON("http://api.dhsprogram.com/rest/dhs/data/FE_FRTY_W_NPG?perpage=1000")
c00045 <- fromJSON("http://api.dhsprogram.com/rest/dhs/data/ML_FEVR_C_NUM?perpage=1000") 

# Unlist the JSON file entries
under5mort <- lapply(under5mort$Data, function(x) { unlist(x) })
tfr <- lapply(tfr$Data, function(x) { unlist(x) })
f1549 <- lapply(f1549$Data, function(x) { unlist(x) })
c00045 <- lapply(c00045$Data, function(x) { unlist(x) })

# Convert JSON input to a data frame
under5mort <- as.data.frame(do.call("rbind", under5mort),stringsAsFactors=FALSE) 
under5mort <- under5mort %>%
  filter(IsPreferred == 1) %>%# This gets just the 5-years preceding the survey.
  dplyr::select(SurveyId, u5mort = Value, SurveyYearLabel, SurveyYear, DHS_CountryCode, CountryName)
tfr <- as.data.frame(do.call("rbind", tfr),stringsAsFactors=FALSE)%>%
  dplyr::select(SurveyId, TFR = Value, SurveyYearLabel, SurveyYear, DHS_CountryCode, CountryName)

f1549 <- as.data.frame(do.call("rbind", f1549),stringsAsFactors=FALSE) %>%
  dplyr::select(SurveyId, W = Value, SurveyYearLabel, SurveyYear, DHS_CountryCode, CountryName)
c00045 <- as.data.frame(do.call("rbind", c00045),stringsAsFactors=FALSE) %>%
  dplyr::select(SurveyId, C = Value, SurveyYearLabel, SurveyYear, DHS_CountryCode, CountryName)

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

qx_const = 0.75

dat <- left_join(c00045, under5mort) %>%
  left_join(., tfr) %>%
  left_join(., f1549) %>%
  mutate(iTFR = as.numeric(C) / as.numeric(W) * 7,
         TFR = as.numeric(TFR),
         source  = "DHS",
         xTFR = 0,
         W = as.numeric(W),
         C = as.numeric(C),
         qx = as.numeric(u5mort)/1000,
         `iTFR+` = 7 * ((C/(1-qx_const*qx))/W),
         `xTFR+` = NA) %>%
  dplyr::select(Code = DHS_CountryCode, Year = SurveyYear, iTFR, xTFR, C, lagTFR=TFR, qx, 'iTFR+', 'xTFR+', source,W) %>%
  left_join(., women) %>%
  mutate(p2534 = (W25 + W30)/ W,
         xTFR = (coef(reg)[1] + coef(reg)[2] * p2534) * (C/W),
         `xTFR+` = (coef(reg)[1] + coef(reg)[2] * p2534) * ((C/(1-qx_const*qx))/W)) %>%
  na.omit




D <- dat




load(file='../R/DATA-RAW/svd.constants.RData')
m = svd.constants$m
X = svd.constants$X

ab = sapply(D$qx, function(this.q) {
  LearnBayes::beta.select( list(x= this.q/2, p=.05), list(x=this.q*2, p=.95))
})

q5_a = ab[1,]
q5_b = ab[2,]

#--- Wilmoth et al. coefficients from Pop Studies
wilmoth = 
  read.csv(text = '
           age,am,bm,cm,vm,af,bf,cf,vf
           0,  -0.5101, 0.8164,-0.0245,     0,-0.6619, 0.7684,-0.0277,     0
           1,      -99,    -99,    -99,   -99,    -99,    -99,    -99,   -99
           5,  -3.0435, 1.5270, 0.0817,0.1720,-2.5608, 1.7937, 0.1082,0.2788
           10, -3.9554, 1.2390, 0.0638,0.1683,-3.2435, 1.6653, 0.1088,0.3423
           15, -3.9374, 1.0425, 0.0750,0.2161,-3.1099, 1.5797, 0.1147,0.4007
           20, -3.4165, 1.1651, 0.0945,0.3022,-2.9789, 1.5053, 0.1011,0.4133
           25, -3.4237, 1.1444, 0.0905,0.3624,-3.0185, 1.3729, 0.0815,0.3884
           30, -3.4438, 1.0682, 0.0814,0.3848,-3.0201, 1.2879, 0.0778,0.3391
           35, -3.4198, 0.9620, 0.0714,0.3779,-3.1487, 1.1071, 0.0637,0.2829
           40, -3.3829, 0.8337, 0.0609,0.3530,-3.2690, 0.9339, 0.0533,0.2246
           45, -3.4456, 0.6039, 0.0362,0.3060,-3.5202, 0.6642, 0.0289,0.1774
           50, -3.4217, 0.4001, 0.0138,0.2564,-3.4076, 0.5556, 0.0208,0.1429
           55, -3.4144, 0.1760,-0.0128,0.2017,-3.2587, 0.4461, 0.0101,0.1190
           60, -3.1402, 0.0921,-0.0216,0.1616,-2.8907, 0.3988, 0.0042,0.0807
           65, -2.8565, 0.0217,-0.0283,0.1216,-2.6608, 0.2591,-0.0135,0.0571
           70, -2.4114, 0.0388,-0.0235,0.0864,-2.2949, 0.1759,-0.0229,0.0295
           75, -2.0411, 0.0093,-0.0252,0.0537,-2.0414, 0.0481,-0.0354,0.0114
           80, -1.6456, 0.0085,-0.0221,0.0316,-1.7308,-0.0064,-0.0347,0.0033
           85, -1.3203,-0.0183,-0.0219,0.0061,-1.4473,-0.0531,-0.0327,0.0040
           90, -1.0368,-0.0314,-0.0184,     0,-1.1582,-0.0617,-0.0259,     0
           95, -0.7310,-0.0170,-0.0133,     0,-0.8655,-0.0598,-0.0198,     0
           100,-0.5024,-0.0081,-0.0086,     0,-0.6294,-0.0513,-0.0134,     0
           105,-0.3275,-0.0001,-0.0048,     0,-0.4282,-0.0341,-0.0075,     0
           110,-0.2212,-0.0028,-0.0027,     0,-0.2966,-0.0229,-0.0041,     0
           ')

af = wilmoth$af[1:11]  # keep age 0,1,...45 
bf = wilmoth$bf[1:11]  # keep age 0,1,...45 
cf = wilmoth$cf[1:11]  # keep age 0,1,...45 
vf = wilmoth$vf[1:11]  # keep age 0,1,...45 

########################## INDEP STAN MODEL ##############################################
# STAN MODEL

stanModelText = '
data {
int<lower=0>        C;  // observed number of children 0-4
vector<lower=0>[7]  W;  // observed numbef of women 15-19...45-49

real q5_a;              // prior will be q(5) ~ beta( q5_a, q5_b) 
real q5_b;         

real af[11];        // 11 age groups starting at x=0,1,5,10,...,45
real bf[11];        // 11 age groups starting at x=0,1,5,10,...,45
real cf[11];        // 11 age groups starting at x=0,1,5,10,...,45
real vf[11];        // 11 age groups starting at x=0,1,5,10,...,45

vector[7]     m;       // mean vector for gamma
matrix[7,2]   X;       // covariance matrix for gamma

}                      

parameters {
real<lower=0,upper=1> q5;    // h = log(q5) in the Wilmoth et al. system
real                   k;    // k = Wilmoth et al. shape parameter

vector[2]     beta; 
real<lower=0> TFR; 

}

transformed parameters {
vector[7]             gamma;
real<upper=0>         h;        // log(q5)
simplex[7]            phi;      // proportion of total fertility by age group 
vector[8]             Fx;       // age-group fertility rates F10...F45  (F10=0)
real<lower=0>         mx[11];   // mortality rates for age groups starting at 0,1,5,10,...45
real<lower=0,upper=1> lx[12];   // life table {lx} values for 0,1,5...,50
real<lower=0,upper=5> Lx[10];   // life table {5Lx} values for 0,5...,45

vector[7] Kx;                   // expected surviving children 0-4 per woman 
// in age group x to x+4

real Kstar;                     // expected surviving total number of children               

//--- child mortality index for Wilmoth model
h = log(q5);

//--- fertility rates

gamma = m + X * beta;
for (i in 1:7) phi[i] = exp(gamma[i]) / sum( exp(gamma));

Fx[1] = 0;                                                // F10
for (i in 2:8) Fx[i]   = TFR * phi[i-1] / 5;              // F15...F45 

//--- mortality rates, life table survival probs, and big L values

for (i in 1:11) {  mx[i]  = exp( af[i] + bf[i]*h + cf[i]*square(h) + vf[i]*k ); }

mx[2]  = -0.25 * (mx[1] + log(1-q5) );               //  recalculate 1_mu_4 = -1/4 log(l[5]/l[1])

lx[1]  = 1;                                          // x=0
lx[2]  = lx[1] * exp(-mx[1]);                        // x=1
lx[3]  = lx[2] * exp(-4*mx[2]);                      // x=5
for (i in 3:12) lx[i]  = lx[i-1] * exp(-5*mx[i-1]);  // x=5,10,...50

Lx[1]                  = 1* (lx[1]+lx[2])/2 + 4*(lx[2]+lx[3])/2 ;   // 5L0
for (i in 2:10) Lx[i]  = 5* (lx[i+1]+lx[1+2])/2 ;                   // 5Lx

//--- main result: expected surviving 0-4 yr olds per woman in each age group
//      indexing is a bit complicated: 
//       Lx[1:10] is for age groups 0,5,...,45
//       Fx[1:8]  is for age groups 10,15,...,45
//       Kx[1:7]  is for age groups 15,...,45

for (i in 1:7) Kx[i]  = (Lx[i+2]/Lx[i+3] * Fx[i] + Fx[i+1]) * Lx[1]/2 ;

Kstar  = dot_product(W, Kx);
}


model {

// LIKELIHOOD
C ~ poisson(Kstar);

// PRIORS 
beta  ~ normal(0,1); 

q5 ~ beta(q5_a, q5_b);   // 90% prior prob. that q5 is between 1/2 and 2x estimated q5

k  ~ normal(0,1); 

}'
  

MODEL = stan_model(model_code=stanModelText, model_name='single schedule TFR')

# construct the matrix of constants for cumulative hazard calculations
n = c(1,5, rep(5,9))    # widths of life table age intervals for x=0,1,5,10...45
cs_constants = matrix(0, 11, 12)
for (j in 1:11) cs_constants[1:j,j+1] = head(n,j)

# construct the constants for the trapezoidal approx of L0...L45 from a row of l0,l1,l5,l10,...,l50
trapez_constants = matrix(0, 12, 10, 
                          dimnames=list(paste0('l', c(0,1,seq(5,50,5))), paste0('L', seq(0,45,5))))
trapez_constants[c('l0','l1','l5'), 'L0'] = c( 1/2, 5/2, 4/2)
for (j in 2:10) trapez_constants[j+1:2, j] = 5/2

stanInits = function(nchains=1) {
  L = vector('list',nchains)
  
  for (i in seq(L)) {
    L[[i]] =   list(
      q5 = rbeta(1, shape1=q5_a, shape2=q5_b),
      k  = rnorm(1, mean=0,sd=1),
      beta = runif(2, min=-.10, max=.10),
      TFR  = pmax( .10, rnorm(1, 2, sd=.50))
    )
  }
  return(L)
} # stanInits


# LOOP OVER SCHEDULES 
results = data.frame()

for (k in 1:nrow(D)) {
  
  print(paste('...starting', k, 'of', nrow(D) ))
  
  stanDataList = list(
    C = round(D$C[k]),
    W = as.numeric( D[k, paste0('W',seq(15,45,5))]),
    q5_a = q5_a[k],
    q5_b = q5_b[k],
    af = af,
    bf = bf,
    cf = cf,
    vf = vf,
    cs_constants = cs_constants,
    trapez_constants = trapez_constants,
    m = m,
    X = t(X)   # 7x2 in this version
  )
  
  # MCMC 
  nchains = 4
  
  fit = sampling(MODEL, 
                 data       = stanDataList,
                 pars       = c('TFR','beta','q5'),
                 init       = stanInits(nchains),
                 seed       = 6447100,
                 iter       = 900,
                 warmup     = 300,
                 thin       = 4,
                 chains     = nchains,
                 control    = list(max_treedepth = 12))
  
  
  tmp = as.data.frame( summary(fit, 'TFR', probs=c(.10,.25,.50,.75,.90))$summary )
  
  names(tmp) = c('post_mean','se_mean','sd',paste0('Q',c(10,25,50,75,90)),'n_eff','Rhat')
  
  tmp$Code   = D$Code[k]  
  tmp$Year   = D$Year[k]
  tmp$iTFR   = D$iTFR[k]
  tmp$lagTFR = D$lagTFR[k]
  
  tmp = dplyr::select(tmp, Code: lagTFR, post_mean, contains('Q'),n_eff,Rhat) %>%
    mutate_at(vars(iTFR:Rhat), round, digits=3) 
  
  results = rbind( results, tmp)
  
} # for k

rownames(results) = NULL
results

# write.csv(results, "../R/DATA-PROCESSED/IPUMS-bayesitfr.csv")