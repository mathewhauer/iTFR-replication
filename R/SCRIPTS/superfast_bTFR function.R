#------------------------------------------
# Carl Schmertmann
# created 07 May 2018
# edited  11 Jul 2019
#
# superfast_bTFR function (does NOT require Stan)
#
# inputs: integer number of children C [may be an n-vector]
#         counts of women W = (W15...W45) [may be an nx7 matrix]
#         scalar or vector of any available q5 estimates [may be a nx(#est) matrix]
#         lowest possible TFR value under consideration (LTFR)
#         highest possible TFR value under consideration (HTFR)
#
# output: if n=1, a single 4-component list containing
#            $ a density object for TFR
#            $ the 10,50,90%iles of the posterior distribution
#         if n>1, a list of 4-component lists, one per population
#------------------------------------------

library(tidyverse)
library(LearnBayes)

superfast_bTFR = function(C , W , q5_est , 
                     LTFR = 0, HTFR = 20, delta_TFR=.02,
                     Lq5 = .0001, Hq5  =.0999,
                     rownames=NULL) {
require(assertthat)
require(LearnBayes)  
  n = length(C)
  W_matrix  = matrix(W, nrow=n)
  q5_matrix = matrix(q5_est, nrow=n) 
  
  if (n==1) {
    assert_that( length(W)==7, 
                 msg='There is only one C value, but W does not have 7 elements')
  } else if (n>1) {
    assert_that( nrow(W)==n, ncol(W)==7,
                 msg='C and W are incompatible')
    if (class(q5_est)=='matrix') {
      assert_that( nrow(q5_est)==n,
                 msg='C and matrix of q5_est are incompatible')
    }
    if (class(q5_est)=='numeric') {
      assert_that( length(q5_est)==n,
      msg='C and vector of q5_est are incompatible')
    }
    
  }
  
  result = vector('list',n)  # container for results
  if (!is.null(rownames)) names(result) = rownames
  
#--------------------------
# fertility: phi10...phi45
# phi_a is the proportion of
# lifetime fertility during
# age group [a,a+5)  
#--------------------------
    
  m = c(0, 1.39212, 1.59237, 1.22909, 0.4523, -0.88676, -3.43722)
  names(m) = seq(15,45,5)
  
  X = matrix( c(0, 0.27443, 0.54078, 0.73193, 0.87739, 1.04049,  1.51719, 
                0, 0.31782, 0.51079, 0.51072, 0.34831, 0.05289, -0.72364),
              nrow=7,ncol=2)
  rownames(X) = seq(15,45,5)
  
  phi = function(beta1,beta2) {
    
    gamma = as.vector( m + X %*% c(beta1,beta2))  # log odds rel to F15
    phi   = exp(gamma) / sum(exp(gamma))          # proportion of lifetime fertil by age group
    
    result = c(0, phi)
    names(result) = seq(10,45,5)
    
    return(result)
    
  } # phi

#-------------------------
# mortality: L0,L5,...L45
#-------------------------  
## function to convert (q5,k) into life table L column in Wilmoth et al. model

  bigL = function(q5,k) {
    
    ## mortality constants for females, ages 0,1,...45 
    ## Wilmoth et al. coefficients from Pop Studies
    ## J Wilmoth, S Zureick, V Canudas-Romo, M Inoue & C Sawyer (2011) 
    ## A flexible two-dimensional mortality model for use in indirect estimation, 
    ## Population Studies, 66:1, 1-28, DOI: 10.1080/00324728.2011.611411 
    
    af = c(-0.6619, -99, -2.5608, -3.2435, -3.1099, -2.9789, -3.0185, 
           -3.0201, -3.1487, -3.269, -3.5202)
    bf = c(0.7684, -99, 1.7937, 1.6653, 1.5797, 1.5053, 1.3729, 1.2879, 
           1.1071, 0.9339, 0.6642)
    cf = c(-0.0277, -99, 0.1082, 0.1088, 0.1147, 0.1011, 0.0815, 0.0778, 
           0.0637, 0.0533, 0.0289)
    vf = c(0, -99, 0.2788, 0.3423, 0.4007, 0.4133, 0.3884, 0.3391, 0.2829, 
           0.2246, 0.1774)
    
    # log mortality rates for age intervals starting at x=0,1,5,10,...,45
    logmx = af + bf * log(q5) + cf * (log(q5))^2 + vf * k
    names(logmx) = c(0,1,seq(5,45,5))
    
    # mortality rates (special treatment for 4m1, because q5 has to match input)
    mx = exp(logmx)
    mx['1'] = -1/4 * (mx['0'] + log(1-q5))
    
    # survival (little_l)
    n = c(1,4,rep(5,9))
    little_l = c(1, exp( -cumsum( n*mx )))
    names(little_l) = c(0,1,seq(5,50,5))
    
    # person-years (bigL, trapezoidal approximation)
    # [0,1) [1,5) [5,10) ... [45,50)
    L = n/2 * ( head(little_l, -1) + tail(little_l, -1) ) 
    
    # collapse the first 2 intervals to make 5-year groups [0,5) [5,10) ... [45,50)
    result = c(L['0'] + L['1'], tail(L,-2))
    
    return(result)
    
  } # bigL  

#--------------------------------------------------
# Expected children at TFR=1
# eta_a is the expected number
# of surviving 0-4 year old children per woman currently
# in age group [a,a+5) if TFR=1
#--------------------------------------------------
  
  eta = function( q5,k, beta1, beta2) {
  
  this.phi = phi( beta1, beta2)
  this.L   = bigL(q5,k)
  
  surv = this.L[paste(seq(10,40,5))]/this.L[paste(seq(15,45,5))]
  
  eta = this.L['0']/5 * 1/2 * (surv * this.phi[paste(seq(10,40,5))]  + this.phi[paste(seq(15,45,5))])
  names(eta) = seq(15,45,5)
  
  return(eta)
}

#--------------------------------------------------
# create a large grid of (q5,k,beta1,beta2) values
#--------------------------------------------------
  q5_vals = Lq5 + (Hq5-Lq5)*1:4/5
  z_vals = seq(-2, +2, length=5)

  G = expand.grid( q5=q5_vals, k=z_vals, b1=z_vals, b2=z_vals)

#--------------------------------------------------
# calculate eta15...eta45 for each (q5,k,beta1,beta2)
# combination in the grid. eta_a is the expected number
# of surviving 0-4 year old children per woman currently
# in age group [a,a+5) AT TFR=1
#--------------------------------------------------

  this.eta = t(apply(G,1, function(x) eta(x[1], x[2], x[3], x[4])))
  colnames(this.eta) = paste0('eta', seq(15,45,5))

#--------------------------------------------------
# array calculations
# there are I combinations of (q5,k,beta1,beta2), one per row of G
# there are J populations (J=n= length of C = rows of W)  
# there are K TFR values for which we want marginal posterior probs
#--------------------------------------------------


  TFR_vals = seq(from = LTFR+delta_TFR/2, 
                 to   = HTFR-delta_TFR/2,
                 by   = delta_TFR)
  

  # W_eta will be an IxJ matrix of expected numbers of children
  # when TFR=1. For example, W_eta[63,12] is the number of expected
  # children in population 12 at the 63rd parameter combination if TFR=1
  
  W_eta = this.eta %*% t(matrix(W, ncol=7))
  
  ## ab is a J x 2 matrix of beta parameters, for under-5 mortality in each 
  ## population
  ab    = t( sapply( q5_est,
                     function(x) {
                       LearnBayes::beta.select(
                         list(x=min(x)/2, p=.05),
                         list(x=max(x)*2, p=.95)
                       )
                     }))
  
  ## f_ij is an IxJ matrix containing the PRIOR probs of each parameter
  ## combination in each population
  
  f_ij = outer(1:nrow(G), 1:n,
               function(i,j) {dbeta(G$q5[i], ab[j,1], ab[j,2]) *
                              dnorm(G$k[i]) * 
                              dnorm(G$b1[i]) *
                              dnorm(G$b2[i]) }
               )
  
  ## Cstar_ijk is an IxJxK array of expected numbers of children for 
  ## each (i=parameter set, j=population, k=TFR level)
  
  Cstar_ijk = outer( W_eta, TFR_vals, '*')
  
  ## Cobs_ijk is an (IJK)-vector of the observed # of children,
  ## in exactly the same order as Cstar_ijk 
  Cobs_ijk  = C[ arrayInd( seq(Cstar_ijk), dim(Cstar_ijk))[,2] ]

  ## L_ijk is an (IJK)-vector of the likelihoods for  
  ## each (i=parameter set, j=population, k=TFR level)
  
  L_ijk = dpois(Cobs_ijk, Cstar_ijk)  # long vector 
  
  ## posterior is an IxJxK array of prior * likelihood, for 
  ## each (i=parameter set, j=population, k=TFR level)
  
  posterior = array( as.vector(f_ij) * L_ijk, 
                     dim=c(nrow(G), n, length(TFR_vals)),
                     dimnames=list(NULL,rownames,TFR_vals))

  ## pp is a J x K matrix of (relative) marginal posterior probs for
  ## each (j=population, k=TFR level)
  
  pp = apply(posterior,2:3, sum)

  ## simulate the density function for a row (=a population)
  ## by using the cumulative (rel) probabilties
  
  simdens = function(pprow) {
    inv_cdf = approxfun( x= cumsum(pprow)/sum(pprow), 
                         y=TFR_vals + delta_TFR/2)
    
    d  = density( inv_cdf( seq(.005,.995,.01)), adjust=1.5)
    return( list( dens= d, 
                  Q10 = inv_cdf(.10), 
                  Q50 = inv_cdf(.50),
                  Q90 = inv_cdf(.90)))
  } 
  
  result = apply(pp, 1, simdens)
  
  if (n==1) return(result[[1]])
  if (n>1) return(result)
} # superfast_bTFR


#------------------------------------------------------
# Example 1: State of Florida data from 2013-2017 ACS
# Scalar C, vector W15...W45  
#------------------------------------------------------

C_FL  = 1105362
W_FL  = c(579563, 632804, 657451, 629384, 612045, 631189, 672041)
q5_FL = .007  #approx

est = superfast_bTFR(C=C_FL, W=W_FL, q5_est=q5_FL, delta_TFR=.01) 

plot( est$dens, main='Florida 2013-2017', xlab='TFR')
abline(v=est$median, lty=2)

#------------------------------------------------------
# Example 2: 67 Florida counties from 2013-2017 ACS
# Vector C, Matrix W15...W45  
#------------------------------------------------------

county_names = 
  c("Alachua", "Baker", "Bay", "Bradford", "Brevard", "Broward", 
    "Calhoun", "Charlotte", "Citrus", "Clay", "Collier", "Columbia", 
    "DeSoto", "Dixie", "Duval", "Escambia", "Flagler", "Franklin", 
    "Gadsden", "Gilchrist", "Glades", "Gulf", "Hamilton", "Hardee", 
    "Hendry", "Hernando", "Highlands", "Hillsborough", "Holmes", 
    "Indian River", "Jackson", "Jefferson", "Lafayette", "Lake", 
    "Lee", "Leon", "Levy", "Liberty", "Madison", "Manatee", "Marion", 
    "Martin", "Miami-Dade", "Monroe", "Nassau", "Okaloosa", "Okeechobee", 
    "Orange", "Osceola", "Palm Beach", "Pasco", "Pinellas", "Polk", 
    "Putnam", "St. Johns", "St. Lucie", "Santa Rosa", "Sarasota", 
    "Seminole", "Sumter", "Suwannee", "Taylor", "Union", "Volusia", 
    "Wakulla", "Walton", "Washington")

C_FL = 
  c(14138, 1616, 11316, 1395, 26816, 110377, 681, 5397, 5437, 11389, 
    16669, 3987, 1909, 773, 61722, 18767, 4308, 560, 2739, 851, 477, 
    760, 607, 1793, 2855, 8001, 4836, 85908, 869, 6404, 2430, 614, 
    500, 16375, 33694, 14868, 2005, 400, 975, 18034, 17240, 6349, 
    156200, 3549, 4119, 13176, 2352, 80510, 21034, 73263, 25856, 
    42610, 38343, 4134, 9525, 14845, 23808, 11551, 15741, 2348, 2341, 
    1137, 815, 24622, 1548, 3802, 1292)

names(C_FL) = county_names

W_FL  = matrix(
  c(
  11462,	21678,	11173,	 8633,	 7108,	 6618,	 6550,
    829,	  860,	  784,	  801,	  888,	  701,	  837,
   4789,	 5414,	 6460,	 5900,	 5166,	 5556,	 5832,
    618,	  522,	  651,	  813,	  615,	  771,	  704,
  14875,	14802,	15427,	15222,	14974,	15038,	18518,
  55214,	56378,	63859,	65060,	63279,	67038,	70021,
    593,	  325,	  406,	  399,	  346,	  311,	  405,
   3286,	 3318,	 2989,	 3064,	 3213,	 3456,	 4539,
   3020,	 2639,	 2647,	 2785,	 2868,	 2981,	 3842,
   7180,	 5817,	 6076,	 6142,	 6765,	 7106,	 7415,
   8639,	 8247,	 8081,	 8466,	 8841,	 9111,	10475,
   1858,	 1768,	 2061,	 2045,	 1724,	 1864,	 2048,
    985,	  696,	  919,	  856,	  966,	  650,	  815,
    268,	  317,	  360,	  349,	  385,	  378,	  425,
  26364,	32031,	39426,	35138,	29816,	29360,	30255,
   9910,	12429,	11578,	 9757,	 8904,	 7778,	 9149,
   2861,	 2218,	 2276,	 2560,	 2395,	 3024,	 3222,
    163,	  273,	  257,	  315,	  194,	  298,	  276,
   1168,	 1587,	 1412,	 1450,	 1685,	 1427,	 1671,
    488,	  393,	  444,	  443,	  400,	  505,	  502,
    142,	  274,	  368,	  311,	  191,	  327,	  267,
    372,	  254,	  321,	  214,	  193,	  520,	  424,
    359,	  169,	  423,	  235,	  233,	  471,	  426,
    837,	  814,	  774,	  793,	  645,	  859,	  754,
   1365,	 1150,	 1324,	 1145,	  847,	 1493,	 1139,
   4760,	 4379,	 4469,	 4398,	 4650,	 4742,	 5677,
   2448,	 2062,	 2236,	 2179,	 2229,	 2376,	 2377,
  42802,	46758,	53268,	49667,	46653,	46886,	46820,
    450,	  430,	  507,	  484,	  318,	  668,	  569,
   3430,	 3536,	 3423,	 3324,	 3475,	 3592,	 4304,
   1221,	 1163,	 1230,	 1261,	 1429,	 1096,	 1270,
    243,	  284,	  328,	  210,	  377,	  393,	  461,
    202,	  157,	  116,	  103,	  206,	  338,	  250,
   8554,	 8108,	 8393,	 8658,	 8535,	 9865,	10233,
  18143,	17575,	19213,	18335,	17906,	19081,	20723,
  13524,	25581,	11298,	 9372,	 8191,	 7849,	 7702,
   1069,	  939,	 1069,	 1113,	  983,	 1145,	 1159,
    266,	  111,	  196,	  223,	  290,	  130,	  149,
    544,	  405,	  559,	  460,	  428,	  495,	  537,
   9328,	 8823,	 9626,	 9517,	 8783,	10616,	11471,
   8452,	 8763,	 9522,	 8952,	 8539,	 9185,	10298,
   3845,	 3320,	 3130,	 3095,	 3542,	 3581,	 4912,
  77171,	88853,	96724,	93579,	93692,	97775,	100789,
   1728,	 1677,	 2230,	 2209,	 2169,	 2452,	 2539,
   2076,	 2011,	 2055,	 1956,	 2250,	 2746,	 2553,
   4992,	 6276,	 7664,	 6844,	 5906,	 5341,	 5855,
   1030,	 1017,	 1094,	 1034,	 1058,	 1101,	 1022,
  43711,	54027,	55719,	49865,	47277,	44582,	44238,
  11336,	11034,	11475,	11402,	10938,	12885,	11410,
  39428,	39219,	41819,	40563,	41249,	42545,	47733,
  13792,	12515,	13307,	14280,	14999,	15908,	16882,
  22405,	24854,	28571,	27649,	26704,	26948,	32037,
  20104,	20047,	20873,	19952,	19614,	19453,	19881,
   2074,	 1874,	 1991,	 1912,	 1868,	 1861,	 2046,
   5030,	 4263,	 5087,	 5149,	 5188,	 5190,	 5769,
   8692,	 8390,	 8412,	 8258,	 9077,	 9205,	11606,
  13875,	14716,	16161,	15712,	15250,	16069,	16905,
   7016,	 5779,	 5498,	 6263,	 6986,	 7760,	 8415,
   8508,	 7990,	 8227,	 8359,	 8746,	 8638,	 9553,
   1199,	 1162,	 1204,	 1108,	 1057,	 1508,	 1667,
    879,	 1559,	 1249,	 1234,	 1018,	 1220,	 1205,
    550,	  514,	  467,	  598,	  392,	  732,	  467,
    349,	  262,	  381,	  312,	  405,	  253,	  336,
  13884,	15430,	14949,	13461,	13459,	14016,	16067,
    781,	  687,	  773,	  869,	 1003,	 1017,	  970,
   1358,	 1323,	 1874,	 2059,	 1971,	 1561,	 1948,
    669,	  558,	  568,	  510,	  594,	  744,	  725), 
     ncol=7, byrow=TRUE, 
     dimnames = list(county_names, seq(15,45,5) ) 
)

  
#approx: just assume all counties have similar, low mortality  
q5_FL = rep(.007, length(C_FL))  

# result will be a list with length= number of counties (67 for FL)
est        = superfast_bTFR(C=C_FL, W=W_FL, q5_est=q5_FL)
names(est) = county_names

# ladder plot with medians and 80% intervals for county-level TFR
df = data.frame( name=county_names,
                 Q10 = sapply( county_names, function(k) est[[k]]$Q10),
                 Q50 = sapply( county_names, function(k) est[[k]]$Q50),
                 Q90 = sapply( county_names, function(k) est[[k]]$Q90)
                )

ggplot( data=df, aes(x=Q50, y=reorder(name, Q50))) +
    geom_point() +
    geom_segment( aes(x=Q10, xend=Q90,
                      y=reorder(name,Q50), yend=reorder(name,Q50))) +
    labs(x='TFR',y='County', 
         title='Florida County-Level TFR',
         subtitle='Medians and 80% Posterior Intervals',
         caption='Source: ACS 2013-2017 estimates') +  
       theme_bw()



