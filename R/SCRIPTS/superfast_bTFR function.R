#------------------------------------------
# Carl Schmertmann
# created 07 May 2018
# edited  10 Jul 2019
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

W_FL  = structure(
     c(11462, 829, 4789, 618, 14875, 55214, 593, 3286, 3020, 
        7180, 8639, 1858, 985, 268, 26364, 9910, 2861, 163, 1168, 488, 
        142, 372, 359, 837, 1365, 4760, 2448, 42802, 450, 3430, 1221, 
        243, 202, 8554, 18143, 13524, 1069, 266, 544, 9328, 8452, 3845, 
        77171, 1728, 2076, 4992, 1030, 43711, 11336, 39428, 13792, 22405, 
        20104, 2074, 5030, 8692, 13875, 7016, 8508, 1199, 879, 550, 349, 
        13884, 781, 1358, 669, 21678, 860, 5414, 522, 14802, 56378, 325, 
        3318, 2639, 5817, 8247, 1768, 696, 317, 32031, 12429, 2218, 273, 
        1587, 393, 274, 254, 169, 814, 1150, 4379, 2062, 46758, 430, 
        3536, 1163, 284, 157, 8108, 17575, 25581, 939, 111, 405, 8823, 
        8763, 3320, 88853, 1677, 2011, 6276, 1017, 54027, 11034, 39219, 
        12515, 24854, 20047, 1874, 4263, 8390, 14716, 5779, 7990, 1162, 
        1559, 514, 262, 15430, 687, 1323, 558, 11173, 784, 6460, 651, 
        15427, 63859, 406, 2989, 2647, 6076, 8081, 2061, 919, 360, 39426, 
        11578, 2276, 257, 1412, 444, 368, 321, 423, 774, 1324, 4469, 
        2236, 53268, 507, 3423, 1230, 328, 116, 8393, 19213, 11298, 1069, 
        196, 559, 9626, 9522, 3130, 96724, 2230, 2055, 7664, 1094, 55719, 
        11475, 41819, 13307, 28571, 20873, 1991, 5087, 8412, 16161, 5498, 
        8227, 1204, 1249, 467, 381, 14949, 773, 1874, 568, 8633, 801, 
        5900, 813, 15222, 65060, 399, 3064, 2785, 6142, 8466, 2045, 856, 
        349, 35138, 9757, 2560, 315, 1450, 443, 311, 214, 235, 793, 1145, 
        4398, 2179, 49667, 484, 3324, 1261, 210, 103, 8658, 18335, 9372, 
        1113, 223, 460, 9517, 8952, 3095, 93579, 2209, 1956, 6844, 1034, 
        49865, 11402, 40563, 14280, 27649, 19952, 1912, 5149, 8258, 15712, 
        6263, 8359, 1108, 1234, 598, 312, 13461, 869, 2059, 510, 7108, 
        888, 5166, 615, 14974, 63279, 346, 3213, 2868, 6765, 8841, 1724, 
        966, 385, 29816, 8904, 2395, 194, 1685, 400, 191, 193, 233, 645, 
        847, 4650, 2229, 46653, 318, 3475, 1429, 377, 206, 8535, 17906, 
        8191, 983, 290, 428, 8783, 8539, 3542, 93692, 2169, 2250, 5906, 
        1058, 47277, 10938, 41249, 14999, 26704, 19614, 1868, 5188, 9077, 
        15250, 6986, 8746, 1057, 1018, 392, 405, 13459, 1003, 1971, 594, 
        6618, 701, 5556, 771, 15038, 67038, 311, 3456, 2981, 7106, 9111, 
        1864, 650, 378, 29360, 7778, 3024, 298, 1427, 505, 327, 520, 
        471, 859, 1493, 4742, 2376, 46886, 668, 3592, 1096, 393, 338, 
        9865, 19081, 7849, 1145, 130, 495, 10616, 9185, 3581, 97775, 
        2452, 2746, 5341, 1101, 44582, 12885, 42545, 15908, 26948, 19453, 
        1861, 5190, 9205, 16069, 7760, 8638, 1508, 1220, 732, 253, 14016, 
        1017, 1561, 744, 6550, 837, 5832, 704, 18518, 70021, 405, 4539, 
        3842, 7415, 10475, 2048, 815, 425, 30255, 9149, 3222, 276, 1671, 
        502, 267, 424, 426, 754, 1139, 5677, 2377, 46820, 569, 4304, 
        1270, 461, 250, 10233, 20723, 7702, 1159, 149, 537, 11471, 10298, 
        4912, 100789, 2539, 2553, 5855, 1022, 44238, 11410, 47733, 16882, 
        32037, 19881, 2046, 5769, 11606, 16905, 8415, 9553, 1667, 1205, 
        467, 336, 16067, 970, 1948, 725), .Dim = c(67L, 7L), 
      .Dimnames = list(county_names,
          c("15", "20", "25", "30", "35", "40", "45")
          )
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



