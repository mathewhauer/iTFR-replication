#------------------------------------------
# Carl Schmertmann
# created 06 May 2018
# edited  07 May 2018
#
# fast_bTFR function (does NOT require Stan)
#
# inputs: integer number of children C [may be an n-vector]
#         counts of women W = (W15...W45) [may be an nx7 matrix]
#         scalar or vector of any available q5 estimates [may be a nx(#est) matrix]
#         lowest possible TFR value under consideration (LTFR)
#         highest possible TFR value under consideration (HTFR)
#
# output: if n=1, a single 2-component list containing
#            $ a density object for TFR
#            $ the posterior median of TFR
#         if n>1, a list of 2-component lists, one per population
#------------------------------------------

fast_bTFR = function(C , W , q5_est , 
                     LTFR = 0, HTFR = 20, delta_TFR=.02,
                     Lq5 = .0001, Hq5  =.0999,
                     rownames=NULL) {
require(assertthat)
  
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
# calculate the prior prob of each (k,beta1,beta2)
# combination in the grid. (Note that this does NOT
# include f(q5), which will vary by observation
#--------------------------------------------------

  G$fkbeta = apply(G,1,function(x) prod(dnorm(x[2:4])) )
  G$fq5    = NA
  G$fprior = NA
  
# add the eta columns to the grid
  G = data.frame(G, this.eta)

for (i in 1:n) {
  
  this.C  = C[i]
  this.W  = W_matrix[i,]
  this.q5 = q5_matrix[i,]

  # expected children for each row of the grid, if TFR=1
  this.Cstar1 = as.matrix( G[, paste0('eta', seq(15,45,5))] ) %*% this.W
  
  # pick appropriate (a,b) constants for q5 ~ Beta(a,b)
  # 90% prob. between 1/2 lowest est and 2* highest est
  ab = LearnBayes::beta.select( list(x= min(q5_matrix[i,])/2, p=.05), 
                                list(x= 2*max(q5_matrix[i,]), p=.95))
  

  # calculate the prior prob of each q5 in the grid
  G$fq5 = dbeta( G$q5, ab[1],ab[2])

  # fill in the prior probabilities f(q5,k,beta)
  G$fprior = G$fkbeta * G$fq5
  
  posterior_prob = function(TFR) {
    Cstar = TFR * this.Cstar1
    L     = dpois(this.C, Cstar)
    return ( sum( L * G$fprior))
  }
  
  TFR_vals = seq(from = LTFR+delta_TFR/2, 
                 to   = HTFR-delta_TFR/2,
                 by   = delta_TFR)
  
  pp = sapply( TFR_vals, posterior_prob) 

  inv_cdf = approxfun( x= cumsum(pp)/sum(pp), 
                       y=TFR_vals + delta_TFR/2)
  
  simdens  = density( inv_cdf( seq(.005,.995,.01)), adjust=1.5 )
  
  result[[i]] = list( dens=simdens, median=inv_cdf(.50))
  
} # for i

  if (n==1) return(result[[1]])
  if (n>1) return(result)
} # fast_bTFR


