######################################################
#  data privacy alg for Hotelling's T2
#
# Created on Wed July 29 2020
# @author: Miaomiao YU 
######################################################

#导入程序包
library(Runuran)
library(cubature)
library(MASS)
options(warn=-1)

####################



#################################################################################
# the bound  of t2                                                            ###
# max.delta is the max shift                                                  ###
# p: dimension                                                                ###
#################################################################################
bound.fix <- function(t, max.delta, p)
{
  bound <- 4 * t * max.delta^{2} * p
  return(bound)
}




stand.x <-function(X, mu0, sigma0)
{
  chol.sigma0 <- chol(solve(sigma0))
  x.trans <- apply(X, 1, function(z) chol.sigma0 %*% (z - mu0))
  return (t(x.trans))
}


#################################################################################
# sample: x(n * p)                                                            ###
# mu0, sigma0: IC mean vector and covariance matrix                           ###
###################################################################################
t.square <- function(x, mu0, sigma0)
{ 
  n <- dim(x)[1]
  p <- dim(x)[2]
  x.trans <- stand.x(x, mu0, sigma0)
  x.bar <- apply(x.trans, 2, mean)
  hat.delta <- max(abs(x.bar))
  stat <- n * t(x.bar) %*% x.bar
  stat.weight <- stat * hat.delta
  return (stat.weight)
}



#################################################################################
# sample: x(n * p)                                                            ###
# mu0, sigma0: IC mean vector and covariance matrix                           ###
# epsilon: parameter of data privacy                                          ###
#################################################################################
t.square.lap <- function(x, mu0, sigma0,epsilon,bound)
{ 
  stat <- t.square(x, mu0, sigma0)
  noise <- abs(urlaplace(n=1, location=0, scale=(bound / epsilon))) #取laplace随机noise的绝对值
  stat.privacy <- stat + noise
  return (stat.privacy)
}


#################################################################################
# RL                                                                          ###
# mu0, sigma0: IC mean vector and covariance matrix                           ###
# alpha: frist error                                                          ###
#################################################################################
RL.T <- function(mu0,sigma0, mu, n,nu=3,epsilon, h)
{
  p <- dim(sigma0)[1]
  x <- mvrnorm(n=n, mu=mu, Sigma=sigma0)
  x.trans <- stand.x(x, mu0, sigma0)
  
  mu.real <- apply(x.trans,2,mean)
  diff1 <- apply(x.trans, 1, function(z) abs(z - mu.real))
  diff2 <-  abs(mu.real)
  t <-  max(diff1)
  delta <- max(diff2)
  bound <- bound.fix(t,delta,p)
  
  stat.test <- t.square.lap(x, mu0,sigma0, epsilon, bound)
  RL <- 1
  if ((stat.test > h)) {
    return(RL)
  } else{
    while ((RL <= 2000) & (stat.test <= h))
    { 
      RL <- RL + 1
      x <- mvrnorm(n=n, mu=mu, Sigma=sigma0)
      x.trans <- stand.x(x, mu0, sigma0)
      
      mu.real <- apply(x.trans,2,mean)
      diff1 <- apply(x.trans, 1, function(z) abs(z - mu.real))
      diff2 <-  abs(mu.real)
      t <-  max(diff1)
      delta <- max(diff2)
      bound <- bound.fix(t,delta,p)
      stat.test <- t.square.lap(x, mu0,sigma0, epsilon, bound)
    }
    return (RL)
  }
}

#################################################################################
# ARL                                                                         ###
# mu0, sigma0: IC mean vector and covariance matrix                           ###
# alpha: frist error                                                          ###
#################################################################################
ARL.T <- function(mu0,sigma0, mu,n,  epsilon,h, max.run)
{
  RL <- rep(0, max.run)
  for(i in 1:max.run)
  { 
    RL[i] <-RL.T(mu0,sigma0, mu,n,nu=3,epsilon, h)
    print (i)
  }
  ARL <- mean(RL)
  SDRL <- sd(RL)
  return (list(ARL=ARL, SDRL=SDRL))
}


#####参数设置###############
mu0 <- c(0, 0, 0)
arl0 <- 370
delta.real <- seq(from=0, to=5, by=0.5)
max.run <- 10000  #run number
epsilon <- c(0.1,
             0.2,
             0.3,
             0.4,
             0.5,
             0.6,
             0.7,
             0.8,
             0.9,
             1,
             1.5,
             2,
             2.5,
             3,
             3.5,
             4,
             4.5,
             5)


n <- 5
sigma0 <- rbind(c(1,0,0),c(0,4,0),c(0,0,9))
sigma <- sigma0
shift <- c(1,0,0) 
h <- c(1709.619141,
       860.7421875,
       581.4941406,
       436.7675781,
       352.34375,
       294.8242188,
       256.149292,
       225.8239746,
       201.5869141,
       182.7490234,
       126.1401367,
       98.41299057,
       81.51367,
       70.25146484,
       62.47314453,
       56.85546875,
       52.17407227,
       48.64501953) 


result <- matrix(0, nrow=(length(delta.real) * length(epsilon)),ncol=2)
for (index.e in 1: length(epsilon))
  for(z in 1: length(delta.real))
  {
    mu <- mu0 + delta.real[z] * shift * sqrt(diag(sigma0))
    esti.list <- ARL.T(mu0,sigma0, mu,n, epsilon[index.e],h[index.e],max.run)
    result[z + (index.e -1 ) *length(delta.real),1] <- esti.list[[1]]
    result[z + (index.e -1 ) *length(delta.real),2] <- esti.list[[2]]
    print(index.e)
    print(z)
  }
