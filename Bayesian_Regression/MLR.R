#Simple Linear Regression
for(j in 2:nRep){
  
  # update beta0
  vstar <- 1/(n/sig2[j-1] + 1/v0)
  mstar <- vstar*((1/sig2[j-1])*sum(y - beta1[j-1]*x) + (1/v0)*m0)
  beta0[j] <- rnorm(1, mstar, sqrt(vstar))
  
  # update beta1
  vstar <- 1/(sumx2/sig2[j-1] + 1/v1)
  mstar <- vstar*((1/sig2[j-1])*sum(x*(y - beta0[j])) + (1/v1)*m1)
  beta1[j] <- rnorm(1, mstar, sqrt(vstar))
  
  # update sigma2
  astar <- a + 0.5*n
  bstar <- 0.5*sum((y - (beta0[j] + beta1[j]*x))^2) + b
  sig2[j] <- rinvgamma(1, astar, rate=bstar)
  
}

#Multiple linear regression (2 covariates)
beta0 <- numeric()
beta1 <- numeric()
beta2 <- numeric()
#beta3 <- numeric()
sig2 <- numeric()

m0 <- m1 <- m2 <- 0
#m0 <- m1 <- m2 <- m3 <- 0
v0 <- v1 <- v2 <- 100
#v0 <- v1 <- v2 <- v3 <- 100
a <- b <- 1

beta0[1] <- beta1[1] <- beta2[1] <- 0
#beta0[1] <- beta1[1] <- beta2[1] <- beta3[1] <- 0
sig2[1] <- 1

nRep <- 100000

n <- length(y)
#sum of x^2 for each of the individual x's
sumx2_1 <- sum(x1^2)
sumx2_2 <- sum(x2^2)
#sumx2_3 <- sum(x3^2)

for(j in 2:nRep){
  
  # update beta0
  vstar <- 1/(n/sig2[j-1] + 1/v0)
  mstar <- vstar*((1/sig2[j-1])*sum(y - beta1[j-1]*x1 - beta2[j-1]*x2) + (1/v0)*m0)
  beta0[j] <- rnorm(1, mstar, sqrt(vstar))
  #vstar <- 1/(n/sig2[j-1] + 1/v0)
  #mstar <- vstar*((1/sig2[j-1])*sum(y - beta1[j-1]*x1 - beta2[j-1]*x2 - beta3[j-1]*x3) + (1/v0)*m0)
  #beta0[j] <- rnorm(1, mstar, sqrt(vstar))
  
  # update beta1
  vstar <- 1/(sumx2_1/sig2[j-1] + 1/v1)
  mstar <- vstar*((1/sig2[j-1])*sum(x1*(y - beta0[j] - beta2[j-1]*x2)) + (1/v1)*m1)
  beta1[j] <- rnorm(1, mstar, sqrt(vstar))
  #vstar <- 1/(sumx2_1/sig2[j-1] + 1/v1)
  #mstar <- vstar*((1/sig2[j-1])*sum(x1*(y - beta0[j] - beta2[j-1]*x2 - beta3[j-1]*x3)) + (1/v1)*m1)
  #beta1[j] <- rnorm(1, mstar, sqrt(vstar))
  
  # update beta2
  vstar <- 1/(sumx2_2/sig2[j-1] + 1/v2)
  mstar <- vstar*((1/sig2[j-1])*sum(x2*(y - beta0[j] - beta1[j]*x1)) + (1/v2)*m2)
  beta2[j] <- rnorm(1, mstar, sqrt(vstar))
  #vstar <- 1/(sumx2_2/sig2[j-1] + 1/v2)
  #mstar <- vstar*((1/sig2[j-1])*sum(x2*(y - beta0[j] - beta1[j]*x1 - beta3[j-1]*x3)) + (1/v2)*m2)
  #beta2[j] <- rnorm(1, mstar, sqrt(vstar))
  
  #update beta3
  #vstar <- 1/(sumx2_3/sig2[j-1] + 1/v3)
  #mstar <- vstar*((1/sig2[j-1])*sum(x3*(y - beta0[j] - beta1[j]*x1 - beta2[j]*x2)) + (1/v3)*m3)
  #beta3[j] <- rnorm(1, mstar, sqrt(vstar))
  
  # update sigma2
  astar <- a + 0.5*n
  bstar <- 0.5*sum((y - (beta0[j] + beta1[j]*x1 + beta2[j]*x2))^2) + b
  sig2[j] <- rinvgamma(1, astar, rate=bstar)
  #astar <- a + 0.5*n
  #bstar <- 0.5*sum((y - (beta0[j] + beta1[j]*x1 + beta2[j]*x2 + beta3[j]*x3))^2) + b
  #sig2[j] <- rinvgamma(1, astar, rate=bstar)
  
}