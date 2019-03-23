# Both µ sig^2 Parameters UNKNOWN! Gibbs Sampling

# Things get complicated when we introduce sigma^2

# We have to sample through bivariate prior distributions.
# Complete Conditionals come into play..



# Pima Indian Women diastolic blood pressure for women without diabetes
  # Diastolic blood pressure is blood going through with no pressure.
  # People from Arizona area.

# Our model is... y1, ..., yn iid~ N(µ, sig^2) (µ mu is of principle interest, but don't know both µ and sigma^2)
# µ ~ N(m,v) ; sig^2 ~ IG(a,b) --> Likelihood ; Prior distribution
# Make inference using π(µ, sig^2 | y1, ..., yn)

library(MASS)
library(tidyverse)
library(invgamma)
glimpse(Pima.tr)

bp <- Pima.tr$bp

# Subset Healthy Women
bp.non <- bp[Pima.tr$type == "No"]

# We know that our n = 132
length(bp.non)


hist(bp.non)


# Let us now figure out our priors
# A healthy person, from wikipedia, has a bp = 80 on average. 3^2 for variance 89 down to 70 (2 sd). 
# That seems reasonable since healthy people stay in the range of mid 60 to 90. 
# 100,000 for variance just means a lot of uncertainty for prior 2. m = 80
# The variance can start with a flat distribution since we don't know much (a = .1, b = .1) prior 2
# Prior 1 can have something around a = 5, b = 400

# Informative vs Non-Informative priors

qnorm(c(.025, .975), 80, 3)
qinvgamma(c(.025, .975), 5, 400)

# Non- informative priors

qnorm(c(.025, .975), 80, 100000)
qinvgamma(c(.025, .975), .01, .01)


# We want access to π(µ, sig^2 | y1, ..., yn)
  # idea sample from it many times by using a Gibbs Sampler

# Step 1: Set µ[0] and sig^2[0] to some value ([0] means the zero-th iteration, usually a superscript in brackets ^(0))
  # mu[0] = ybar, sig^2[0] = s^2
# Step 2: for j in 1 to J, dothe following
  #a) take a sample from π(µ[j] | sig^2[j-1], y1, ... , yn) 
    # --> same as N(n ybar v+m sig^2[j-1]/(n v+sig^2[j-1]), v sig^2[j-1]/ (n v + sig^2[j-1]))
  #b) take a sample from π(sig^2[j] | µ[j], y1, ..., yn) 
    # --> we use µ[j] because we are updating it!
# Step 3: Discard first M iterations ( get rid of exploration a.k.a. "burning")


# WE ARE CREATING DRAWS FROM π(µ, sig^2 | y1, ..., yn), already does integration for the marginal as well!
  # values we are sticking in the mu, comes from the marginal distributions of µ and sig^2
  # integration all over the place, but we don't need to do any of it!

# Find our (Step 1)
ybar <- mean(bp.non)
s2 <- var(bp.non)
# Informed priors
m <- 80 ; v <- 3^2
a <- 5 ; b <- 400
# variables
n <- length(bp.non)

# intialize empty vector to hold our emty MCMC draws.
mu <- c()
sig2 <- c()


# Starting values
mu[1] <- ybar
sig2[1] <- s2
mu[1] <- 0
sig2[1] <- 1

J <- 10000 # it is good practice to start J small.
for (j in 2:J) { # start at 2 because sigma 2 starts at j-1
  # Update mu
  vstar <- v * sig2[j-1] / (n * v + sig2[j - 1])
  mstar <- (n * v * ybar + m * sig2[j - 1])/(n * v + sig2[j - 1])
  mu[j] <- rnorm(1, mstar, sqrt(vstar))
  # Update sig2 using mu[j]
  astar <- a + n/2
  bstar <- b + .5*sum((bp.non - mu[j])^2)
  sig2[j] <- rinvgamma(1,astar,bstar)
}

# traceplot shows random normality (should not see trends)
plot(mu,type='l') # well-behaved gibbs sampler, jumps up when using 0,1
plot(sig2,type='l')
plot(mu,sig2)

# keep relevant values and burn old ones (remove first 50)
keep <- 51:10000
plot(mu[keep],sig2[keep]) # we see a slight skew in the top area (sig2 side). Let's plot separately.
# let's investigate 
par(mfrow = c(1,2))
plot(density(mu[keep]))
plot(density(sig2[keep]))
par(mfrow = c(1,1)) # it is confirmed in the variance
# Not sure why, but there was a marginal right skew.

# Monte carlo estimate for the population mean.
mean(mu[keep]) 
quantile(mu[keep], c(.025,0.975))



### POSTERIOR PREDICTIVE (NO CLOSED FORM, SO MONTE CARLO)

# I'm taking the posterior draws and sticking it into the normal.
ynew <- rnorm(9950, mu[keep], sqrt(sig2[keep])) 
hist(ynew)
# probability that the next pima woman has an unhealthy Diastolic BP (~60) is..
mean(ynew < 60) 








