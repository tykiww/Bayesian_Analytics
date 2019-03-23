############# ASSESSING CONVERGENCE AND MIXING


# Markov Chain Monte Carlo vs Monte Carlo
# Introducing a dependence of multiple parameters, CORRELATED DRAWS!!
# For gibbs sampling, it is mu, sigma^2
# Because we are working with correlated draws, we have to assess convergence and mixing.

# CONVERGENCE:: Draws are being pulled from a "stationary distribution" Change has systematically stopped
# we can use a trace plot to figure out this "stationary distribution" -> posterior distribution

# Mixing
# If the chain is mixing well, it means there is dependence. If not, there is auto-orrelation
# Out assumption is... y1, ..., yn iid N(mu, sig^2)
# We can assess using an autocorrelation plot.

# ACF plots 

  # The more we see bars in the ACF plot (some kind of pattern), we see autocorrelation. We need to remedy
  # We are going to thin the chain by preserving each lag (place where there is no autocorrelation).
  # We don't want to reshuffle the draws for mu to remove 












# Let's return to the Pima Indian woman code.

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



norm_mcmc <- function(J, y, m, v, a, b, starting_mu_sig2 = TRUE) {
  
  #'  m, v, a, b : full-conditional priors for NN and IG
  #'  J : Monte-Carlo iterations
  #'  y : Data
  #'  starting_mu_sig2 : specifies whether to choose starting values from the data or choose 0 and 1
  
  # Set empy vector for mu, sig2
  mu <- c() ; sig2 <- c()
  # Get important values
  n <- length(y) ; ybar <- mean(y) ; s2 <- sd(y)
  if (starting_mu_sig2 == TRUE) {
    mu[1] <- ybar ; sig2[1] <- s2
  } else { mu[1] <- 0 ; sig2[1] <- 1}
  
  
  for (j in 2:J) {
    # Create posterior for unknown mu
    mstar <- ((n * v * ybar) + (m * sig2[j-1])) / (n * v + sig2[j-1])
    vstar <- (v * sig2[j-1]) / (n * v + sig2[j-1])
    mu[j] <- rnorm(1,mstar,sqrt(vstar))
    # Create posterior for unknown sig^2
    astar <- a + (n/2)
    bstar <- b + sum((y-mu[j])^2)/2
    sig2[j] <- rinvgamma(1,astar,bstar)
  }
  list("mu" = mu,"sig2" = sig2, "post-pred" = rnorm(J,mu,sqrt(sig2)), "prior-pred" = rnorm(J,m,sqrt(v)))
  # taking a looooooop of each mu and sig2, so okay to perish at certain values.
}

prior1 <- norm_mcmc(10000,bp.non,80,3^2,5,400,TRUE)
prior2 <- norm_mcmc(10000,bp.non,0,10000^2,.01,.01,TRUE)

plot(prior1$mu, type = 'l') # no worries about burning.
lines(prior2$mu, type = 'l', col = "red") # there is a slight difference
acf(prior1$mu) # both looks good.
acf(prior2$mu)
plot(density(prior1$mu), col = 'red') # prior effects[80] show closer to 80
lines(density(prior2$mu), col = 'blue') # prior effects[0] show closer to 0
quantile(prior1$mu, c(0.025,0.975))
quantile(prior2$mu, c(0.025,0.975))

plot(prior1$sig2, type = 'l') 
lines(prior2$sig2, type = 'l', col = "red")
acf(prior1$sig2)
acf(prior2$sig2)
plot(density(prior1$sig2), col = 'red')
lines(density(prior2$sig2), col = 'blue')


# Informed priors
m <- 80 ; v <- 3^2
a <- 5 ; b <- 400

# uninformed priors
m <- 0 ; v <- 10000^2
a <- .01 ; b <- .01



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

# keep relevant values and burn old ones (remove first 50, okay to be conservative)
keep <- 51:10000
plot(mu[keep],type='l')
plot(sig2[keep],type='l')

# USE ACF plots AFTER BURNING!! 
acf(mu[keep]) # autocorrelation looks pretty okay. within blue bars
acf(sig2[keep])  # nothing jumping out about these. The autocorrelation of lag 1 is close to 0
                 # This means each bar is comparing correlation of one value with the next value.
# LAG 1 correlation. just so we can understand this.

mu.original <- mu[-c(1,10000)] # remove because it has the same length
mu.lag1 <- mu[-c(1,2)]

head(cbind(mu.original,mu.lag1)) # original time with the next right after it.
plot(mu.original,mu.lag1) # practically no correlation. Doesn't matter which direction you go.
cor(mu.original,mu.lag1) 


plot(mu[keep],sig2[keep]) # we see a slight skew in the top area (sig2 side). Let's plot separately.

# IF THERE IS MIXING, we are saying that our inference is much more unstable. Our bounds aren't as safe.


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
# P(Ynew | Yobs) = integral from f(Ynew | µ, sig2)π(µ,sig2|Yobs) du dsig2

# I'm taking the posterior draws and sticking it into the normal.
ynew <- rnorm(9950, mu[keep], sqrt(sig2[keep])) 
hist(ynew)
# probability that the next pima woman has an unhealthy Diastolic BP (~60) is..
mean(ynew < 80) 
quantile(prior1$`post-pred`,c(0.025,.975))

# SUMMARIZE BIVARIATE POSTERIOR DISTRIBUTIONS!!

library(MASS)
# 2 dimensional perspective plot
persp(kde2d(mu[keep], sig2[keep]), phi = 50, theta = 30, xlab = "µ", ylab = "sigma^2")
# image plots to see the bivariate
image(kde2d(mu[keep], sig2[keep]))
contour(kde2d(mu[keep], sig2[keep]), add = TRUE, lwd = 2)
mtext(expression(sigma^2), side =2, line = 2.5)




##### COMPARISONS!!! Pretty much independent 2-sample t-test


# Yi|µ[1],sig2[1] ~iid N(mu[1], sig2[1])
# Xi|µ[2],sig2[2] ~iid N(mu[2], sig2[2])

plot(Pima.tr$bp, col = Pima.tr$type, cex =1, pch =20)
hist(Pima.tr$bp[Pima.tr$type == "No"])
hist(Pima.tr$bp[Pima.tr$type == "Yes"], add=TRUE, col = "red")
# overlapped but we see the blood pressure on average is slightly higher for the diabetes

# We want to compare µ[1] - µ[2] = 0.

# ø = (µ[1], µ[2], sig2[1], sig2[2]) Now 4 paramaters
# Result: Let ø be some parameter (it could be mutidimensional)
  # ø[1], ..., ø[J] b J draws from the posterior distribution of ø π(ø|y1, ..., yn)
# Then g(ø[1]), ..., g(ø[J]) are draws from π(g(ø)| y1, ..., yn) the posterior of g(ø)


# Ex: ø = (µ[1], µ[2], sig2[1], sig2[2])
# g(ø) = µ[1] - µ[2]
# So µ1[1]1 - µ2[1]1, ...,µ1[J] - µ2[J] are draws from π(µ[1] - µ[2] | Y1, ..., Yn, X1, ..., Xn)
# This holds because the posterior for µ1 and µ2 are independent!

# So, does this hold for any paramter using markov chains? YES!!

diab <- norm_mcmc(m = 80, v = 3^2, a = 5, b = 400, y = Pima.tr$bp[Pima.tr$type == "Yes"], J = 10000)
nondiab <- norm_mcmc(m = 80, v = 3^2, a = 5, b = 400, y = Pima.tr$bp[Pima.tr$type == "No"], J = 10000)

plot(diab$mu) # pretty reasonable
acf(diab$mu) # no ac that is extensive.
mu1_mu2 <- diab$mu - nondiab$mu
mean(mu1_mu2 > 0) # very high probability that it is greater than 0.
quantile(mu1_mu2, c(0.025, .5, 0.975)) ; mean(mu1_mu2)
hist(mu1_mu2)

# reasonable assumption that diabetic blood pressure is higher than non-diabetic

# assumptions, independence amongst and between group
  # independence between group is really hard to observe between groups.
# normality assumptions.
