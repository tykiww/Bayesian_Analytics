

## Beta-Bernoulli JAGS practice

# This is the standard, probably the easiest Bayesian distributional model.

# How to get JAGS?
# First install R2jags, will give you an error. Follow the link to download rjags (Uses C++ bindings)
# Afterwards, you should be able to install R2jags and rjags.


# Load Library
library(R2jags)
library(rjags)

# Establish reliability
set.seed(5)

# Specify data
y <- rbinom(10, 1, .3)
# Set other Params
N <- length(y)


data.jags <- c('y', 'N')
parms <- c('theta')

# Bernoulli theta is the likelihood
# If the data is independent (it is), y is the product


# CREATE BUGS MODEL

mdl <- "
  model {
    # N is length of data from data.jags
   for (i in 1:N) {
    # theta is parameter specified in parms
     y[i] ~ dbern(theta)
   }
    # prior is beta 1,1 (in-informed) 
   theta ~ dbeta(1,1)
  }

"

# Write file to local drive.
writeLines(mdl, 'bern.txt')


bern.sim <- jags(data = data.jags, inits = NULL, parameters.to.save = parms,
                 model.file = 'bern.txt', n.chains = 5, n.iter = 5000, n.burnin = 1000,
                 n.thin = 1)
bern.sim

    ## Inference for Bugs model at "bern.txt", fit using jags,
    ## 5 chains, each with 5000 iterations (first 1000 discarded)
    ## n.sims = 20000 iterations saved
    ## mu.vect sd.vect   2.5%    25%    50%    75%  97.5%  Rhat n.eff
    ## theta      0.417   0.137  0.169  0.318  0.411  0.511  0.692 1.001 10000
    ## deviance  14.330   1.238 13.461 13.549 13.856 14.593 17.799 1.001 14000
    ## 
    ## For each parameter, n.eff is a crude measure of effective sample size,
    ## and Rhat is the potential scale reduction factor (at convergence, Rhat=1).
    ## 
    ## DIC info (using the rule, pD = var(deviance)/2)
    ## pD = 0.8 and DIC = 15.1
    ## DIC is an estimate of expected predictive error (lower deviance is better).

# theta is shown to be estimated: 0.417 with SD: 0.137

# Let's now take apart how Deviance is calculated:
# deviance is a goodness-of-fit statistic for a statistical model (like variance)
  # Deviance: -2*log likelihood () 


sims <- as.mcmc(bern.sim) # Coerce simulation as 

chains <- as.matrix(sims)
head(chains) # deviance and theta.

# bernoulli data likelihood for theta 1
.4729014^4*(1-.4729014)^6
# log of that..
log(.4729014^4*(1-.4729014)^6)
# deviance
-2*log(.4729014^4*(1-.4729014)^6)


# NOW CALCULATE DIC Deviance information Criterion (deviance + penalty)

# Variance of the deviance
var(chains[,1])
# p/D is the penalty (p as the number of parameters)
var(chains[,1])/2
# OR by WINBUGS

# DIC deviance + penalty
mean(chains[,1])+var(chains[,1])/2

# Any Information Criterion has a general form Deviance + Penalty
# Deviance of the theta in Smeaglehalter's formula
mean(chains[,1]) + mean(chains[,1] - (-2*log(mean(chains[,2])^4*(1-mean(chains[,2]))^6))) # 



# HAVE MY CHAINS CONVERGED?

library(coda)



# Rhat :: Are all the chains coming from the same place? Have they converged?
bern.sim$BUGSoutput # Having rhat close to 1 is the best! Looks good.

# Effective Size :: Are the data independent and uncorrelated?
effectiveSize(sims) # 20000 says our draws are independent. Size = Sample size. When the sample is correlated, we don't have as much info
# When we have indepedent, uncorrelated data. If the effective size of the chains are < 5000, you cannot have good posterior analysis.

# Autocorrelation :: How much do the chains correlate on each lagged iteration?
autocorr.diag(sims)  # lag 1 autocorrelation: Looking at draw one's correlation with draw 2. # lag zero doesn't matter. Will be 1 every time!
                     # If it is not close to zero, we need to thin. Closer to 0 is less lagged correlation.

# Raftery Lewis :: How much accuracy do you have in your tail estimates?
sims <- as.mcmc(chains) # Computing raftery by each separate chain.
raftery.diag(sims)

# The raftery lewis diagram is important. Tells you about how much accuracy you have in your tail. Since we are not just look at point estimates
# and also interval estimates, it is nice to know how close we are at the tail. How much is that tail going to move? How good do we feel about
# our intervals? If you are looking at that 0.025 quantile and you want accuracy of 0.005, the probability of that is .95
# The dependence factor .994 is our accuracy. Are my posterior probabilities reasonably accurate? 
# if the dependence of theta is closer to 5, you don't want to use intervals. You can change this by thinning. 



# Extras:

sims <- as.mcmc(bern.sim) #rhat and gelman are the same thing
gelman.diag(sims) # rhat needs to be close to 1!
plot(chains[,2], type = 'l') # trace plot like this is good.
plot(sims) # there are actually 5 colors for each chain. Starts at 1000. If these chains are diferent, you have a problem.
