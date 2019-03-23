# Gibbs sampler practice


##### STEP 1 #####

# data is assumed iid normal
set.seed(15)
data <- c(runif(15),rnorm(35)) + 400

# Create full-conditional priors for unknown mu
m <- 350 ; v <- 1000
# Create full-conditional priors for unkonwn sig2
a <- 0.1 ; b <- 0.1


##### STEP 2 #####

# Set empy vector for mu, sig2
mu <- c() ; sig2 <- c()

# Set starting values (close to posterior mode) either use ybar and s^2 from data or 0 and 1 then perish
ybar <- mean(data) ; s2 <- sd(data)
mu[1] <- ybar ; sig2[1] <- s2
# mu[1] <- 0 ; sig2[1] <- 1

# For j in J... Find the posterior distributions!

J <- 100000
for (j in 2:J) {
  # Create posterior for unknown mu
  mstar <- ((n * v * ybar) + (m * sig2[j-1])) / (n * v + sig2[j-1])
  vstar <- (v * sig2[j-1]) / (n * v + sig2[j-1])
  mu[j] <- rnorm(1,mstar,vstar)
  # Create posterior for unknown sig^2
  astar <- a + (n/2)
  bstar <- b + sum((data-mu[j])^2)/2
  sig2[j] <- rinvgamma(1,astar,bstar)
}

plot(mu[-1],type = 'l')
plot(sig2[-1],type = 'l')
plot(mu[-1],sig2[-1])



# Credible intervals for posterior values! 
quantile(mu[-1],c(.025,.5,.975))
quantile(sig2[-1],c(.025,.5,.975))


# Posterior predictive

ynew <- rnorm(J-1,mu[-1],sig2[-1])
mean(ynew > 400) # probability that x > 450
