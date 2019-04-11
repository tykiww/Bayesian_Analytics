
# Metropolis Algorithm! (takes the handcuffs off of conjugacy)

# Target distribution
# f(x) = c exp(-x^6) (1 + |2x|)^3 DOES NOT INTEGRATE TO 1
# Proposal distribution (Normal)

set.seed(200)
# starting value
x <- c(1) %>% as.numeric
vv <- 1 # Need Tau somewhere between too big and too small to fix autocorrelation.
# tau is how big of a step we take to get candidate values from f(x)
# If you don't have any flat zones (metropolis skyline) too much. Some flat some not traceplots, we are good!

# For loop
J <- 10000
j <- 2
for (j in 2:J) {
  # sample xp from the proposal distribution
  xp <- rnorm(1,x[j-1],vv)
  # compute the probability of accepting the proposed value of a
  
  
  
  gxp <- exp(-xp^6)*(1 + abs(2*xp))^3
  gxc <- exp(-x[j-1]^6)*(1 + abs(2*x[j-1]))^3
  alpha <- min(c(1,gxp/gxc))
  
  # can do a bernoulli or a uniform value between 0 and 1
  (rbinom(1, 1, alpha) == 1) %>% 
    ifelse( xp, x[j-1]) -> x[j]
  # tells us if we rejected or accepted
  
} 

# trace plot
plot(x, type = 'l') # Look at a subset (100 or so) at the beginning.
acf(x) # highly correlated draws. Tau too small is moving to slow.


plot(density(x)) # normalized one


#################

# f(x) is target
# g(x) is what we can evaluate (proportional to f(x)).

# f(x) is proportional to... Likelihood*prior!
# Therefore g(x) = likelihood*prior



## In a case where it is irrelevant.


# Our data is y1...yn iid~ N(µ,sig2), sig2 is known!
# Prior is µ ~ N(m,v)
# Posterior is π(µ|sig2,y1,..yn) ~N(m*,v*) (THIS IS OUR F)




















