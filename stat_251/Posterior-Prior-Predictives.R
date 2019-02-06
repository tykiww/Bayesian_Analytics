



# creating predictive distributions

pred.dist <- function(ynew, nnew, yobs, nobs, a, b, type = "post") {
  
  astar <- yobs + a
  bstar <- nobs - yobs + b
  
  
  # gamma on the log scale (log posterior predictive)
  lgamma(astar + bstar) - lgamma(astar) - lgamma(bstar) + 
    lchoose(nnew,ynew) + lgamma(astar + ynew) + 
    lgamma(bstar + nnew - ynew) - 
    lgamma(astar + bstar + nnew) -> lpop
  
  # gamma on the log scale (log prior predictive)
  lgamma(a + b) - lgamma(a) - lgamma(b) + 
    lchoose(nnew,ynew) + lgamma(a + ynew) + 
    lgamma(b + nnew - ynew) - 
    lgamma(a + b +nnew) -> lprp
  
  # why don't you work? ifelse(type=="post", exp(lpop),exp(lprp))
  
  if (type =="post") {
    exp(lpop)
  } else {
    exp(lprp)
  }
  

}


# returning to Steph Curry's ability to make free throws in NBA basketball games this season. 
# We'll only use data from 2016-2017. We will look at the old data.

# For prior-predictive, we will ignore the data from first 15 games.


# Prior: beta(90,10) strong prior belief that theta is close to 0.9
# data yobs = 71, nobs = 77
# Posterior distribution(161,16)
# nobs is the number of free throws that curry attempted in those 15 games
# theta, the parameters of interst, represnts the probability he makes any given free throw attempts.



nobs <- 77; yobs <- 71; a <- 90; b <- 10; 
nnew <- 19; ynew <- 0 # for now, we are putting ynew at 0 because he hasn't done anything

pred.dist(ynew, nnew, yobs, nobs, a, b, type = "post") # basically 0. Posterior is smaller than prior.
pred.dist(ynew, nnew, yobs, nobs, a, b, type = "prior")
# So, now we need to do this for all possible values.


out.post <- pred.dist(0:19, nnew, yobs, nobs, a, b, type = "post")
plot(0:19,out,type = 'h', col = "steel blue") # maxed out at 18 free throws out of 19 for the posterior predictive distributions, 
# for predicting new observations and model fit.
out.prior <- pred.dist(0:19, nnew, yobs, nobs, a, b, type = "prior")
lines(0:19 + .1, out.prior, type = 'h', col = "maroon") # more mass is being pushed to the tail
# With this scenario, we would conclude that we have a good fitting model.





# Let's now create a MONTE CARLO approximation for predictive distribution

# get a large sample of theta values from the posterior distribution. Denote the jth value in the sample by theta (j)
# for each theta (j), sample a value for y(new) from teh conditional distribution of y

# generate values of theta (thousand draws from the posterior distribution)
theta <- rbeta(1000, yobs + a, nobs - yobs + b)
ynew <- rbinom(1000,nnew,theta) # for each value of theta, it creates a y new (essentially a for loop)

table(ynew)/1000


lines(12:19 - 0.1, table(ynew)/1000, type = 'h', col = 'forest green') # posterior predictive/ monte carlo estimate
# If we take enough draws, it will be exactly the same as the 


# prior predictive
theta <- rbeta(1000, a,b)
ynew <- rbinom(1000,nnew,theta) # for each value of theta, it creates a y new (essentially a for loop)
table(ynew)/1000

lines(11:19 - 0.2, table(ynew)/1000, type = 'h', col = "brown")

