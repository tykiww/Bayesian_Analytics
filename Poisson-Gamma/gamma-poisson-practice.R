# Gamma Poisson Distribution


##POISSON##

# More than just a binomial predetermined number of trials (Count data)

# conjugate distribution of the Poisson

# Expected Value:: a/b OR aβ

## Assumptions

# Random variable Y represents the number of occurences for some event o interest in a fixed amount of time/space/etc.

# Theta is a rate parameter (lambda but we will call it theta) represents the expected number of occurences per unit

# Does it seem reasonable that the mean and the variance our equal? Variance is also the expected number, so the bigger our rate parameter, the larger our spread.

# Occurences are assumed to be independent in time/space/etc.

# Occurences are assumed to have a constant underlying rate (theta does not change over time/space/etc)

# probability of two or more occurences within a very small interval is essentially zero (there is some time interval in-between)

# Examples
  # Number of Goals Scored by Chelsea in their next game (you cannot have 2 goals at once, so that is good. Could not be independent)
  # Number of fatalities on utah roadways in october (interval could be month, but doesn't it depend of weather and more people in the car?)
  # Number of pine trees in a number of selected 10x10 plot of the uinta-wasatch-Cache National Forest (this one seems reasonable)
  # Count in 1 minute period from geiger counter at a decomissioned nuclear test site (this one is reasonable)
  # Number of free throws I make out of 10 attempts (predetermined number of trials CANNOT WORK)

# PMF Poisson (support: 0 to infinity integers)
?dpois ; ?ppois ; ?rpois ; ?qpois


theta <- 4
xax <- seq(0,20)
dpois(xax,theta) %>% plot(type = "h")

##GAMMA##

# The prior distribution for theta

# conjugacy and same support

# There are two types of Gamma
  # a, b OR a, β (b = 1/β)
  # a is shape.
  # b is rate (as b increases, expectation goes down)
  # β is scale (as beta increases, expectation goes up)

# Fix rate/scale (as a increases, the mean and spread increases)
xax <- seq(0,100, length.out = 100)
plot(xax,dgamma(xax,637,16), type = "l")

qgamma(c(.025,.975),637,16)

lines(xax,dgamma(xax,1,1), col = "red")
lines(xax,dgamma(xax,2,1), col = "blue")
lines(xax,dgamma(xax,4,1), col = "brown")

# Fix shape (as b increases, the mean and spread decreases)

xax <- seq(0,8, length.out = 100)
plot(xax,dgamma(xax,3,1), type = "l", ylim = c(0,2))
lines(xax,dgamma(xax,3,2), col = "red")
lines(xax,dgamma(xax,3,3), col = "blue")
lines(xax,dgamma(xax,3,4), col = "brown")


### Part 2 ###
# Gamma-Poisson Inference

# Scoring Ability of the 2016 BYU women's soccer team.'


# Y = # of goals scored i na game is poisson
# Prior distribution is the Gamma(shape=8, rate = 3)

# create prior plot. Gamma(9,3)
xax <- seq(0,9, length.out = 100)
plot(xax,dgamma(xax,8,3), type = "l", 
     xlab = expression(pi(theta)),
     ylab = expression(theta))
     
abline(v = 9/3, col = "red") ; abline(v = (9-1)/3, col = "blue") # we are off zero because we should have at least 1.
abline(v = qgamma(c(.025,.975),8,3), col = "gold") # Hard to get 6 because you will have to get 12 or 16 games.
legend(5.8,.41,c("Mean", "Mode", "Quantiles"), c("red","blue","gold"))



# Posterior!! 

# BYU won 2-1 over Washington State. What is my posterior distribution?

# y = 2
y <- 2 ; a <- 8 ; b <- 3
astar <- y + a ; bstar <- b + 1

plot(xax,dgamma(xax,astar,bstar), # Gamma(10,4)
     type = "l", col = "dark gray",
     xlab = expression(pi(theta)),
     ylab = expression(theta), ylim = c(0,.7))
lines(xax,dgamma(xax,9,3), type = "l")


# Next game, BYU lost 0-1 to Nebraska, With this match result, what is my posterior distribution?
ynew <- 0
astar1 <- ynew + astar ; bstar1 <- bstar + 1

lines(xax,dgamma(xax,astar1,bstar1), col = "brown")
legend(5,.7,c("posterior","prior", "new posterior"),c("black","gray", "brown"))


# PRIOR ELICIATION FOR GAMMA-POISSON SETTING
# IF we have little idea what theta might be, it is common to choose both a and b to be very small (a = 1, b = 1 etc.)
# IF we can choose the prior mean, E(theta) = mu, and the prior standard deviation, SD(theta) = sd, then we can solve
  # for a and b using the fact that a Gamma random variable, the mean is mu = (a/b) and sd = (sqrt(a)/b)
# IF we have more than one part of data, each 'b+1' will be greater (b+n) per trial (or game in this situation above).
# THINK OF B as the total number of prior occurences observed ("observed") A as the total number of prior occurences
  # "observed" in the prior units.








# Inference on multiple Observations.
  # what if we see n games? Multiple y?

# Nothing changes in regard to posterior derivation. the Likelihood will change, however.
  # likelihood of 1, to a joint density of all observations. y1 -> yn
# We will have to assume that the observations are conditionally independent and identically distributed.
  # Likelihoods are now a product.
  # y(i)|theta ~(iid) Poi(theta) 


library(dplyr)

total <- c(34,45,23,40,39,66,74,53,22,28,36,39,45,41,51)
sum(total)- sum(females)
females <- c(12,20,13,18,15,32,33,27,11,13,17,20,20,21,23)

c()

637/16





qbeta(c(.025,.975),1,1)



# Posterior Predictive Estimate
theta <- rgamma(100000, 637,16)
ynew <- rpois(100000,theta) # for each value of theta, it creates a y new (essentially a for loop)
ynew>
density(ynew) %>% plot



paste("Based on the priors selected and the data observed, the approximate posterior predictive probability of Ted Williams getting at least two hits out of five after his career is ", round(mean(ynew>=2),4), sep = "")

#### QUIZ 2
a <- 1 ; b <- 1 # For both Prior Beta(50,120)
# Ted Williams 
y1 <- 295 ; n1 <- 636
nReps <- 100000

theta1 <- rbeta(nReps,a + y1, n1 - y1 + b)
# Joe DiMaggio
y2 <- 341 ; n2 <- 636
theta2 <- rbeta(nReps,a + y2, n2 - y2 + b)

paste("Based on the priors we chose and the observed data, the approximate posterior probability of girls noticing more than guys is ", round(mean(theta1>theta2),4), sep = "")


