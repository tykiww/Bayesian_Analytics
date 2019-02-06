

# Christensen et al. (2011, pp. 22–27) gave a hypothetical example for inference on θ = proportion of U.S. 
# transportation workers who are on the influence of drugs while working. I am amending this example (though 
# I heartily recommend the original to you). Suppose that my prior is θ ∼ Beta(1.4, 23.6).


# Prior Elicitation

# Method 1, just use a uniform distribution! a = 1, b = 1
# Method 2, make an inference (like above) that succeeding is 16x less likely than failing.
# Method 3, moments. Use function below.

get.beta.ab <- function(mu, sigma2){
  a <- (mu^2-mu^3-mu*sigma2)/sigma2
  b <- (a-a*mu)/mu
  if (is.nan(a) | is.na(a) | a<=0 |
      b<=0 | is.na(b) | is.nan(b)) {
    print("New choice for mu and sigma^2 required")
  } else return(list(a=a, b=b))
}
get.beta.ab(0.75, 0.01)
var(data)

## 1. Graph this prior distribution in R. (use dbeta)
a <- 1.4
b <- 23.6
xx <- seq(0,1,length.out = 1000)
prior <-dbeta(xx,a,b)
plot(xx,prior,type="l", main="Prior vs Posterior Distribution",xlab=expression(theta), 
     ylab= expression(paste(pi, "(", theta, ")", sep="")))
## 2. What is the (prior) mean of θ according to this prior?
a/(a + b)
mean(rbeta(100,a,b))
## 3. What is the maximum a priori estimate of θ? (prior mode)
(a-1)/(a+b-2)
## 4. What is a 95% credible interval for the prior value of θ? (use
##    qbeta twice to get the 2.5th and 97.5th percentiles)
c("lower" = qbeta(.025, a, b), "upper" = qbeta(.975, a, b))
## 5. Suppose that a sample of workers revealed the following:
## What is the posterior distribution for θ|y?
data <- c(0,0,0,1,0,1,0,0,0,0,1,1)
y <- 4
n <- length(data)
astar <- a + y
bstar <- n-y + b

posterior <-dbeta(xx,astar,bstar)
lines(xx,posterior,type="l", main="Beta(1.4, 23.6) Posterior Distribution",xlab=expression(theta), 
     ylab= expression(paste(pi, "(", theta, ")", sep="")), col = "red")
## 6. What is the posterior mean for θ|y?
(astar/(astar+bstar))
mean(rbeta(1000,astar,bstar))
## 7. What is the maximum a posteriori estimate of θ|y? (posterior mode)
(astar-1)/(astar+bstar-2)
## 8. What is a 95% credible interval for θ|y? (use qbeta twice)
c("lower" = qbeta(.025, astar, bstar), "upper" = qbeta(.975, astar, bstar))
## 9. How well does the posterior agree with the prior?
# Not bad.










############### HOME FIELD ADVANTAGE ###########################

# The home-field advantage refers to the tendency for teams to perform better at their home 
# court/field/stadium/pitch/track etc. than anywhere else


# 1. Come up with a prior on theta and justify your choice.
    # I will justify that close to 3/4 of the time, home teams seem to win.
    # accordingly, the mean will be 3/4 and I expect that to vary by 20% (95% of the time).
a <- get.beta.ab(0.75, 0.01)$a
b <- get.beta.ab(0.75, 0.01)$b
    # graph the prior.
xx <- seq(0,1,length.out = 1000)
prior <-dbeta(xx,a,b)
plot(xx,prior,type="l", main="Prior vs Posterior Distribution",xlab=expression(theta), 
     ylab= expression(paste(pi, "(", theta, ")", sep="")), ylim = c(0,15))
# 2. Data Given, now update
wins <- 157
losses <- 104
data <- sample(c(rep(1,wins),rep(0,losses)),wins+losses,replace = FALSE)

y <- wins
n <- length(data)
astar <- a + y
bstar <- n-y + b

# Give new posterior distribution
xx <- seq(0,1,length.out = 1000)
posterior <-dbeta(xx,astar,bstar)
lines(xx,posterior,type="l", main="Beta(1.4, 23.6) Posterior Distribution",xlab=expression(theta), 
      ylab= expression(paste(pi, "(", theta, ")", sep="")), col = "red")
mean(posterior>.5)
mean(rbeta(1000,astar,bstar))







