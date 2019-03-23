# Inverse Gamma
# ## SIGMA^2 UNKOWN, MU KNOWN




library(invgamma)

scores <- c(108,88,97,129,91,80,114,118,87,89,106,106,102,112,90,119,100,106,93,97)

# Prior 1 a = 5, b = 900
# Known mean = 100
a <- 5
b <- 900
n <- length(scores)
mu <- 100

starval <- function(a,b,y,mu) {
  n <- length(y)
  astar <- a + (n/2)
  bstar <- b + (.5)*sum((scores-mu)^2)
  c('astar' = astar, 'bstar' = bstar)
}


abstar <- starval(a,b,scores,mu)

plot(0:400,dinvgamma(0:400,abstar[1],abstar[2]), type = 'l')
(post.mean <- abstar[2]/(abstar[1] - 1))
(post.mean <- abstar[2]^2/((abstar[1] - 2)*(abstar[1] - 1)^2))

# Prior 2 a = 50627, b = 11,390,850



summary(rinvgamma(10000,abstar[1],abstar[2]))

qinvgamma(c(0.025,0.975), abstar[1],abstar[2])
