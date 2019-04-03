### Exponential Data
data <- rexp(100,4)

### Conjugate Prior is GAMMA! Priors can be chosen in the same manner
a <- .01
b <- .01

# Plotting Prior
plot(seq(0,1,length = 1001),dgamma(seq(0,1,length = 1001),a,b), type = 'l')

# getting prior
starval <- function(a, b, y) {
  n <- length(y)
  astar <- a + n
  bstar <- b + sum(y)
  c("a*" = astar, "b*" = bstar)
}


abstar <- starval(a,b,data)
# posterior predictive
theta <- rgamma(10000,abstar[1],abstar[2])
ynew <- rexp(10000,theta)
plot(density(ynew))



monty <- function (switch = TRUE) {
  door <- sample(1:3,1,replace = TRUE)
  pick <- sample(1:3,1,replace = TRUE)
  ifelse(switch==TRUE, pick!=door,pick==door)
}


mean(replicate(1000,monty(TRUE )))
mean(replicate(1000,monty(FALSE)))


install.packages("devtools")
devtools::install_version("car", version = "1.16", repos = "http://cran.us.r-project.org")



