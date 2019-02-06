##### MONTE CARLO #######


#### Generating Monte Carlo Samples


n <- 10000
vals <- rnorm(n,0,1)
g <- (vals < 1.5 & vals>-0.3)

pnorm(1.5)-pnorm(-.3)
# Estimating  Monte Carlo Error
cs <- cumsum(g)
mcrun <- cs/(1:n)
plot(1:n,mcrun,type = "l")
abline(h = pnorm(1.5)-pnorm(-.3), col = "red")




## Beta Prior (1,1); Posterior (6,5)
qbeta(c(.025,.975), 6,5)
quantile(rbeta(100000,6,5), c(.025,.975))  # use qbeta when you have beta for sure.





#### Millenials wiht landlines vs. Seniors with landlines

# n-y+b

# millenials 
prob_m <- 0.05 # p
a_m <- 1 # alpha
b_m <- 19 # beta
y_m <- 3 # successes
n_m <- 100
theta_1 <- rbeta(1000,a_m + y_m,n_m-y_m+b_m)
hist(theta_1)
plot(density(theta_1))
# Seniors
prob_s <- 0.6
a_s <- 60
b_s <- 40
y_s <- 61
n_s <- 100
theta_2 <- rbeta(1000,a_s + y_s,n_s-y_s+b_s)
hist(theta_2)
plot(density(theta_2))


plot(density(theta_1), xlim = c(0,1))
lines(density(theta_2), col = "red")

# plot the difference
plot(density(theta_1-theta_2), xlim = c(-1,1), col = "blue")
lines(density(theta_1-theta_2), col = "blue")
mean(theta_1-theta_2)
quantile(theta_1-theta_2, c(0.025, 0.975))







