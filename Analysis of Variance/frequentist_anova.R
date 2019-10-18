## Understanding Frequentist ANOVA

library(tidyverse)
# Create Data
set.seed(5)

dat <- bind_cols(
  response = rnorm(24, 7, 3),
  tmt = sort(rep(1:4,6))
)
W <- model.matrix(~ -1 + as.factor(dat$tmt))
y <- dat$response
mu_hat <- solve(t(w)%*%w)%*%t(w)%*%dat$response
# w; y; muhat
y_hat <- W %*% muhat
sse <- t(y - y_hat)%*%(y-y_hat) %>% as.numeric()
s2_hat <- sse/(length(y)-length(mu_hat))

# assumes independence and constant variance per usual, so matrix is 0s (covariance = 0) except sig2 across diagonal
var_mu_hat <- s2_hat*solve(t(W)%*%W) 

dm12 <- cbind(1, -1, 0, 0)
dm12_mu_hat <- dm12%*%mu_hat
var_dm12_mu_hat <- dm12%*%var_mu_hat%*%t(dm12)
var_dm12_mu_hat

dm12_mu_hat / sqrt(var_dm12_mu_hat) # t statistic



A <- cbind(
  rbind(
    rep(1/4, 4), # intercept
    c(rep(1/2, 2), rep(-1/2, 2)), # marginal_a
    rep(c(1/2, -1/2), 2), # marginal_b
    c(1, -1, -1, 1) # interaction
  )
)

Ai <- solve(A)

A; Ai

X <- W %*% Ai
X

beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
beta_hat
X %*% beta_hat # = y_hat
var_beta_hat <- s2_hat * solve(t(X) %*% X) # if we had unequal cell sizes, we would have off-diagonal values (covariances != 0)
var_beta_hat


# t test

t_stat <- (beta_hat[2] - 0) / # estimate minus null value
  sqrt(var_beta_hat[2,2]) # standard error for beta1_hat

# f test statistic: t statistic squared

A_ <- rep(1:2, each = 12)
B_ <- rep(1:2, each = 6, times = 2)

dat_ <- 
  cbind(
    dat,
    A_,
    B_
  )

dat_

mod1 <- lm(y ~ as.factor(A_) * as.factor(B_), data = dat_)
anova(mod1)
summary(mod1) # note betas are different because R uses a different X matrix

mod2 <- lm(y ~ -1 + X)
summary(mod2) # these betas should match

effA <- cbind(0,1,0,0)
effA %*% beta_hat

t_stat_1 <- effA %*% beta_hat / 
  sqrt(effA %*% var_beta_hat %*% t(effA)) # should give same value
t_stat_1

t(effA %*% beta_hat) %*% solve(effA %*% var_beta_hat %*% t(effA) %*% effA %*% beta_hat)

