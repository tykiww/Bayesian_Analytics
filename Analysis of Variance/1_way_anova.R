# One-Way ANOVA

library(R2jags)
mdl <- "
  model {
    for (i in 1:28) {
      response[i] ~ dnorm(mu[tmt[i]], prec)
    }
    prec <- 1/vv
    vv ~ dgamma(2, .1)
    for (i in 1:4) {
      mu[i] ~ dnorm(15, .0001)
    }
  }
"

writeLines(mdl, 'anova.txt')
dat <- read.table('04anova.dat')
tmt <- dat$V1
response <- dat$V2

data.jags <- c('tmt', 'response')
parms <- c('mu', 'vv')

anova.sim <- jags(data = data.jags, inits = NULL, parameters.to.save = parms,
                  model.file = 'anova.txt', n.iter = 42000, n.burnin = 2000,
                  n.chains = 5, n.thin = 8)
```
```{r}
mu1vmu2 <- chains[,2] - chains[,3]
mu1vmu3 <- chains[,2] - chains[,4]
mu1vmu4 <- chains[,2] - chains[,5]
mu2vmu3 <- chains[,3] - chains[,4]
mu2vmu4 <- chains[,3] - chains[,5]
mu3vmu4 <- chains[,4] - chains[,5]

plot(density(mu1vmu2), type = 'l')
plot(density(mu1vmu3), type = 'l')