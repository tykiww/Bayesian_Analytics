library(tidyverse)
url <- "https://raw.githubusercontent.com/tykiww/projectpage/master/datasets/451/07mixedmods.dat"
bond <- read_csv(url)
library(R2jags)
mdl <- "
model  {
  for (i in 1:21){
  pressure[i] ~ dnorm(mu[i],1/s2error)
  mu[i] <- gamma[metn[i]]  + u[ingot[i]]
}
for (i in 1:3){
  gamma[i] ~ dnorm(75,.001)
}
for (i in 1:7){
  u[i] ~ dnorm(0,1/s2ing)
}
s2error ~ dgamma(1.5,.1)
s2ing ~ dgamma(1.5,.1)
}
"

writeLines(mdl,'mixedmods.txt')

metn <- bond[,4]
ingot <- bond[,1]
pressure <- bond[,3]

data.jags <- c('metn','ingot','pressure')
parms <- c('gamma','s2error','s2ing')


mixedmods.sim <- jags(data=data.jags,inits=NULL,
     parameters.to.save=parms,
	   model.file='mixedmods.txt',n.iter=12000,
     n.burnin=2000,n.chains=5,n.thin=5)
