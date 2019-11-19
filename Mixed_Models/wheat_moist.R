url <- "https://raw.githubusercontent.com/tykiww/projectpage/master/datasets/451/moisture.dat"
wheat <- read_csv(url)
library(R2jags)
mdl <- '
  model {
    for (i in 1:60){
      yield[i] ~ dnorm(mu[i],1/vv)
      mu[i] <- b0 + b1*moisture[i]
    }
    b0 ~ dnorm(30,.00001)
    b1 ~ dnorm(0,.00001)
    vv ~ dgamma(2,.25)
  } 
'
writeLines(mdl,'linreg.txt')
yield <- wheat$yield
moisture <- wheat$moisture
data.jags <- c('yield','moisture')
parms <- c('b0','b1','vv')
linreg.sim <- jags(data = data.jags,parameters.to.save = parms,
                   model.file = 'linreg.txt',
                   inits = NULL, n.iter = 12000, n.burnin = 2000, 
                   n.thin=1,n.chains=1)

mdl <- '
  model {
for (i in 1:60){
yield[i] ~ dnorm(mu[i],1/vv)
mu[i] <- b0[variety[i]] + b1*moisture[i]
}
for (i in 1:10){
b0[i] ~ dnorm(30,.00001) }
b1 ~ dnorm(0,.00001)
vv ~ dgamma(2,.25)
} 
'
writeLines(mdl,'randint.txt')
yield <- wheat$yield
moisture <- wheat$moisture
variety <- wheat$variety
data.jags <- c('yield','moisture','variety')
parms <- c('b0','b1','vv')
randint.sim <- jags(data = data.jags,parameters.to.save = parms,model.file = 'randint.txt',
                   inits = NULL, n.iter = 12000, n.burnin = 2000, n.thin=1,n.chains=1)

mdl <- '
  model {
for (i in 1:60){
yield[i] ~ dnorm(mu[i],1/vv)
mu[i] <- b0[variety[i]] + b1*moisture[i]
}
for (i in 1:10){
b0[i] ~ dnorm(mub0,1/vvint) }
mub0 ~ dnorm(30,.000001)
vvint ~ dgamma(4,.25)
b1 ~ dnorm(0,.00001)
vv ~ dgamma(2,.25)
} 
'
writeLines(mdl,'hierint.txt')
yield <- wheat$yield
moisture <- wheat$moisture
variety <- wheat$variety
data.jags <- c('yield','moisture','variety')
parms <- c('b0','b1','vv','mub0','vvint')
hierint.sim <- jags(data = data.jags,parameters.to.save = parms,model.file = 'hierint.txt',
                    inits = NULL, n.iter = 12000, n.burnin = 2000, n.thin=1,n.chains=1)

mdl <- '
  model {
for (i in 1:60){
yield[i] ~ dnorm(mu[i],1/vv)
mu[i] <- b0[variety[i]] + b1[variety[i]]*moisture[i]
}
for (i in 1:10){
b0[i] ~ dnorm(mub0,1/vvint)
b1[i] ~dnorm(mub1,1/vvslp)
}
mub0 ~ dnorm(30,.000001)
vvint ~ dgamma(4,.25)
mub1 ~ dnorm(0,.00001)
vvslp ~ dgamma(1.1,.5)
vv ~ dgamma(1.1,.5)
} 
'
writeLines(mdl,'hier.txt')
yield <- wheat$yield
moisture <- wheat$moisture
variety <- wheat$variety
data.jags <- c('yield','moisture','variety')
parms <- c('b0','b1','vv','mub0','vvint','mub1','vvslp')
hier.sim <- jags(data = data.jags,parameters.to.save = parms,model.file = 'hier.txt',
                    inits = NULL, n.iter = 12000, n.burnin = 2000, n.thin=1,n.chains=1)
