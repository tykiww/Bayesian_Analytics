
# Time to failure Poisson Models!
# 7 total


pumps <- read.table(text = "
time fail
94.5 5
15.7 1
62.9 5
126 14
5.24 3
31.4 19
1.05 1
1.05 1
2.1 4
10.5 22
", header = TRUE)


library(R2jags)
time <- pumps$time
fail <- pumps$fail
mdl <- "
 model {
    for (i in 1:10){
       fail[i] ~ dpois(lambda[i])
       lambda[i] <- theta*time[i]
    }
       theta ~ dgamma(1.1,1)
 }
"
writeLines(mdl,'pumps1.txt')
data.jag <- c('fail','time')
parms <- c('lambda','theta')
pumps1.sim <- jags(data=data.jag,parameters.to.save = parms,
                   inits = NULL, model.file = 'pumps1.txt',
                   n.iter = 15000, n.burnin = 5000, n.chains = 4,
                   n.thin = 1)

pumps1.sim$BUGSoutput


# 

time <- pumps$time
fail <- pumps$fail
mdl <- "
model {
for (i in 1:10){
fail[i] ~ dpois(lambda[i])
lambda[i] <- alpha + theta*time[i]
}
theta ~ dgamma(1.1,1)
alpha ~ dnorm(0,.0001)
}
"
writeLines(mdl,'pumps2.txt')
data.jag <- c('fail','time')
parms <- c('lambda','theta','alpha')
pumps2.sim <- jags(data=data.jag,parameters.to.save = parms,
                   inits = NULL, model.file = 'pumps2.txt',
                   n.iter = 15000, n.burnin = 5000, n.chains = 4,
                   n.thin = 1)

#

time <- pumps$time
fail <- pumps$fail
mdl <- "
model {
for (i in 1:10){
fail[i] ~ dpois(lambda[i])
lambda[i] <- theta*time[i]
}
theta ~ dgamma(a,b)
a ~ dexp(1)
b ~ dgamma(1.1,10)
}
"
writeLines(mdl,'pumps3.txt')
data.jag <- c('fail','time')
parms <- c('lambda','theta','a','b')
pumps3.sim <- jags(data=data.jag,parameters.to.save = parms,
                   inits = NULL, model.file = 'pumps3.txt',
                   n.iter = 15000, n.burnin = 5000, n.chains = 4,
                   n.thin = 1)

#

time <- pumps$time
fail <- pumps$fail
mdl <- "
model {
for (i in 1:10){
fail[i] ~ dpois(lambda[i])
lambda[i] <- alpha + theta*time[i]
}
alpha ~ dnorm(0,.0001)
theta ~ dgamma(a,b)
a ~ dexp(1)
b ~ dgamma(1.1,1)
}
"
writeLines(mdl,'pumps4.txt')
data.jag <- c('fail','time')
parms <- c('lambda','theta','a','b','alpha')
pumps4.sim <- jags(data=data.jag,parameters.to.save = parms,
                   inits = NULL, model.file = 'pumps4.txt',
                   n.iter = 15000, n.burnin = 5000, n.chains = 4,
                   n.thin = 1)


# 
time <- pumps$time
fail <- pumps$fail
mdl <- "
 model {
    for (i in 1:10){
       fail[i] ~ dpois(lambda[i])
       lambda[i] ~ dgamma(1.1,.1)
    }
 }
"
writeLines(mdl,'pumps5.txt')
data.jag <- c('fail')
parms <- c('lambda')
pumps5.sim <- jags(data=data.jag,parameters.to.save = parms,
                   inits = NULL, model.file = 'pumps5.txt',
                   n.iter = 15000, n.burnin = 5000, n.chains = 4,
                   n.thin = 1)


# 

mdl <- "

model {    
for (i in 1:10){
theta[i] ~ dgamma(alpha,beta);
fail[i] ~ dpois(lambda[i]);
lambda[i] <- theta[i]*time[i];
}	

alpha ~ dexp(.1);
beta ~ dgamma(5,.5);

}
"


writeLines(mdl,'pumps6.txt')


time <- pumps[,1]
fail <- pumps[,2]

data.jags <- c('time','fail')
parms <- c('theta','alpha','beta','lambda')

pumps6.sim <- jags(data=data.jags,inits=NULL,parameters.to.save=parms,
                   model.file='pumps6.txt',jags.seed=123,
                   n.iter=11000,n.burnin=1000,n.chains=2,n.thin=1)


# 



mdl <- "

model {    
for (i in 1:10){
theta[i] ~ dgamma(alpha,beta);
fail[i] ~ dpois(lambda[i]);
lambda[i] <- theta[i]*time[i];
}	

newfail ~ dpois(lambda[11])
lambda[11] <- newtheta*newtime

alpha ~ dexp(.1);
beta ~ dgamma(5,.5);
newtime ~ dgamma(4,.1)
newtheta ~ dgamma(alpha,beta)
}
"


writeLines(mdl,'pumps7.txt')


time <- pumps[,1]
fail <- pumps[,2]
newfail <- 10
data.jags <- c('time','fail','newfail')
parms <- c('theta','alpha','beta','lambda','newtime','newtheta')

pumps7.sim <- jags(data=data.jags,inits=NULL,parameters.to.save=parms,
                   model.file='pumps7.txt',jags.seed=123,
                   n.iter=11000,n.burnin=1000,n.chains=2,n.thin=1)


