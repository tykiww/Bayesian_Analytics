set.seed(4)
pick <- sample(1:120,80)
train <- vo2[pick,]
test <- vo2[-pick,]
library(R2jags)
mdl <- "
  model  {
   for (i in 1:80){
    y[i] ~ dnorm(mu[i],1/s2e)
    mu[i] <- b0 + bgen*Gender[i] + bmph*MPH[i] + bhr*HR[i]
   }
   b0 ~ dnorm(0,.00001)
   bgen ~ dnorm(0,.00001)
   bmph ~ dnorm(0,.00001)
   bhr ~ dnorm(0,.00001)
   s2e ~ dgamma(4,.1)
}
"
writeLines(mdl,'train.txt')
Gender <- train$Gender
MPH <- train$MPH
HR <- train$HR
y <- train$MaxVO2ML
data.jags <- c('y','Gender','MPH','HR')
parms <- c('b0','bgen','bmph','bhr','s2e')
train.sim <- jags(data=data.jags,parameters.to.save = parms,
                  model.file = 'train.txt',inits = NULL,
                  n.iter = 12000, n.burnin = 2000,n.chains = 4,
                  n.thin = 2)
train.sim
sims <- as.mcmc(train.sim)
chains <- as.matrix(sims)
sims <- as.mcmc(chains)
dim(sims)
effectiveSize(sims)
autocorr.diag(sims)
raftery.diag(sims)
chains[1,]
b0 <- chains[,1]
bgen <- chains[,2]
bhr <- chains[,3]
bmph <- chains[,4]
sde <- sqrt(chains[,6])
test[1,]
ppd102 <- b0 + bgen*0 + bhr*185 + bmph*4.8 + rnorm(20000,0,sde)
plot(density(ppd102,adjust=2))
mean(ppd102)
median(ppd102)
mean(ppd102)-test[1,8]
(mean(ppd102)-test[1,8])^2
ppd <- b0 + outer(bgen,test[,2]) + outer(bhr,test[,6]) + outer(bmph,test[,5])
apply(ppd,2,mean)
sum((apply(ppd,2,mean)-test[,8])^2)
