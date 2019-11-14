setwd("C:/Users/tykwad1/Downloads")

V1 <- c(30,20,60,80,40,50,60,30,70,60)
V2 <- c(73,50,128,170,87,108,135,69,148,132)
linreg<- data.frame(cbind(V1,V2))



x <- linreg$V1
y <- linreg$V2
N <- length(y)
linreg_dat <- list(N=N,y=y,x=x)
plot(x,y)



library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores()-1)
fit <- stan(file = 'linregmodel.stan', data = linreg_dat, 
            iter = 3500, warmup=1000, chains = 4) # burnin = warmup
traceplot(fit,'beta0')
samps <- extract(fit)
names(samps)
chains <- cbind(samps[[1]],samps[[2]],samps[[5]])
colnames(chains) <- c('b0','b1','s2')
library(coda)
sims <- as.mcmc(chains)
raftery.diag(sims)
autocorr.diag(sims)
effectiveSize(sims)
# Stan is compiled.
# compile step takes a long time


# Predicting using chains.

ypred <- mean(chains[,1]) + mean(chains[,2])*x
sqrt(mean((ypred - y)^2)) # MSE





