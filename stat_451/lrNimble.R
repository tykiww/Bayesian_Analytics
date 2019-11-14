library('nimble')

linregCode <- nimbleCode({
  for (i in 1:n){
    y[i] ~ dnorm(mu[i],1/s2)
    mu[i] <- b0 + b1*x[i]
  }
  b0 ~ dnorm(0,.0001)  # normal is (mean, precision)
  b1 ~ dnorm(0,.0001)
  s2 ~ dgamma(1.5,.5)  # gamma is (shape, rate)
})

regConsts <- list(n=20)
regData <- list(x=x,y=y)
regInits <- NULL

linreg <- nimbleModel(code=linregCode,name='linregEx',data=regData,inits=regInits,constants=regConsts)

linreg$getNodeNames()
plot(linreg$graph)

Clinreg <- compileNimble(linreg,showCompilerOutput = FALSE)

linregConf <- configureMCMC(linreg,print=TRUE,thin=10)

# linregConf$addMonitors(c('b0','b1','s2'))

linregMCMC <- buildMCMC(linregConf)

ClinregMCMC <- compileNimble(linregMCMC,project=linreg,resetFunctions=TRUE)

niter <- 120000
set.seed(0)
ClinregMCMC$run(niter)

chains <- as.matrix(ClinregMCMC$mvSamples)
chains <- chains[2001:12000,]

library(coda)
sims <- as.mcmc(chains)
raftery.diag(sims)
autocorr.diag(sims)
effectiveSize(sims)
