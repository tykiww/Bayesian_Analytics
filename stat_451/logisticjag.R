library('R2jags')

mdl <- "
 model  {
      for (i in 1:8){
        CHD[i] ~ dbin(p[i],nRisk[i]);
			  logit(p[i]) <- b[tmt[i]];
			  b[i] ~ dnorm(-2,.1);				  
             }
  }

"
writeLines(mdl,'logistic.txt')
CHD <- chd[,1]
nRisk <- chd[,2]
tmt <- chd[,6]
data.jags <- c('CHD','nRisk','tmt')
parms <- c('b','p')
logistic.sim <- jags(data=data.jags,inits=NULL,parameters.to.save=parms,
	   model.file='logistic.txt',n.iter=12000,n.burnin=2000,n.chains=8,
	   n.thin=10)


