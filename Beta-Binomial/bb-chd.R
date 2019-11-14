library('R2jags')

mdl <- "
 model  {
      for (i in 1:8){
        CHD[i] ~ dbin(p[i],nRisk[i]);
			  p[i] ~ dbeta(1,5)
             }
  }

"
writeLines(mdl,'ANOVAchd.txt')
CHD <- chd[,1]
nRisk <- chd[,2]
tmt <- chd[,6]
data.jags <- c('CHD','nRisk')
parms <- c('p')
ANOVAchd.sim <- jags(data=data.jags,inits=NULL,
                     parameters.to.save=parms,
                     model.file='ANOVAchd.txt',
                     n.iter=12000,n.burnin=2000,n.chains=12,
                     n.thin=10)

sims <- as.mcmc(ANOVAchd.sim)
chains <- as.matrix(sims)
sims <- as.mcmc(chains)
raftery.diag(sims)
effectiveSize(sims)
autocorr.diag(sims)
plot(sims)
chains[1,]
p1 <- chains[,2]
p2 <- chains[,3]
p3 <- chains[,4]
p4 <- chains[,5]
p5 <- chains[,6]
p6 <- chains[,7]
p7 <- chains[,8]
p8 <- chains[,9]
plot(p1,type='l')
plot(p2,type='l')
plot(p3,type='l')
plot(p4,type='l')
plot(p5,type='l')
plot(p6,type='l')
plot(p7,type='l')
plot(p8,type='l')
cat <- (-1/4)*(p1+p2+p3+p4)+(1/4)*(p5+p6+p7+p8)
ecg <- (-1/4)*(p1+p2+p5+p6)+(1/4)*(p3+p4+p7+p8)
age <- (-1/4)*(p1+p3+p5+p7)+(1/4)*(p2+p4+p6+p8)
catxecg <- p1+p2-p3-p4-p5-p6+p7+p8
catxage <- p1-p2+p3-p4-p5+p6-p7+p8
ecgxage <- p1-p2-p3+p4+p5-p6-p7+p8
catxecgxage <- -p1+p2+p3-p4+p5-p6-p7+p8
plot(density(catxecgxage))
plot(density(ecgxage))
plot(density(catxage))
plot(density(catxecg))
plot(density(cat))
mean(cat)
quantile(cat,c(.025,.975))
mean(cat>0)
plot(density(ecg))
mean(ecg>0)
plot(density(age))
mean(age>0)
