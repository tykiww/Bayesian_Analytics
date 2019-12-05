url <- "https://raw.githubusercontent.com/tykiww/projectpage/master/datasets/weightlifting/weightsmult.txt"
weightnew <- read.table(url)




# Hierarchical model

mdl <- "

model  {        
            for (i in 1:399){
                 response[i] ~ dnorm(mu[i],prec)
	           mu[i] <- b0[ntmt[i],subject[i]] + b1[ntmt[i],subject[i]]*timez[i] 				  
                 }

          for (i in 1:3) {
	      for (j in 1:maxn[i]){
              b0[i,j] ~ dnorm(mu0[i],precint);
	      b1[i,j] ~ dnorm(mu1[i],precslp);
                                  }
	     mu0[i] ~ dnorm(70,.001)
	     mu1[i] ~ dnorm(0,.001)
	     }
	     
                s2 ~ dgamma(1.1,.1);
                prec <- 1/s2;
		s2int ~ dgamma(1.1,.1);
		precint <- 1/s2int
		s2slp ~ dgamma(1.1,1)
		precslp <- 1/s2slp
         }
"
writeLines(mdl,'hiermod.txt')

maxn <- NULL             
maxn[1] <- 20
maxn[2] <- 16
maxn[3] <- 21
response <- weightnew[,4]
timez <- weightnew[,6]
nsub <- weightnew[,7]
ntmt <- weightnew[,8]
subject <- weightnew[,1]

data.jags <- c('response','ntmt','timez','subject','maxn')
parms <- c('mu0','mu1','s2','s2int','s2slp')

hier.sim <- jags(data=data.jags,inits=NULL,parameters.to.save=parms,
                  model.file='hiermod.txt',
                   n.iter=12000,n.burnin=2000,n.chains=1,n.thin=1)


# Fixed Model


library(R2jags)

mdl <- "

model  {        
            for (i in 1:399){
                 response[i] ~ dnorm(mu[i],prec)
	           mu[i] <- b0[ntmt[i]] + b1[ntmt[i]]*timez[i] 				  
                 }

          for (i in 1:3) {
            b0[i] ~ dnorm(60,.001);
	      b1[i] ~ dnorm(0,.01);
             }
                s2 ~ dgamma(1.1,.1);
                prec <- 1/s2;
         }
"
writeLines(mdl,'fixed.txt')

data.jags <- c('response','ntmt','timez')
parms <- c('b0','b1','s2')

fixed.sim <- jags(data=data.jags,inits=NULL,parameters.to.save=parms,
                  model.file='fixed.txt',
                  n.iter=12000,n.burnin=2000,n.chains=1,n.thin=1)


