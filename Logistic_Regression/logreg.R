# Logistic Regression using JAGS

library(tidyverse)
url <- "https://raw.githubusercontent.com/tykiww/projectpage/master/datasets/icu/icudata.csv"
dat <- read_csv(url)

# check for NAs

skimr::skim(dat)

# STA 1 is dead 0 is alive.



#################################################################################################

# Prepare Data
y <- dat$STA

iter <- colnames(dat)[-1]
for (i in iter) assign(i,c(dat[,i])[[1]])
n <- nrow(dat)

data.jags <- c('y','n',iter[-1])
parms <- c('bint','p',paste("b",iter,sep = "")[-1])


# Write Model

library('R2jags')

mdl <- " model  {

                 # Likelihood
                     for (i in 1:n){
                           y[i] ~ dbern(p[i])
                           
			                     logit(p[i]) <- bint + bCPR*CPR[i] + bAGE*AGE[i] + bHRA*HRA[i] + bRACE*RACE[i] + bSEX*SEX[i] + bSYS*SYS[i] + bTYP*TYP[i]
                     }
                           
                 # Priors
                 bint ~ dnorm(0,.1);
                 bCPR ~ dnorm(0,.1);
                 bAGE ~ dnorm(0,.1);
                 bHRA ~ dnorm(0,.1);
                 bRACE ~ dnorm(0,.1);
                 bSEX ~ dnorm(0,.1);
                 bSYS ~ dnorm(0,.1);
                 bTYP ~ dnorm(0,.1);
                }
"
writeLines(mdl,'logistic.txt')

# Run Model

logistic.sim <- jags(data=data.jags,inits=NULL,parameters.to.save=parms,
                     model.file='logistic.txt',n.iter=12000,n.burnin=2000,n.chains=8,
                     n.thin=10)

