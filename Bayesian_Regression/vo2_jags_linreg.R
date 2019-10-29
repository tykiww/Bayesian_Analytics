# VO2 Running Dataset

url <- "https://raw.githubusercontent.com/tykiww/projectpage/master/datasets/vo2/vo2.dat"
v_dat <- read.table(url, header = TRUE)

plot(MaxVO2ML~.,data = v_dat)

hist(v_dat$MaxVO2ML) # Looks Normal, can use Normal Likelihood
# JAGS Regression using MaxVO2ML as predictor

# BMI, HR, mpH

# RJags
library(rjags)

Y <- v_dat$MaxVO2ML

BMI <- v_dat$BMI
HR <- v_dat$HR
MPH <- v_dat$MPH


sBMI <- (v_dat$BMI - mean(v_dat$BMI))/sd(v_dat$BMI) # standardization helps with understanding relative coefficient effect.
sHR <- (v_dat$HR - mean(v_dat$HR))/sd(v_dat$HR)
sMPH <- (v_dat$MPH - mean(v_dat$MPH))/sd(v_dat$MPH)
n <- nrow(v_dat)

model_string <- "model{

  # Likelihood
  for(i in 1:n){
    Y[i]   ~ dnorm(mu[i],1/vv)
    mu[i] <- beta[1] + beta[2]*BMI[i] + beta[3]*HR[i] + beta[4]*MPH[i]
  }

  # Prior for beta
  for(j in 1:4){
    beta[j] ~ dnorm(0,0.0001)
  }

  # Prior for the inverse variance
  vv   ~ dgamma(0.01, 0.01)
  sigma   <- 1/sqrt(vv)

}"

model <- jags.model(textConnection(model_string), 
                    data = list(Y=Y,n=n,BMI=BMI,HR=HR, MPH=MPH))

update(model, 10000, progress.bar="none"); # Burnin for 10000 samples

samp <- coda.samples(model, 
                     variable.names=c("beta","vv"), 
                     n.iter=20000, progress.bar="none")

summary(samp)

# Regular Regression comparison
summary(lm(MaxVO2ML ~ BMI +HR + MPH,data = v_dat))



# R2 Jags
library(R2jags)
# Remember you put in precisions and not variances in jags

mdl <- "
  model {
    for (i in 1:n) {
        Y[i]   ~ dnorm(mu[i],1/vv)
        mu[i] <- b0 + b1*BMI[i] + b2*HR[i] + b3*MPH[i]
    }
  
  # Prior for the Inverse variance
  
  vv ~ dgamma(0.01, 0.01)
  b0 ~ dnorm(0, .0001)
  b1 ~ dnorm(0, .0001)
  b2 ~ dnorm(0, .0001)
  b3 ~ dnorm(0, .0001)
  
  }
"


writeLines(mdl, "mult_lin_reg.txt")


data.jags <- c('Y','BMI','HR','MPH', 'n')
parms <- c('b0', 'b1', 'b2', 'b3', 'vv')

mult_lin_reg.sim <- jags(data = data.jags, inits = NULL, parameters.to.save = parms, model.file = 'mult_lin_reg.txt', n.iter = 42000, n.burnin = 2000,
                         n.chains = 4, n.thin = 4)


# WE WANT GENDER IN THE MODEL


# Gender, MPH, HR, BMI..

mdl2 <- "
  model {
    for (i in 1:n) {
        Y[i]   ~ dnorm(mu[i],1/vv)
        mu[i] <- b0 + b1*Gender[i] + b2*MPH[i] + b3*HR[i] + b4*BMI[i]
    }
  
  # Prior for the Inverse variance
  
  vv ~ dgamma(0.01, 0.01)
  b0 ~ dnorm(0, .0001)
  b1 ~ dnorm(0, .0001)
  b2 ~ dnorm(0, .0001)
  b3 ~ dnorm(0, .0001)
  b4 ~ dnorm(0, .0001)  
  }
"

Gender <- v_dat$Gender

writeLines(mdl2, "mult_lin_reg.txt")


data.jags <- c('Y','Gender','BMI','HR','MPH', 'n')
parms <- c('b0', 'b1', 'b2', 'b3','b4', 'vv')

mult_lin_reg.sim <- jags(data = data.jags, inits = NULL, parameters.to.save = parms, model.file = 'mult_lin_reg.txt', n.iter = 42000, n.burnin = 2000,
                         n.chains = 4, n.thin = 4)
mult_lin_reg.sim 
