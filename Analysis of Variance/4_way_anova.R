dt <- "
-0.68, 1, 1
3.77, 1, 1
-0.51, 1, 2
2.14, 1, 2
6.42, 1, 3
1.79, 1, 3
3.06, 1, 4
2.73, 1, 4
2.43, 2, 1
3.28, 2, 1
6.46, 2, 2
2.4, 2, 2
2.84, 2, 3
4.68, 2, 3
3.86, 2, 4
5.72, 2, 4
3.81, 3, 1
0.63, 3, 1
6.48, 3, 2
5.48, 3, 2
8.8, 3, 3
8.88, 3, 3
10.94, 3, 4
9.41, 3, 4
8.64, 4, 1
6.41, 4, 1
10.84, 4, 2
11 4, 2,
7.69, 4, 3
7.29, 4, 3
10.63, 4, 4
12.22, 4, 4
"


dat4x4 <- read.table(text = dt,col.names = c("y", "tmt1", "tmt2"), sep = ",")



library(R2jags)

mdl <- "
model {
  for (i in 1:32){
      response[i] ~ dnorm(mu[tmt1[i],tmt2[i]],prec)
    }
  prec <- 1/vv
  vv ~ dgamma(2,.5)
  for (i in 1:4) {
    for (j in 1:4) {
      mu[i,j] ~ dnorm(5,.0001)
    }
  }
}
"

writeLines(mdl,'anova.txt')

response <- dat4x4[,1]
tmt1 <- dat4x4[,2]
tmt2 <- dat4x4[,3]
data.jags <- c('response','tmt1','tmt2')
parms <- c('mu','vv')

ex4x4.sim <- jags(data=data.jags, inits = NULL, parameters.to.save = parms,
                  model.file = 'ex4x4.txt', n.iter=42000, n.burnin=2000, n.chains=4,
                  n.thin = 4)

