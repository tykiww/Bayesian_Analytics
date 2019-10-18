



data {
  int<lower=0> N; // number of observations
  real y[N]; // response
  real x[N]; // covariate
}
parameters {
  real  beta0; // initialize parameters.
  real beta1;
  real<lower=0> sigma; // lower, upper, range.
}
transformed parameters { // Breaking down
  real mu[N];
  real<lower=0> sigma2;
  sigma2 = sigma^2;
  for (i in 1:N) {
    mu[i] = beta0 + beta1*x[i];
  }
}
model {
  beta0 ~ normal(0,100); // JAGS is mean, precision.
  beta1 ~ normal(0,100);  // Normals are (mean, sd)
  sigma2 ~ gamma(1.5,.5);  // gammas are (shape,rate)
  for ( i in 1:N) {
    y[i] ~ normal(mu[i],sigma);
  }
}

