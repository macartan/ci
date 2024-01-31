data {
  int<lower=0> N; // Total number of observations
  int<lower=0,upper=1> X[N]; // Treatment indicator
  int<lower=1,upper=20> school[N]; // School ID for each observation
  vector[N] Y; // Outcome variable
}
parameters {
  vector<lower=0>[3] sigma; // Standard deviations
  vector[20] a; // School intercepts
  vector[20] b; // School slopes
  real mu_a; // Prior mean for intercepts
  real mu_b; // Prior mean for slopes
}
transformed parameters {
  vector[N] Y_vx; // Expected values
  for (i in 1:N)
    Y_vx[i] = a[school[i]] + b[school[i]] * X[i];
}
model {
  // Priors
  a ~ normal(mu_a, sigma[1]);
  b ~ normal(mu_b, sigma[2]);
  sigma ~ cauchy(0, 2);
  mu_a ~ normal(0, 10);
  mu_b ~ normal(0, 10);
  
  // Likelihood
  Y ~ normal(Y_vx, sigma[3]);
}
