data {
  int<lower=0> N;
  vector[N] Y;
  vector[N] X;
}
parameters {
  real a;
  real b;
  real<lower=0> sigma;
}
model {
  Y ~ normal(a + b * X, sigma);
}