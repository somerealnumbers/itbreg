data {
  int<lower=0> N;   // the number of observations (or rows in our data frame)
  vector[N] x;      // the explanatory variable
  vector[N] y;      // the dependant variable
}
parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}
model {
  y ~ normal(alpha + beta * x, sigma);
}