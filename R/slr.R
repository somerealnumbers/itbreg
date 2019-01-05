# required packages

library(rjags)
library(tidyverse)

# data

auto_df <- read_csv("http://www-bcf.usc.edu/~gareth/ISL/Auto.csv")

# eda

plot(auto_df$weight, auto_df$acceleration)

# simple linear model y ~ X

slr_string = "model {
	
	# normal likelihood
	
	for (i in 1:N) {
		acceleration[i] ~ dnorm(beta0 + beta1* weight[i], 1/sigma^2)
	}
	
	# priors
	
	beta0 ~ dnorm(0, 1/1000^2)
	beta1 ~ dnorm(0, 1/1000^2)
	
	log_sigma ~ dunif(-10, 10)
	sigma <- exp(log_sigma)
}"

# set random seed

set.seed(100)

# data for jags

dat <- list(acceleration = auto_df$acceleration
, N = nrow(auto_df)
, weight = auto_df$weight)

# run

slr <- jags.model(textConnection(slr_string)
, data = dat
, n.chains = 1)

# burn in

update(slr, 20000)

# samples

slr_sim <- coda.samples(model = slr
, variable.names = c("beta0", "beta1", "sigma")
, n.iter = 30000)

# plot

plot(slr_sim)

# summary

summary(slr_sim)

# compare with OLS

ols_slr <- lm("acceleration ~ weight", data = auto_df)

summary(ols_slr)