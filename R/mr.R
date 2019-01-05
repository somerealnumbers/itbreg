# required packages

library(rjags)
library(tidyverse)

# data

auto_raw_df <- read_csv("http://www-bcf.usc.edu/~gareth/ISL/Auto.csv")

# eda

auto_df <- auto_raw_df %>% select(-name, -horsepower, -origin, -cylinders, -year, -displacement) 

auto_df %>%
	pairs()

# multi-variate linear model y ~ f(X)

mr_string = "model {
	
	# normal likelihood
	
	for (i in 1:N) {
		mpg[i] ~ dnorm(beta[1] + beta[2]*weight[i] + beta[3]*acceleration[i], 1/sigma^2)
	}
	
	# priors
	
	for (i in 1:3) {
		beta[i] ~ dnorm(0, 1/1000^2)
	}
	
	log_sigma ~ dunif(-10, 10)
	sigma <- exp(log_sigma)
}"

# set random seed

set.seed(100)

# data for jags

dat <- list(mpg = auto_df$mpg
, N = nrow(auto_df)
, weight = auto_df$weight
, acceleration = auto_df$acceleration)

# run

mr <- jags.model(textConnection(mr_string)
, data = dat
, n.chains = 3)

# burn in

update(mr, 20000)

# samples

mr_sim <- coda.samples(model = mr
, variable.names = "beta"
, n.iter = 30000)

# plot

plot(mr_sim)

# summary

summary(mr_sim)

# compare with OLS

ols_mr <- lm("mpg ~ weight + acceleration", data = auto_df)

summary(ols_mr)