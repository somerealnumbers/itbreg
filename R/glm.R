# required packages

library(rjags)
library(tidyverse)

# data

college_raw_df <- read_csv("http://www-bcf.usc.edu/~gareth/ISL/College.csv")

# cleaning

college_df <- college_raw_df %>% select(Private, Apps, Accept, Expend) %>%
mutate(Accept.Rate = (Accept/Apps)*100) %>%
mutate_at(vars(Private), funs(if_else(Private == "Yes", 1, 0))) %>%
select(-Apps, -Accept) 

# eda

#college_df %>%
#	pairs()

# logistic regression

lr_string = "model {
	
	# logit link function
	
	for (i in 1:N) {
		Private[i] ~ dbern(p[i])
		logit(p[i]) = beta[1] + beta[2]*Accept.Rate[i] + beta[3]*Expend[i]
	}
	
	# priors
	
	for (i in 1:3) {
		beta[i] ~ dnorm(0, 1/1000^2)
	}

}"

# set random seed

set.seed(100)

# data for jags

dat <- list(Private = college_df$Private
, N = nrow(college_df)
, Accept.Rate = college_df$Accept.Rate
, Expend = college_df$Expend)

# run

lr <- jags.model(textConnection(lr_string)
, data = dat
, n.chains = 1)

# burn in

update(lr, 10000)

# samples

lr_sim <- coda.samples(model = lr
, variable.names = "beta"
, n.iter = 10000)

# plot

plot(lr_sim)

# summary

summary(lr_sim)

# compare with OLS

freq_lr <- glm("Private ~ Accept.Rate + Expend", data = college_df, family = "binomial")

summary(freq_lr)
