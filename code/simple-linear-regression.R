# Simple linear regression with brms

# 1) load packages
library(tidyverse)
library(tidybayes)
library(brms)

# 2) get data
dat = mtcars

# 3) plot data
mtcars %>%
  ggplot(aes(x = hp, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm")

# 4) standardize data
dat_s = dat %>%
  mutate(hp_s = (hp - mean(hp))/sd(hp),
         mpg_10 = mpg/10,
         gear = as.factor(gear),
         names = case_when(gear == "3" ~ "siri",
                           gear == "4" ~ "alexa",
                           gear == "5" ~ "eva"))

dat_s %>%
  ggplot(aes(x = hp_s, y = mpg_10, color = gear)) +
  geom_point() +
  geom_smooth(method = "lm")

# 5) fit a model
fit1 = brm(mpg_10 ~ hp_s,
           data = dat_s,
           family = gaussian(),   # specify the likelihood
           chains = 2,
           iter = 1000,
           file = "models/fit1.rds",
           file_refit = "on_change")

# 6) plot the model
conditional_effects(fit1)
plot(conditional_effects(fit1), points = T)

# 7) Fit an interaction model
fit5 = brm(mpg_10 ~ hp_s*gear,  # interaction between hp and gear
           data = dat_s,
           family = gaussian(),   # specify the likelihood
           chains = 2,
           iter = 1000,
           file = "models/fit5.rds",
           file_refit = "on_change")

# 8) refit the interaction model with different names for the gears
fit6 = update(fit5, newdata = dat_s,
              formula = . ~ hp_s*names)

# 9) View prediction intervals
conditional_effects(fit6, method = "predict")

# Different likelihood ----------------------------------------------------

# mpg can't go below zero and is continuous, so a Gamma might be better.
# Let's update the previous model using a Gamma likelihood with a log link
fit2 = update(fit1, family = Gamma(link = "log"),
              file = "models/fit2.rds")

plot(conditional_effects(fit2), points = T)


# Priors ----------------------------------------------------

prior_summary(fit2)

fit3 = brm(mpg_10 ~ hp_s,
           data = dat_s,
           family = Gamma(link = "log"),
           chains = 2,
           iter = 1000,
           prior = c(prior(normal(0, 1), class = "b"),
                     prior(normal(0.6, 0.2), class = "Intercept")),
           file = "models/fit3.rds",
           sample_prior = T,
           file_refit = "on_change")


