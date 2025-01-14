library(brms)
library(rethinking)
library(janitor)
library(tidybayes)
library(tidyverse)

# linear model petal.length ~ sepal.length, y ~ x, y ~ a + Bx,
# lm(petal.length ~ sepal.length)

# petal.length ~ Normal(u, sigma)
# u = a + B*sepal.length
# a ~ N(0, 1)
# B ~ N(0, 1)
# sigma ~ Exponential(1)

mod = lm(Petal.Length ~ Sepal.Length, data = iris)

summary(mod)
iris %>% # ctrl+shift+m OR cmd+shift+m
  ggplot(aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point() +
  geom_smooth(method = lm)

# rethinking style
r_mod <- quap(
  alist(
    Petal.Length ~ dnorm(mu, sigma),
    mu <- a[Species] + b[Species]*Sepal.Length,
    a[Species] ~ dnorm(-5, sigma_slope_species),
    sigma_slope_species ~ dexp(1),
    b[Species] ~ dnorm(0, sigma_species),
    sigma_speices ~ dexp(1),
    sigma ~ dexp(1)),
  data = iris)

precis(r_mod)

mod_brm = brm(Petal.Length ~1 + Sepal.Length +
                (1 + Sepal.Length|Species),
              data = iris)



summary(mod_brm)
iris_clean  = iris %>% clean_names()
mod_brm_update = update(mod_brm, newdata = iris_clean,
                        formula = petal_length ~ sepal_length,
                        prior = c(prior(normal(0, 5), class = b),
                                  prior(normal(-5, 2), class = Intercept),
                                  prior(exponential(1), class = sigma)))

plot_data = plot(conditional_effects(mod_brm), points = T)

plot_data$`Sepal.Length:Species` +
  theme_classic() +
  labs(y = "Petal Length (mm)",
        x = "Sepal Length (mm)")






