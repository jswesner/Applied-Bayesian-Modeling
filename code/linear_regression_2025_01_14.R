library(brms)
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
# r_mod <- quap(
#   alist(
#     Petal.Length ~ dnorm(mu, sigma),
#     mu <- a[Species] + b[Species]*Sepal.Length,
#     a[Species] ~ dnorm(-5, sigma_slope_species),
#     sigma_slope_species ~ dexp(1),
#     b[Species] ~ dnorm(0, sigma_species),
#     sigma_speices ~ dexp(1),
#     sigma ~ dexp(1)),
#   data = iris)
#
# precis(r_mod)
# varying intercepts and slopes
mod_brm = brm(Petal.Length ~ 1 + Sepal.Length + (1 + Sepal.Length|Species),
              data = iris)

# fixed effects
mod_brm_fixed = brm(Petal.Length ~ 1 + Sepal.Length*Species,
              data = iris)


plot(conditional_effects(mod_brm), points = T)

plot(conditional_effects(mod_brm_fixed), points = T)


posteriors = tibble(Sepal.Length = seq(from = 4.3,
                                       to = 8,
                                       length.out = 30)) %>%
  expand_grid(Species = c("setosa", "versicolor", "virginica")) %>%
  tidybayes::add_epred_draws(mod_brm)

posteriors %>%
  # filter(.draw <= 100) %>%
  ggplot(aes(x = Sepal.Length, y = .epred, color = Species, fill = Species)) +
  # geom_point() +
  # geom_line(aes(color = Species,
  #               group = interaction(Species,.draw)), alpha = 0.2) +
  geom_point(data = iris, aes(y = Petal.Length, color = Species)) +
  stat_lineribbon(alpha = 0.2) +
  theme_classic() +
  labs(y = "Petal Length (cm)",
       x = "Sepal Length (cm)")


# derived quantities
slopes = tibble(Sepal.Length = seq(from = 5,
                                       to = 6,
                                       length.out = 2)) %>%
  expand_grid(Species = c("setosa", "versicolor", "virginica")) %>%
  tidybayes::add_epred_draws(mod_brm)


slope_calcs = slopes %>%
  ungroup %>%
  select(-.row, -.chain, -.iteration) %>%
  pivot_wider(names_from = Sepal.Length, values_from = .epred) %>%
  mutate( slope = `6`-`5`)

slope_diffs = slope_calcs %>%
  select(Species, .draw, slope) %>%
  # filter(.draw <= 2) %>%
  pivot_wider(names_from = Species, values_from = slope) %>%
  mutate(diff_vs = versicolor - setosa,
         diff_virg_s = virginica - setosa)

quantile(slope_diffs$diff_vs, probs = c(0.025, 0.5, 0.975))






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






