library(tidybayes)
library(brms)
library(tidyverse)

mtcars
mean_hp = mean(mtcars$hp)
sd_hp = sd(mtcars$hp)

fit5$data %>% as_tibble()
as_draws_df(fit5)

# 1) set up values to predict from

hp_s = 0
gear = "4"
names = "alexa"

data_to_predict = tibble(hp_s = hp_s,
                         gear = gear,
                         names = names)

first_post = data_to_predict %>%
  add_epred_draws(fit5)

first_post %>%
  ggplot(aes(x = hp_s, y = .epred, color = gear)) +
  geom_point() +
  facet_wrap(~names)

# create a sequence of continuous predictor
hp_s = seq(-2, 2, length.out = 30) # simulates 30 sequences from -2 to 2
gear = unique(fit5$data$gear) # get unique values of gear
names = unique(fit5$data$names)

data_to_predict = tibble(hp_s = hp_s) %>%
  expand_grid(gear = gear) %>%
  expand_grid(names = names) %>%
  exapnd_grid(other = c(-1, 0, 1))

first_post = data_to_predict %>%
  add_epred_draws(fit5)

first_post %>%
  filter(.draw <= 200) %>%
  ggplot(aes(x = (hp_s*sd_hp) + mean_hp, y = .epred*10, color = gear)) +
  # geom_point() +
  geom_line(aes(group = interaction(.draw, gear)), alpha = 0.2) +
  facet_wrap(gear~names) +
  geom_point(data = fit5$data, aes(y = mpg_10*10), color = "black")




# calculate slope ---------------------------------------------------------

# create a sequence of continuous predictor
hp_s = seq(0, 1, length.out = 2) # simulates 2 sequences from 0 to 1
gear = unique(fit5$data$gear) # get unique values of gear
names = unique(fit5$data$names)

data_to_predict = tibble(hp_s = hp_s) %>%
  expand_grid(gear = gear) %>%
  expand_grid(names = names)

first_post = data_to_predict %>%
  add_epred_draws(fit5)

first_post %>%
  filter(.draw <= 200) %>%
  ggplot(aes(x = hp_s, y = .epred, color = gear)) +
  # geom_point() +
  geom_line(aes(group = interaction(.draw, gear)), alpha = 0.2) +
  facet_wrap(gear~names) +
  geom_point(data = fit5$data, aes(y = mpg_10), color = "black")

slopes = first_post %>%
  ungroup %>%
  select(hp_s, gear, names, .draw, .epred) %>%
  pivot_wider(names_from = hp_s,
              values_from = .epred) %>%
  mutate(slope = `1` - `0`)


slopes %>%
  group_by(gear, names) %>%
  median_qi(slope, .width = 0.95)

# probability of slope being negative
slopes %>%
  group_by(gear, names) %>%
  reframe(prob_negative = sum(slope < 0)/max(.draw))


# probability of slope being negative
slopes %>%
  select(gear, names, .draw, slope) %>%
  pivot_wider(names_from = gear,
              values_from = slope) %>%
  mutate(diff_43 = `3` - `4`,
         diff_45 = `5` - `4`,
         diff_35 = `3` - `5`) %>%
  median_qi(diff_45)


slopes %>%
  select(gear, names, .draw, slope) %>%
  pivot_wider(names_from = gear,
              values_from = slope) %>%
  mutate(diff_43 = `3` - `4`,
         diff_45 = `5` - `4`,
         diff_35 = `3` - `5`) %>%
  group_by(names) %>%
  reframe(prob = sum(diff_45>0)/max(.draw))
