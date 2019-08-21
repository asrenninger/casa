library(tidyverse)
library(janitor)

##

library(readxl)

##

national_ages <-
  bind_rows(read_xls("data/aging.xls", skip = 5, sheet = 2) %>%
              clean_names() %>%
              filter(area == "England" & age_group != "All ages") %>%
              separate(age_group, sep = "-", into = c("lower", "upper")) %>%
              replace_na(list("upper" = 94)) %>%
              mutate(upper = as.numeric(upper)) %>%
              select(upper, x2016:x2041) %>%
              gather(year, population, x2016:x2041) %>%
              mutate(population = population * -1, group = "male"),
            read_xls("data/aging.xls", skip = 5, sheet = 2) %>%
              clean_names() %>%
              filter(area == "England" & age_group != "All ages") %>%
              separate(age_group, sep = "-", into = c("lower", "upper")) %>%
              replace_na(list("upper" = 94)) %>%
              mutate(upper = as.numeric(upper)) %>%
              select(upper, x2016:x2041) %>%
              gather(year, population, x2016:x2041) %>%
              mutate(group = "female"))

##

theme_pop <- function () {
  theme_minimal() +
    theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
          panel.grid.major.y = element_line(size = 0.1, colour = 'grey50'),
          panel.grid.major.x = element_line(size = 0.1, colour = 'grey50'),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.line.x = element_line(size = 0.5, colour = 'black'),
          axis.line.y = element_blank(),
          axis.ticks.x = element_line(size = 0.5, colour = 'black'),
          axis.ticks.y = element_line(size = 0.1, colour = 'grey50'),
          axis.text.x = element_text(face = 'bold'),
          axis.text.y = element_text(face = 'bold'),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'black', size = 15),
          strip.text = element_text(face = 'bold', colour = 'black'),
          plot.margin = margin(5, 5, 5, 5)
    )
}

##

library(gganimate)

##

groups <- 
  read_xls("data/aging.xls", skip = 5, sheet = 2) %>%
  clean_names() %>%
  filter(area == "England" & age_group != "All ages") %>%
  separate(age_group, sep = "-", into = c("lower", "upper"), remove = FALSE) %>%
  replace_na(list("upper" = 94)) %>%
  mutate(upper = as.numeric(upper)) %>%
  select(age_group, lower, upper) %>%
  group_by(age_group) %>%
  slice(1)

##

national_aging <- 
  ggplot(national_ages %>%
           left_join(groups) %>%
           mutate(year = str_replace_all(year, pattern = "x", replacement = ""))) +
  geom_bar(aes(y = population, x = upper, fill = group), stat = 'identity',
           show.legend = FALSE) +
  geom_text(aes(x = upper, y = 0, label = age_group), colour = '#ffffff') +
  scale_fill_manual(values = c("#800026", "#081d58")) +
  scale_y_continuous(labels = abs, limits = max(national_ages$population, na.rm = TRUE) * c(-1, 1)) +
  scale_x_continuous(labels = c("", "", ""), breaks = c(25, 50, 75), limits = c(-1, 100)) +
  transition_states(year) +
  ease_aes('linear') +
  labs(title = "national projections {closest_state}",
       subtitle = "PROJECTED POPULATION BY AGE",
       x = "age bracket", y = "population") +
  coord_flip() +
  theme_pop()

##

anim_save(national_aging, filename = "aging.gif")

##

local_ages <- 
  read_xls("data/aging.xls", skip = 5, sheet = 4) %>%
  clean_names() %>%
  filter(area != "England" & age_group != "All ages") %>%
  separate(age_group, sep = "-", into = c("lower", "upper")) %>%
  replace_na(list("upper" = 94)) %>%
  mutate(age = as.numeric(upper)) %>%
  select(code, age, x2016:x2041) %>%
  gather(year, population, x2016:x2041)

##

library(sf)

##

authorities <-
  st_read("data/authorities_WGS84.geojson") %>%
  clean_names() %>%
  rename(code = lad17cd) %>%
  select(code, st_areashape, st_lengthshape)

##

local_ages %>%
  group_by(code) %>%
  summarise(age = weighted.mean(age, population)) %>%
  left_join(authorities) %>%
  drop_na() %>%
  st_as_sf() %>%
  select(age) %>%
  plot()

##

expectancy <- 
  read_xls("data/healthy.xls", skip = 10, sheet = 1) %>%
  clean_names()

names(expectancy)

##

expectancy_spatially <- 
  expectancy %>%
  left_join(authorities) %>%
  drop_na() %>%
  st_as_sf()

##

background <- 
  authorities %>%
  mutate(dissolve = 1) %>%
  group_by(dissolve) %>%
  summarise()

##

library(scico)

##

ggplot(data =
         expectancy_spatially %>%
         select(healthy_life_expectancy_for_females_2009_2013_years,
                healthy_life_expectancy_for_males_2009_2013) %>%
         gather(variable, value, healthy_life_expectancy_for_females_2009_2013_years:healthy_life_expectancy_for_males_2009_2013) %>%
         mutate(name = case_when(variable == "healthy_life_expectancy_for_males_2009_2013" ~ "male",
                                 variable == "healthy_life_expectancy_for_females_2009_2013_years" ~ "female")) %>%
         select(variable, name, value, geometry)) +
  geom_sf(data = background,
          aes(), 
          fill = 'grey70', colour = NA, size = 0) +
  geom_sf(aes(fill = value), 
          colour = NA, size = 0) +
  scale_fill_scico(palette = 'lajolla', direction = -1,
                   guide = guide_continuous) +
  facet_wrap(~ name) +
  theme_map()

##

ggplot(data =
         expectancy_spatially %>%
         mutate(difference_male = life_expectancy_at_birth_for_males_2009_2013 - healthy_life_expectancy_for_males_2009_2013,
                difference_female = life_expectancy_at_birth_for_females_2009_2013 - healthy_life_expectancy_for_females_2009_2013_years) %>%
         select(difference_female, difference_male) %>%
         gather(variable, value, difference_male:difference_female) %>%
         mutate(name = case_when(variable == "difference_male" ~ "male",
                                 variable == "difference_female" ~ "female")) %>%
         select(variable, name, value, geometry)) +
  geom_sf(data = background,
          aes(), 
          fill = 'grey70', colour = NA, size = 0) +
  geom_sf(aes(fill = value), 
          colour = NA, size = 0) +
  scale_fill_scico(palette = 'lajolla', direction = 1,
                   guide = guide_continuous) +
  facet_wrap(~ name) +
  theme_map()

##

names(expectancy)

## 

library(magrittr)

lm(healthy_life_expectancy_for_females_2009_2013_years ~ life_expectancy_at_birth_for_females_2009_2013, 
   data = expectancy) %>%
  summary() %>%
  use_series("r.squared")

lm(healthy_life_expectancy_for_males_2009_2013 ~ life_expectancy_at_birth_for_males_2009_2013, 
   data = expectancy) %>%
  summary() %>%
  use_series("r.squared")

##

plot_expectancies <- 
  ggplot(bind_rows(select(expectancy, 
                          healthy_life_expectancy_for_females_2009_2013_years,
                          life_expectancy_at_birth_for_females_2009_2013) %>%
                     set_names(c("in good health", "at birth")) %>%
                     mutate(class = "female"),
                   select(expectancy, healthy_life_expectancy_for_males_2009_2013,
                          life_expectancy_at_birth_for_males_2009_2013) %>%
                     set_names(c("in good health", "at birth")) %>%
                     mutate(class = "male")), 
         aes(x = `at birth`, y = `in good health`, colour = class)) +
  geom_point(alpha = 0.5, show.legend = FALSE) +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, linetype = 2, show.legend = FALSE) +
  geom_text(aes(x = 75, y = 70, label = "r-squared = 0.88"), hjust = 0) +
  geom_text(aes(x = 80, y = 50, label = "r-squared = 0.73"), hjust = 0) +
  scale_colour_manual(values = c("#800026", "#081d58")) +
  labs(title = "local authorities",
       subtitle = "TWIN LIFE EXPECTANCIES") +
  theme_ver()

##

ggsave(plot_expectancies, filename = "expectancies.png", height = 6, width = 6, dpi = 300)

##

