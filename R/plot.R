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
  read_xlsx("data/hslepivotab1.xlsx", sheet = 5) %>%
  clean_names()

names(expectancy)

##

expectancy %>%
  separate(period, sep = "-", into = c("start", "end")) %>%
#  mutate(start = as.numeric(start),
 #        end = as.numeric(paste("20", end, sep = ""))) %>%
  mutate(year = end) %>%
  filter(age_group == "65-69") %>%
  filter(sex == "Male") %>%
  filter(end == "11") %>%
  select(year, code, proportion_of_life_spent_in_good_health_percent) %>%
  left_join(authorities) %>%
  drop_na() %>%
  st_as_sf() %>%
  select(proportion_of_life_spent_in_good_health_percent) %>%
  plot()





