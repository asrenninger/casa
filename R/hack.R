library(tidyverse)
library(readxl)
library(janitor)

##

national_ages <-
  bind_rows(read_xls("data/aging.xls", skip = 5, sheet = 2) %>%
              clean_names() %>%
              filter(area == "England" & age_group != "All ages") %>%
              separate(age_group, sep = "-", into = c("lower", "upper")) %>%
              replace_na(list("upper" = 94)) %>%
              mutate(upper = as.numeric(upper)) %>%
              select(upper, x2016:x2041) %>%
              gather(year, population, x2016:x2036) %>%
              mutate(population = population * -1, group = "male"),
            read_xls("data/aging.xls", skip = 5, sheet = 3) %>%
              clean_names() %>%
              filter(area == "England" & age_group != "All ages") %>%
              separate(age_group, sep = "-", into = c("lower", "upper")) %>%
              replace_na(list("upper" = 94)) %>%
              mutate(upper = as.numeric(upper)) %>%
              select(upper, x2016:x2041) %>%
              gather(year, population, x2016:x2036) %>%
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

library(scico)

##

national_aging <- 
  ggplot(national_ages %>%
           left_join(groups) %>%
           mutate(year = str_replace_all(year, pattern = "x", replacement = ""))) +
  geom_bar(aes(y = population, x = upper, fill = group), stat = 'identity',
           show.legend = FALSE) +
  geom_text(aes(x = upper, y = 0, label = age_group), colour = '#ffffff') +
  scale_fill_manual(values = c(scico(palette = "grayC", 10)[3], scico(palette = "grayC", 10)[10])) +
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

expectancy <- 
  read_xls("data/healthy.xls", skip = 10, sheet = 1) %>%
  clean_names()

##

library(magrittr)

##

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
  scale_colour_manual(values = c(scico(palette = "grayC", 10)[3], scico(palette = "grayC", 10)[10])) +
  labs(title = "local authorities",
       subtitle = "TWIN LIFE EXPECTANCIES") +
  theme_ver()

##

ggsave(plot_expectancies, filename = "expectancies.png", height = 4, width = 6, dpi = 300)

##

library(sf)

##

authorities <- st_read("data/authorities.shp")

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

map_differences <- 
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
  labs(title = "local authorities",
       subtitle = "DIFFERENCES WITH HEALTHSPAN AND LIFESPAN") +
  theme_map()

##

ggsave(map_differences, filename = "differences.png", height = 8, width = 8, dpi = 300)

##

autocorrelating <- 
  expectancy %>%
  mutate(difference_male = life_expectancy_at_birth_for_males_2009_2013 - healthy_life_expectancy_for_males_2009_2013,
         difference_female = life_expectancy_at_birth_for_females_2009_2013 - healthy_life_expectancy_for_females_2009_2013_years) %>%
  mutate(difference = ((difference_male + difference_female) / 2)) %>%
  left_join(authorities) %>%
  st_as_sf()

##

library(spdep)

##

coords <- 
  autocorrelating %>%
  st_centroid() %>%
  st_coordinates()

nearest <- knn2nb(knearneigh(coords, 5))
weights <- nb2listw(nearest, style = "W")

##

moranstest <- moran.test(autocorrelating$difference, weights)
montecarlo <- moran.mc(autocorrelating$difference, weights, nsim = 999)

moranstest

##

ggplot(as.data.frame(montecarlo$res), aes(montecarlo$res)) + 
  geom_histogram(binwidth = 0.01) +
  geom_vline(aes(xintercept = 0.466), colour = "grey70",size = 1) +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(title = "observed and permuted Moran's I",
       subtitle = "I = 0.466 | P < 0.01",
       x = "results",
       y = "count") +
  theme_ver()

ggsave(filename = "montecarlo.png", height = 4, width = 6, dpi = 300)

##

moransi <- as_tibble(localmoran(autocorrelating$difference, weights))

##

autocorrelating <- 
  autocorrelating %>%
  bind_cols(moransi) %>%
  rename(locali = Ii,
         expectation = E.Ii,
         variance = Var.Ii,
         deviation = Z.Ii,
         p_value = `Pr(z > 0)`)


##

guide_discrete <-
  guide_legend(direction = "horizontal",
               keyheight = unit(2, units = "mm"),
               keywidth = unit(10, units = "mm"),
               title.position = 'top',
               label.position = 'bottom',
               title.hjust = 0.5,
               label.hjust = 1,
               nrow = 1,
               byrow = TRUE)

##

theme_map_legend <- function () {
  theme_void() + 
    theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major = element_line(size = NA), 
          panel.grid.minor = element_line(size = NA),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'black', size = 15),
          plot.caption = element_text(face = 'bold', colour = 'black'),
          strip.text = element_text(face = 'bold', colour = 'black'),
          legend.position = 'bottom',
          legend.title = element_text(face = 'bold', colour = 'grey50'),
          legend.text = element_text(colour = 'black'),
          plot.margin = margin(5, 5, 5, 5)
    )
  
}

##

map_moran_difference <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = autocorrelating,
          aes(fill = factor(ntile(difference, 5))), size = 0, colour = NA) +
  scale_fill_manual(values = scico(palette = 'cork', 5),
                    labels = as.character(quantile(autocorrelating$difference,
                                                   c(.1,.2,.4,.6,.8),na.rm = TRUE)),
                    name = "difference",
                    guide = guide_discrete) +
  labs(title = "difference") +
  theme_map_legend()

##

map_moran_i <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = autocorrelating,
          aes(fill = factor(ntile(locali, 5))), size = 0, colour = NA) +
  scale_fill_manual(values = scico(palette = 'cork', 5),
                    labels = str_sub(as.character(quantile(autocorrelating$locali,
                                                           c(.1,.2,.4,.6,.8),na.rm = TRUE)), 1, 4),
                    name = "i value",
                    guide = guide_discrete) +
  labs(title = "local moran's i") +
  theme_map_legend()

##

map_moran_p <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = autocorrelating,
          aes(fill = factor(ntile(p_value, 5))), size = 0, colour = NA) +
  scale_fill_manual(values = scico(palette = 'cork', 5),
                    labels = str_sub(as.character(quantile(autocorrelating$p_value,
                                                           c(.1,.2,.4,.6,.8),na.rm = TRUE)), 1, 4),
                    name = "p value",
                    guide = guide_discrete) +
  labs(title = "p value") +
  theme_map_legend()  

##

library(gridExtra)

##

moran <- grid.arrange(map_moran_difference, map_moran_i, map_moran_p, ncol = 3)

##

ggsave(moran, filename = "matrix.png", height = 6, width = 12, dpi = 300)

##

autocorrelating <- 
  autocorrelating %>%
  mutate(scaled_difference = scale(difference)) %>%
  select(difference, scaled_difference, locali, expectation, variance, deviation, p_value) %>%
  mutate(lagged_difference = lag.listw(weights, scaled_difference),
         quad_sig = NA)

##

autocorrelating <-
  autocorrelating %>%
  mutate(quad_sig = case_when(scaled_difference >= 0 & lagged_difference >= 0 & p_value <= 0.05 ~ 1,
                              scaled_difference <= 0 & lagged_difference <= 0 & p_value <= 0.05 ~ 2,
                              scaled_difference >= 0 & lagged_difference <= 0 & p_value <= 0.05 ~ 3,
                              scaled_difference >= 0 & lagged_difference <= 0 & p_value <= 0.05 ~ 4,
                              scaled_difference <= 0 & lagged_difference >= 0 & p_value <= 0.05 ~ 5)) %>%
  st_as_sf()

##

labels <- c("high-high", "low-low")

##

map_quads <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = autocorrelating,
          aes(fill = factor(quad_sig)), size = 0, colour = NA) +
  scale_fill_manual(values = scico(palette = 'cork', 2),
                    name = "quadrants",
                    labels = labels,
                    guide = guide_discrete,
                    na.translate = FALSE) +
  labs(title = "local moran's i") +
  theme_map_legend()  

##

ggsave(map_quads, filename = "quadrants.png", height = 8, width = 8, dpi = 300)

##

ahah <- read_csv("data/ahahinputs.csv")

##

area <- 
  st_read("data/area.shp") %>%
  st_drop_geometry() %>%
  transmute(lsoa11 = lsoa01cd,
            code = lad17cd) %>%
  left_join(ahah) %>%
  select(lsoa11:green900) %>%
  group_by(code) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  select(-seqnum)

##

ahah %>%
  select(ffood_d, pubs2_d, pharm_d, leis_d) %>%
  set_names(c("fast food", "pubs", "pharmacies", "leisure")) %>%
  gather(variable, value, `fast food`:`leisure`) %>%
  mutate(value = log(value)) %>%
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~ variable) + 
  theme_hor()

##

area_spatially <- 
  area %>%
  left_join(authorities) %>%
  st_as_sf()

##

library(geogrid)

##

hexdata <- 
  authorities %>%
  filter(code %in% expectancy$code) %>%
  as('Spatial')

##

# new_cells_hex <- calculate_grid(shape = hexdata, grid_type = "hexagonal", seed = 1)
# resulthex <- assign_polygons(hexdata, new_cells_hex)

##

resulthex <- st_read("data/hexgrid_britain.shp")

##

summary_variables <- 
  resulthex %>%
  st_as_sf() %>%
  select(code) %>%
  left_join(area) %>%
  st_as_sf()

##

library(scales)

##

map_summary <-
  ggplot(data = 
           summary_variables %>%
           filter(code %in% expectancy$code) %>%
           transmute(`general practicioners` = gpp_d, 
                     `hospitals` = ed_d, 
                     `dentists` = dent_d, 
                     `pharmacies` = pharm_d) %>%
           gather(variable, value, `general practicioners`:`pharmacies`) %>%
           group_by(variable)) +
  geom_sf(aes(fill = value), 
          colour = NA, size = 0) +
  scale_fill_scico(palette = 'lajolla', direction = 1,
                   guide = guide_continuous,
                   name = "distance to...",
                   limits = c(0, 10), oob = squish) +
  facet_wrap(~ variable) +
  labs(title = "local authorities",
       subtitle = "PREDICTORS OF ILL HEALTH") +
  theme_map()

##

ggsave(map_summary, filename = "summary.png", height = 8, width = 8, dpi = 300)

##

rururb <- 
  read_csv("data/rururb.csv") %>%
  clean_names() %>%
  rename(code = lad11cd) %>%
  select(code, total_rural_population_2011:total_population_2011, ruc11, broad_ruc11) %>%
  rename(class = ruc11,
         class_broad = broad_ruc11)

deaths <- read_csv("data/deaths.csv") %>%
  clean_names() %>%
  rename(code = lad11cd) %>%
  select(code, deaths_num:agestand_mortality)

income <- 
  read_xlsx("data/income.xlsx", sheet = 2, skip = 2) %>%
  clean_names() %>%
  rename(code = lau1_code) %>%
  select(code, region, x1998:x2016)

names(income) <- str_replace_all(names(income), pattern = "x", replacement = "income_")

##

data <- 
  expectancy %>%
  mutate(difference_male = life_expectancy_at_birth_for_males_2009_2013 - healthy_life_expectancy_for_males_2009_2013,
         difference_female = life_expectancy_at_birth_for_females_2009_2013 - healthy_life_expectancy_for_females_2009_2013_years) %>%
  left_join(area) %>%
  left_join(rururb) %>%
  left_join(deaths) %>%
  left_join(income) %>%
  left_join(authorities) %>%
  st_as_sf() %>%
  select(code, region, everything())

##

glimpse(data)

##

correlations <-
  data %>%
  transmute(`male (diff)` = difference_male,
            `female (diff)` = difference_female,
            `mortality` = agestand_mortality,
            `fast food (d)` = ffood_d,
            `pubs (d)` = pubs2_d,
            `GPs (d)` = gpp_d,
            `population` = total_population_2011,
            `density` = total_population_2011 / (st_rshp / (1000 * 1000)),
            `income` = income_2011,
            `rur-urban` = urban_city_and_town_population_2011 / total_population_2011,
            `living alone` = older_people_living_alone,
            `unemployment` = unemployment,
            `NO2` = no2,
            `PM10` = pm10, 
            `parks` = green900) %>%
  drop_na() %>%
  select(-geometry)

##

correlating(correlations)

##

left <-
  data %>%
  mutate(HLE = (healthy_life_expectancy_for_females_2009_2013_years + healthy_life_expectancy_for_males_2009_2013) / 2) %>%
  select(code, region, lower_tier_local_authority, HLE, everything()) %>%
  as('Spatial')

##

data %>%
  mutate(HLE = (healthy_life_expectancy_for_females_2009_2013_years + healthy_life_expectancy_for_males_2009_2013) / 2) %>%
  select(code, region, lower_tier_local_authority, HLE, everything()) %>%
  filter(code == "E06000053")

left <- left[- 52, ]

##

neighbs <- poly2nb(left, queen = TRUE)
neighbs[[46]] <- as.integer(c(44, 45)) 

##

weights <- nb2listw(neighbs, style = "W")

##

left$pollution <- scale(left$no2) + scale(left$pm10) + scale(left$so2)
left$income_2011 <- log(left$income_2011)

##

library(spgwr)

##

bandwidth <- gwr.sel(HLE ~ pollution, 
                     data = left)

geogress    <- gwr(HLE ~ pollution, 
                   data = left,
                   bandwidth = col.bw)

##

map_air <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = 
            geogress$SDF %>%
            st_as_sf() %>%
            select(pollution),
          aes(fill = factor(ntile(pollution, 5))), size = 0.01, colour = 'gray70') +
  scale_fill_manual(values = scico(palette = 'oslo', 5),
                    labels = str_sub(as.character(quantile(geogress$SDF$pollution,
                                                           c(.1,.2,.4,.6,.8),
                                                           na.rm = TRUE)), 1, 4),
                    name = "coefficent",
                    guide = guide_discrete) +
  labs(title = "air pollution") +
  theme_map_legend() 

##

ggsave(map_air, filename = "air.png", height = 8, width = 8, dpi = 300)

##

left %>%
  st_as_sf() %>%
  st_drop_geometry() %>%
  mutate(coefficient = geogress$SDF$pollution) %>%
  select(region, lower_tier_local_authority, HLE, pollution, coefficient) %>%
  top_n(10, coefficient) %>%
  arrange(desc(coefficient))

left %>%
  st_as_sf() %>%
  st_drop_geometry() %>%
  mutate(coefficient = geogress$SDF$pollution) %>%
  select(region, lower_tier_local_authority, HLE, pollution, coefficient) %>%
  top_n(-10, coefficient) %>%
  arrange(coefficient)

##

results <- 
  read_csv("data/gwrresults.csv") %>%
  clean_names()

## 

shape <- 
  st_read("data/results_shape.shp") %>%
  filter(code %in% left$code) %>%
  select(code, geometry)

results_shape <-
  results %>%
  bind_cols(shape) %>%
  st_as_sf() %>%
  clean_names()

##

map_deprivation <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = results_shape,
          aes(fill = factor(ntile(depriv_coef, 5))), size = 0.01, colour = 'gray70') +
  scale_fill_manual(values = scico(palette = 'oslo', 5),
                    labels = str_sub(as.character(quantile(results_shape$depriv_coef,
                                                           c(.1,.2,.4,.6,.8),na.rm = TRUE)), 1, 4),
                    name = "coefficent",
                    guide = guide_discrete) +
  labs(title = "deprivation") +
  theme_map_legend()  

map_air <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = results_shape,
          aes(fill = factor(ntile(airpol_coef, 5))), size = 0.01, colour = 'gray70') +
  scale_fill_manual(values = scico(palette = 'oslo', 5),
                    labels = str_sub(as.character(quantile(results_shape$airpol_coef,
                                                           c(.1,.2,.4,.6,.8),na.rm = TRUE)), 1, 4),
                    name = "coefficent",
                    guide = guide_discrete) +
  labs(title = "air pollution") +
  theme_map_legend() 

map_lifestyle <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = results_shape,
          aes(fill = factor(ntile(lifest_coef, 5))), size = 0.01, colour = 'gray70') +
  scale_fill_manual(values = scico(palette = 'oslo', 5, direction = -1),
                    labels = str_sub(as.character(quantile(results_shape$lifest_coef,
                                                           c(.1,.2,.4,.6,.8),na.rm = TRUE)), 1, 4),
                    name = "coefficent",
                    guide = guide_discrete) +
  labs(title = "lifestyle distances") +
  theme_map_legend() 

map_unemployment <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = results_shape,
          aes(fill = factor(ntile(unempl_coef, 5))), size = 0.01, colour = 'gray70') +
  scale_fill_manual(values = scico(palette = 'oslo', 5),
                    labels = str_sub(as.character(quantile(results_shape$unempl_coef,
                                                           c(.1,.2,.4,.6,.8),na.rm = TRUE)), 1, 4),
                    name = "coefficent",
                    guide = guide_discrete) +
  labs(title = "unemployment") +
  theme_map_legend()

map_health <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = results_shape,
          aes(fill = factor(ntile(hlthserv_coef, 5))), size = 0.01, colour = 'gray70') +
  scale_fill_manual(values = scico(palette = 'oslo', 5),
                    labels = str_sub(as.character(quantile(results_shape$hlthserv_coef,
                                                           c(.1,.2,.4,.6,.8),na.rm = TRUE)), 1, 4),
                    name = "coefficent",
                    guide = guide_discrete) +
  labs(title = "health services") +
  theme_map_legend()

##

results <- grid.arrange(map_lifestyle, map_health, map_unemployment, map_deprivation, ncol = 2)

ggsave(results, filename = "results.png", height = 12, width = 12, dpi = 300)



