library(tidyverse)
library(janitor)
library(readxl)
library(scales)

# ————————————————————————————————————————————————————
# IMPORT DATA ####
# ————————————————————————————————————————————————————

sr_dat_raw <- read_xlsx("Data/srr-published-with-rating_2023-10-03.xlsx")

sr_dat <- clean_names(sr_dat_raw) %>%
  mutate(id = as.character(id)) %>% 
  rename(sector = sector_name,
         year = year_of_publication, 
         sdg = un_sustainable_development_goal,
         res_agen = research_funding_agency_name,
         rating = based_on_the_above_assessments_of_the_methods_how_would_you_rate_the_reliability_of_the_review
  ) %>% 
  mutate(type = case_match(type, "srr" ~ "SR")) %>% 
  ungroup()

# ————————————————————————————————————————————————————
# ANALYSE CONFIDENCE RATING BY YEAR ####
# ————————————————————————————————————————————————————

sr_rat_year <- sr_dat %>%
  select(id, year, sector, rating) %>% 
  drop_na(year, rating, sector) %>% 
  filter(year != 999,
         year <= 2023) %>% 
  group_by(year,  rating) %>% 
  summarise(id_count = n_distinct(id)) %>% 
  group_by(year) %>% 
  mutate(year_sr_tot = sum(id_count)) %>% 
  group_by(rating, .add = TRUE) %>% 
  mutate(year_rating_perc = id_count/year_sr_tot)

# ————————————————————————————————————————————————————
#     CREATE CHART - SRs BY RATING OVER TIME ####
# ————————————————————————————————————————————————————
plot_sr_rat_year <- ggplot(data = sr_rat_year,
                           mapping = aes(
                             x = year,
                             y = year_rating_perc,
                             group = rating,
                             colour = rating
                           )
) + 
  geom_line(linewidth = 0.5) + 
  scale_colour_manual(values = c("#30BE30",
                                 "#FFBB01",
                                 "red"),
                      breaks = c("High", "Medium", "Low")
  ) +
  labs(
    x = "Year",
    y = "Percentage of reviews"
  ) + 
  scale_y_continuous(limits = c(0, 1),
                     breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  theme(
    legend.title = element_blank(),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45),
    panel.grid.major.y = element_line(
      linewidth = 0.25,
      colour = "#DEDEDE"
    )
  )

plot_sr_rat_year
