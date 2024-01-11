library(tidyverse)

IEs_health <- IEs %>% 
  mutate(id = as.character(id)) %>% 
  filter(sector_name == "Health") %>% 
  drop_na(country) %>% 
  group_by(country) %>%
  summarise(id_count = n_distinct(id)) %>% 
  arrange(desc(id_count))
    
IEs_ed <- IEs %>% 
  mutate(id = as.character(id)) %>% 
  filter(sector_name == "Education") %>% 
  drop_na(country) %>% 
  group_by(country) %>%
  summarise(id_count = n_distinct(id)) %>% 
  arrange(desc(id_count))
