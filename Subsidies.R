library(tidyverse)
library(janitor)

IEs_subsidies <- IEs %>% 
  select(id, title, assigned_to, type, created_at, updated_at, year_of_publication, intervention, outcome) %>% 
  filter(intervention %in% c("Soya subsidies", "Maize subsidies", "Cassava subsidies", "cocao subsidies", "cotton subsidies", "coffee subsidies", "rice subsidies", "sugar cane subsidies", "sweet potato/yam subsidies", "wheat subsidies"))

IEs_subsidies %>% 
  count(intervention)