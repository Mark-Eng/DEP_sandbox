library(tidyverse)
library(janitor)
library(readxl)
library(writexl)


# Prep IEs
ie_dat_raw <- read_xlsx("Data/ier-published_2023-11-22.xlsx") 

ie_dat  <- clean_names(ie_dat_raw) %>% 
  mutate(id = as.character(id)) %>% 
  rename(sector = sector_name,
         year = year_of_publication, 
         sdg = un_sustainable_development_goal,
         res_agen = research_funding_agency_name
  ) %>% 
  mutate(type = case_match(type, "ier" ~ "IE")) %>% 
  ungroup()

# Prep SRs
SRs_raw <- read_csv("Data/srr-records.csv")
SRs <- clean_names(SRs_raw)

SRs <- SRs %>% 
  rename(rating = based_on_the_above_assessments_of_the_methods_how_would_you_rate_the_reliability_of_the_review)