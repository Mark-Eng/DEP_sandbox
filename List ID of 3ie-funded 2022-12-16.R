library(tidyverse)
library(readxl)
library(stringr)
library(purrr)

SRs <- read_xlsx("data/SR_2022-12-15.xlsx")

# List IDs of 3ie-funded SRs, method 1:
funded_SRs <- SRs %>%
  filter(`3ie_funded` == "yes") %>% 
  select(id) %>% 
  reduce(str_c) %>% 
  str_c(collapse = ",")

print(funded_SRs)


# Method 2:
funded_SRs <- SRs %>%
  filter(`3ie_funded` == "yes") %>% 
  select(id)

funded_SRs %>% 
  reduce(str_c) %>% 
  str_c(collapse = ",")

  

