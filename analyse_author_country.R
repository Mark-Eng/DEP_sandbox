
# ————————————————————————————————————————————————————
# PACKAGES ####
# ————————————————————————————————————————————————————
library(tidyverse)
library(readxl)
library(writexl)
library(janitor)


# ————————————————————————————————————————————————————
# INCOME/REGION LISTS ####
# ————————————————————————————————————————————————————

# HIC list
{
  hic_list <- c( # Includes all transitional countries whose first year of HIC status was 2014 or earlier
    "Andorra",
    "Antigua and Barbuda",
    "Aruba",
    "Australia",
    "Austria",
    "Bahamas",
    "Bahrain",
    "Barbados",
    "Belgium",
    "Bermuda",
    "Brunei Darussalam",
    "Canada",
    "Cayman Islands",
    "Channel Islands",
    "Chile",
    "Curacao",
    "Cyprus",
    "Czech Repuplic",
    "Denmark",
    "Estonia",
    "Faeroe Islands",
    "Finland",
    "France",
    "French Polynesia",
    "Germany",
    "Greece",
    "Greenland",
    "Guam",
    "Hong Kong (SAR)",
    "Hungary",
    "Iceland",
    "Ireland",
    "Isle of Man",
    "Israel",
    "Italy",
    "Japan",
    "Korea, Rep.",
    "Kuwait",
    "Latvia",
    "Liechtenstein",
    "Lithuania",
    "Luxembourg",
    "Macao (SAR)",
    "Malta",
    "Monaco",
    "Netherlands",
    "New Caledonia",
    "New Zealand",
    "Northern Mariana Islands",
    "Norway",
    "Oman",
    "Poland",
    "Portugal",
    "Puerto Rico",
    "Qatar",
    "San Marino",
    "Saudi Arabia",
    "Seychelles",
    "Singapore",
    "Sint Maarten (Dutch Part)",
    "Slovak Republic",
    "Slovenia",
    "Spain",
    "St. Kitts and Nevis",
    "St. Martin (French Part)",
    "Sweden",
    "Switzerland",
    "Taiwan",
    "Trinidad and Tobago",
    "Turks and Caicos Islands",
    "United Arab Emirates",
    "United Kingdom",
    "United States",
    "Uruguay",
    "Virgin Islands (US)"
  )
}

# L&MIC list
{ 
  lmic_list <- c(
    "Afghanistan",
    "Albania",
    "Algeria",
    "Angola",
    "Armenia",
    "Azerbaijan",
    "Bangladesh",
    "Belarus",
    "Belize",
    "Benin",
    "Bhutan",
    "Bolivia",
    "Bosnia and Herzegovina",
    "Botswana",
    "Brazil",
    "Bulgaria",
    "Burkina Faso",
    "Burundi",
    "Cambodia",
    "Cameroon",
    "Cabo Verde",
    "Central African Republic",
    "Chad",
    "China",
    "Colombia",
    "Comoros",
    "Congo, Dem. Rep.",
    "Congo, Rep.",
    "Costa Rica",
    "Côte d'Ivoire",
    "Cuba",
    "Djibouti",
    "Dominica",
    "Dominican Republic",
    "Ecuador",
    "Egypt, Arab Rep.",
    "El Salvador",
    "Eritrea",
    "Ethiopia",
    "Fiji",
    "Gabon",
    "Gambia, The",
    "Georgia",
    "Ghana",
    "Grenada",
    "Guatemala",
    "Guinea",
    "Guinea-Bissau",
    "Guyana",
    "Haiti",
    "Honduras",
    "India",
    "Indonesia",
    "Iran, Islamic Rep.",
    "Iraq",
    "Jamaica",
    "Jordan",
    "Kazakhstan",
    "Kenya",
    "Kiribati",
    "Korea, Dem. People's Rep.",
    "Kosovo",
    "Kyrgyz Republic",
    "Lao PDR",
    "Lebanon",
    "Lesotho",
    "Liberia",
    "Libya",
    "Macedonia, FYR",
    "Madagascar",
    "Malawi",
    "Malaysia",
    "Maldives",
    "Mali",
    "Marshall Islands",
    "Mauritania",
    "Mexico",
    "Micronesia, Federal States",
    "Moldova",
    "Mongolia",
    "Montenegro",
    "Morocco",
    "Mozambique",
    "Myanmar",
    "Namibia",
    "Nauru",
    "Nepal",
    "Nicaragua",
    "Niger",
    "Nigeria",
    "Pakistan",
    "Papua New Guinea",
    "Paraguay",
    "Peru",
    "Philippines",
    "Rwanda",
    "Samoa",
    "São Tomé and Principe",
    "Senegal",
    "Serbia",
    "Sierra Leone",
    "Solomon Islands",
    "Somalia",
    "South Africa",
    "South Sudan",
    "Sri Lanka",
    "St. Lucia",
    "St. Vincent and the Grenadines",
    "Sudan",
    "Suriname",
    "Eswatini",
    "Syrian Arab Republic",
    "Tajikistan",
    "Tanzania",
    "Thailand",
    "Timor-Leste",
    "Togo",
    "Tonga",
    "Tunisia",
    "Turkey",
    "Turkmenistan",
    "Tuvalu",
    "Uganda",
    "Ukraine",
    "Uzbekistan",
    "Vanuatu",
    "Vietnam",
    "West Bank and Gaza",
    "Yemen, Rep.",
    "Zambia",
    "Zimbabwe",
    "American Samoa",
    "Argentina",
    "Croatia",
    "Equatorial Guinea",
    "Hungary",
    "Mauritius",
    "Nauru",
    "Palau",
    "Panama",
    "Romania",
    "Russian Federation",
    "Seychelles",
    "Venezuela, RB"
  )
}

# SSA list
{
  ssa_list <- c(
    "Angola",
    "Benin",
    "Botswana",
    "Burkina Faso",
    "Burundi",
    "Cameroon",
    "Cabo Verde",
    "Central African Republic",
    "Chad",
    "Comoros",
    "Congo, Dem. Rep.",
    "Congo, Rep.",
    "Côte d'Ivoire",
    "Eritrea",
    "Eswatini",
    "Ethiopia",
    "Equatorial Guinea",
    "Gabon",
    "Gambia, The",
    "Ghana",
    "Guinea",
    "Guinea-Bissau",
    "Kenya",
    "Lesotho",
    "Liberia",
    "Madagascar",
    "Malawi",
    "Mali",
    "Mauritania",
    "Mauritius",
    "Mozambique",
    "Namibia",
    "Niger",
    "Nigeria",
    "Rwanda",
    "São Tomé and Principe",
    "Senegal",
    "Seychelles",
    "Sierra Leone",
    "Somalia",
    "South Africa",
    "South Sudan",
    "Sudan",
    "Tanzania",
    "Togo",
    "Uganda",
    "Zambia",
    "Zimbabwe"
  )
}

# MENA list 
{  
  mena_list <- c(
    "Algeria",
    "Bahrain",
    "Djibouti",
    "Egypt, Arab Rep.",
    "Iran, Islamic Rep.",
    "Iraq",
    "Israel",
    "Jordan",
    "Kuwait",
    "Lebanon",
    "Libya",
    "Malta",
    "Morocco",
    "Oman",
    "Qatar",
    "Saudi Arabia",
    "Syrian Arab Republic",
    "Tunisia",
    "United Arab Emirates",
    "West Bank and Gaza",
    "Yemen, Rep."
  )
}

# ————————————————————————————————————————————————————
# DEFINE ANALYSIS FUNCTIONS ####
# ————————————————————————————————————————————————————

{ # Curly brace for code folding purposes only
  
perc_3 <- function(numer, denom) {
  round(numer/denom, digits = 3)
}

local_y_n <- function(df) {
  df %>% 
    mutate(local_author_row = case_when(
      is.na(author_country) ~ NA,
      author_country == country ~ "Yes",
      TRUE ~ "No"
    )) %>% 
    group_by(id) %>% 
    mutate(local_author_study = case_when(
      any(author_country %in% country | is.na(author_country)) ~ "Yes",
      TRUE ~ "No"
    )) %>% 
    mutate(local_first = case_when(
      any(row_number() == 1 & author_country == country) ~ "Yes",
      TRUE ~ "No"
    )) %>% 
    ungroup() 
}

perc_local_study <- function(df) {
  df %>% 
    group_by(local_author_study) %>% 
    summarise(id_count = n_distinct(id)) %>% 
    mutate(local_study_perc = perc_3(id_count, sum(id_count)))
}

# country_perc_local_study <- function(df) {
#   n_country <- df %>%
#     group_by(country) %>%
#     summarise(n_country = n_distinct(id))
#   
#   no_local_country <- df %>%
#     group_by(id) %>%
#     filter(all(!author_country %in% country | is.na(author_country))) %>%
#     filter(local_author_study == "No") %>% 
#     group_by(country) %>%
#     summarise(no_loc_country = n_distinct(id))
# 
#   no_local_country_perc <- full_join(n_country, no_local_country) %>%
#     replace_na(list(no_loc_country = 0)) %>%
#     mutate(no_loc_country_perc = perc_3(no_loc_country, n_country))
# }

# country_perc_test <- local %>% 
#   country_perc_local_study()
  
country_perc_local_study <- function(df) {
  left_join(
    df %>%
      group_by(country) %>%
      summarise(n_country = n_distinct(id)),
    df %>%
        filter(local_author_study == "Yes") %>%
        group_by(country) %>%
        summarise(n_yes_loc = n_distinct(id)),
    ) %>%
    replace_na(list(n_yes_loc = 0)) %>%
    mutate(yes_loc_perc = perc_3(n_yes_loc, n_country))
}

country_perc_test <- local %>% 
  country_perc_local_study()

  mutate(
    n_no_local = sum(local_author == "No"),
    n_yes_local = sum(local_author == "Yes"),
    no_local_perc = perc_3(n_no_local, n_author_info),
    yes_local_perc = perc_3(n_yes_local, n_author_info)
  )

} # Curly brace for code folding purposes only

# ————————————————————————————————————————————————————
# IMPORT & CLEAN DATA ####
# ————————————————————————————————————————————————————
ie_dat_raw <- read_xlsx("data/ier-published-author-info_2023-12-09.xlsx")

ie_dat <- ie_dat_raw %>% 
  clean_names() %>% 
  mutate(id = as.character(id)) %>% 
  rename(year = year_of_publication) %>% 
  group_by(id) %>% 
  fill(title, country, continent) %>%
  ungroup()

# ————————————————————————————————————————————————————
# NO LOCAL AUTHOR ####
# ————————————————————————————————————————————————————

# ..........................................
## Identify all single-country studies ####
# ..........................................
single_country <- ie_dat %>% 
  group_by(id) %>% 
  filter(n_distinct(na.omit(country)) <= 1) %>% 
  filter(!any(country == "Multi-country")) %>%
  ungroup() %>% 
  select(id, title, authors, author_country, continent, country)

# ..........................................
## Identify all single-country studies for which author country info exists ####
# ..........................................

author_info <- single_country %>% 
  mutate(author_country = case_when(
    author_country %in% c("Not specified",
                          "Not applicable",
                          "Not applicable (no studies)") ~ "Not reported",
    TRUE ~ author_country
  )) %>%
  group_by(id) %>% 
  filter(any(!is.na(author_country))) %>% 
  filter(any(author_country != "Not reported")) %>% 
  ungroup() %>%
  filter(!(is.na(authors) & is.na(author_country))) %>% 
  mutate(n_author_info = n_distinct(id)) 
  
# ..........................................
## Determine % of studies with and without local author (full dataset) ####
# ..........................................

local <- author_info %>%  
 local_y_n() 
  
calc_test <- local %>% 
  perc_local_study()
  
country_perc_test <- local %>% 
  country_perc_local_study()

# I think all the below is redundant. Will delete once confirmed.

# no_local <- author_info %>%  
#   group_by(id) %>% 
#   filter(all(!author_country %in% country | is.na(author_country))) %>%
#   ungroup() %>%
#   mutate(n_no_local = n_distinct(id)) %>%
#   mutate(no_local_perc = round(n_no_local/n_author_info, digits = 3))
# 
# yes_local <- author_info %>% 
#   group_by(id) %>% 
#   filter(any(author_country %in% country | is.na(author_country))) %>% 
#   ungroup() %>% 
#   mutate(n_yes_local = n_distinct(id)) %>% 
#   mutate(yes_local_perc = round(n_yes_local/n_author_info, digits = 3))
# 
# mena_yes_local_value <- yes_local %>% 
#   filter(country %in% mena_list) %>% 
#   mutate(n_mena_local_yes = n_distinct(id)) %>% 
#   select(n_mena_local_yes) %>% 
#   distinct() %>% 
#   pull(n_mena_local_yes)
# 
# yes_local_tot <- yes_local %>% 
#   select(n_yes_local) %>% 
#   distinct() %>% 
#   pull(n_yes_local)
# 
# yes_local_ids <- yes_local %>% 
#   pull(id)

# ..........................................
## Determine prevalence of local FIRST authorship (full dataset) ####
# ..........................................

first_authors <- author_info %>% 
  select(!n_author_info) %>% 
  group_by(id) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

local_first_author <- first_authors %>% 
  filter(id %in% yes_local_ids) %>% 
  mutate(n_yes_local = n_distinct(id)) %>% 
  filter(author_country == country) %>% 
  mutate(n_local_first = n_distinct(id)) %>% 
  mutate(local_first_perc = round(n_local_first/n_yes_local, digits = 3))

# ..........................................
## Collaborations between LMIC/local and HIC authors (full dataset) ####
# ..........................................

# % of all studies that are collaborations between LMIC and HIC authors
collab_lmic <- author_info %>% 
  group_by(id) %>% 
  filter(any(!is.na(author_country) & (author_country %in% hic_list)) &
           any(!is.na(author_country) & (author_country %in% lmic_list))) %>% 
  ungroup() %>% 
  mutate(n_collab_lmic = n_distinct(id)) %>% 
  mutate(collab_lmic_perc = round(n_collab_lmic/n_author_info, digits = 3))

# % of studies that are collaborations between LOCAL and HIC authors (as % of all studies and those with local authors)
collab_local <- yes_local %>% 
  group_by(id) %>% 
  filter(any(author_country %in% hic_list)) %>% 
  ungroup() %>% 
  mutate(n_collab = n_distinct(id)) %>% 
  mutate(collab_perc = round(n_collab/n_author_info, digits = 3)) %>% 
  mutate(collab_local_perc = round(n_collab/n_yes_local, digits = 3))

# % of studies with local and HIC authors where local is first author
collab_local_first <- collab_local %>% 
  group_by(id) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  filter(!author_country %in% hic_list) %>% 
  mutate(n_collab_local_first = n_distinct(id)) %>% 
  mutate(collab_local_first_perc = round(n_collab_local_first/n_collab, digits = 3)) %>% 
  select(!c(n_author_info, n_yes_local, yes_local_perc, collab_perc))

# In local/HIC collaborations, what's the % of local first by country?
collab_country <- collab_local %>% 
  group_by(country) %>% 
  summarise(n_collab_country = n_distinct(id))

local_first_country <- collab_local_first %>% 
  group_by(country) %>%
  summarise(n_loc_first = n_distinct(id))

local_first_country_perc <- full_join(collab_country, local_first_country) %>% 
  replace_na(list(n_loc_first = 0)) %>%
  mutate(loc_first_country_perc = round(n_loc_first/n_collab_country, digits = 3))
  
# ..........................................
## Health checks ####
# ..........................................

test_first <- first_authors %>% 
  filter(id %in% c("26805", "26795"))

test <- single_country %>% 
  group_by(id) %>% 
  filter(any(!is.na(authors) & is.na(author_country))) %>% 
  ungroup()

to_check <- no_local %>% 
  group_by(id) %>% 
  filter(any(!is.na(authors) & is.na(author_country))) %>% 
  ungroup()

test_id <- author_info %>% 
  filter(id == "25148")

# ..........................................
## Determine % of studies with no local author, by region ####
# ..........................................

n_region <- author_info %>% 
  group_by(continent) %>% 
  summarise(n_region = n_distinct(id)) 

n_mena_value <- n_region %>% 
  filter(continent == "Middle East and North Africa") %>% 
  pull(n_region)

no_local_region <- author_info %>% 
  group_by(id) %>% 
  filter(all(!author_country %in% country | is.na(author_country))) %>% 
  group_by(continent) %>% 
  summarise(no_loc_reg = n_distinct(id))

no_local_region_perc <- full_join(n_region, no_local_region) %>% 
  replace_na(list(no_local_region = 0)) %>% 
  mutate(no_loc_reg_perc = round(no_loc_reg/n_region, digits = 3)) 

# ..........................................
## Determine % of studies with no local author, by country ####
# ..........................................

n_country <- author_info %>% 
  group_by(country) %>% 
  summarise(n_country = n_distinct(id))

no_local_country <- author_info %>% 
  group_by(id) %>% 
  filter(all(!author_country %in% country | is.na(author_country))) %>% 
  group_by(country) %>% 
  summarise(no_loc_country = n_distinct(id))

no_local_country_perc <- full_join(n_country, no_local_country) %>% 
  replace_na(list(no_loc_country = 0)) %>% 
  mutate(no_loc_country_perc = round(no_loc_country/n_country, digits = 3))

# ..........................................
## Create country datasets for specific regions ####
# ..........................................

by_reg <- author_info %>% 
  group_by(continent) %>% 
  summarise(id_count = n_distinct(id))

ssa_count <- author_info %>% 
  filter(country %in% ssa_list) %>% 
  mutate(ssa_count = n_distinct(id)) %>% 
  select(ssa_count) %>% 
  distinct()

ssa_count_value <- ssa_count %>% 
  pull(ssa_count)

ssa_no_local_perc <- no_local_country_perc %>% 
  filter(country %in% ssa_list) %>% 
  rename(country_ies = n_country,
         no_local_author = no_loc_country,
         percentage_with_no_local = no_loc_country_perc) %>% 
  filter(country_ies >=50)


ssa_collab <- collab_local %>% 
  filter(country %in% ssa_list) %>% 
  mutate(n_ssa_collab = n_distinct(id)) %>% 
  mutate(ssa_count = ssa_count_value) %>% 
  mutate(ssa_collab_perc = round(n_ssa_collab/ssa_count, digits = 3))

ssa_collab_value <- ssa_collab %>% 
  select(n_ssa_collab) %>% 
  distinct() %>% 
  pull(n_ssa_collab)

ssa_local_first <- local_first_author %>% 
  filter(country %in% ssa_list) %>% 
  mutate(n_ssa_local_first = n_distinct(id)) %>% 
  mutate(n_ssa_collab = ssa_collab_value) %>% 
  mutate(ssa_local_first_perc = round(n_ssa_local_first/n_ssa_collab, digits = 3))

write_xlsx(ssa_no_local_perc, "output_tables/IEs with no local author - SSA by country.xlsx")

mena_no_local_perc <- no_local_country_perc %>% 
  filter(country %in% mena_list) %>% 
  rename(country_ies = n_country,
         no_local_author = no_loc_country,
         percentage_with_no_local = no_loc_country_perc)

write_xlsx(mena_no_local_perc, "output_tables/IEs with no local author - MENA by country.xlsx")

# % of MENA studies that are collaborations between LOCAL and HIC authors (as % of all MENA studies and of MENA studies with local authors)
mena_collab_local <- yes_local %>% 
  group_by(id) %>% 
  filter(any(author_country %in% hic_list)) %>% 
  filter(any(country %in% mena_list)) %>% 
  ungroup() %>% 
  mutate(n_mena = n_mena_value) %>% 
  mutate(n_mena_yes_local = mena_yes_local_value) %>% 
  mutate(n_mena_collab = n_distinct(id)) %>% 
  mutate(mena_collab_perc = round(n_mena_collab/n_mena, digits = 3)) %>% 
  mutate(mena_collab_local_perc = round(n_mena_collab/n_mena_yes_local, digits = 3))

mena_not_iran_collab_local <- mena_collab_local %>% 
  group_by(id) %>% 
  filter(any(country != "Iran, Islamic Rep." & !is.na(country))) %>% 
  ungroup()
  

# ..........................................
## Miscellaneous scraps ####
# ..........................................

ssa_single_country <- author_info %>% 
  group_by(id) %>% 
  filter(any(continent == "Sub-Saharan Africa")) %>% 
  ungroup() %>% 
  mutate(n_ssa = n_distinct(id))

ssa_no_local <- ssa_single_country %>% 
  group_by(id) %>% 
  filter(all(!author_country %in% country | is.na(author_country))) %>% 
  ungroup() %>% 
  mutate(n_ssa_no_local = n_distinct(id)) %>% 
  mutate(no_local_perc = round(n_ssa_no_local/n_ssa, digits = 2))

iran_no_local <- no_local %>% 
  group_by(id) %>% 
  filter(any(country == "Iran, Islamic Rep.")) %>% 
  ungroup()


# ————————————————————————————————————————————————————
# IRAN AUTHORSHIP IN MENA REGION ####
# ————————————————————————————————————————————————————

iran <- ie_dat %>% 
  group_by(id) %>%
  filter(any(continent == "Middle East and North Africa")) %>% 
  # filter(any(country == "Iran, Islamic Rep.")) %>%
  ungroup() %>% 
  select(id, title, author_country, country) %>%
  drop_na(author_country) %>%
  mutate(author_country = case_when(
    author_country != "Iran, Islamic Rep." ~ "Not Iran",
    author_country == "Iran, Islamic Rep." ~ "Iran",
  )) %>% 
  group_by(author_country) %>%
  summarise(author_count = n())

# ————————————————————————————————————————————————————
# SSA AUTHORSHIP ####
# ————————————————————————————————————————————————————

ssa <- ie_dat %>% 
  drop_na(author_country) %>% 
  filter(author_country != "Not reported") %>%
  group_by(id) %>% 
  filter(any(continent == "Sub-Saharan Africa")) %>% 
  mutate(author_ssa = case_when(
    any(author_country %in% ssa_list) ~ "Yes",
    TRUE ~ "No"
  )) %>% 
  ungroup() %>%
  select(id, title, authors, author_country, author_ssa, year, country)


ssa_binary <- ssa %>% 
  group_by(author_ssa) %>% 
  summarise(author_count = n()) %>% 
  mutate(perc = round(author_count/sum(author_count), digits = 2))

ssa_time <- ssa %>% 
  filter(author_ssa == "No") %>% 
  group_by(year) %>% 
  summarise(id_count = n_distinct(id))