library(tidyverse)
library(readr)

sat_district_data_class_2020 <- read_csv("data/sat-district-data-class-2020.csv")
sat_district_data_class_2020$CntyName = 
  substr(sat_district_data_class_2020$CntyName, 1, nchar(sat_district_data_class_2020$CntyName)-7)

# county pop merge
USDA_county_pops_2020 <- read_csv("data/USDA-county-pops-2020.csv")

USDA_county_pops_2020 = USDA_county_pops_2020 %>% 
  rename('CntyName'='County name') %>%
  select(c('FIPS*', CntyName, 'RUC code', 'Pop. 2020', 'Change 2010-20'))

merged = inner_join(sat_district_data_class_2020, USDA_county_pops_2020, by = "CntyName")

#county poverty merge
USDA_county_poverty_2019 <- read_csv("data/USDA-county-poverty-2019.csv", skip = 2)
USDA_county_poverty_2019 = USDA_county_poverty_2019 %>%
  rename('total_poverty_percent' = 'Percent...7') %>%
  rename('child_poverty_percent' = 'Percent...10') %>%
  select('FIPS*', Name, 'RUC Code', total_poverty_percent, child_poverty_percent)

merged = inner_join(merged, USDA_county_poverty_2019, by = "FIPS*")

#county education level merge

