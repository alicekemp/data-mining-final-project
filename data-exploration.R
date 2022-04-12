library(tidyverse)
library(ggplot2)

sat_district_data_class_2020 <- read.csv("sat-district-data-class-2020.csv")
disadvantaged = sat_district_data_class_2020 %>%
  filter(Group == "Economically Disadvantaged")
censusu_poverty_estimates_2020 <- read_csv("censusu-poverty-estimates-2020.csv", skip = 3)
poverty_abridged_tx =  censusu_poverty_estimates_2020[,1:10] %>% 
  rename(CntyName =  Name) %>% 
  rename('PostalCode'='Postal Code') %>% 
  filter(PostalCode == 'TX') %>% filter(CntyName != 'Texas')
merged = inner_join(disadvantaged, poverty_abridged_tx, by = "CntyName")

