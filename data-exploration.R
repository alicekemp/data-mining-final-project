library(tidyverse)
library(ggplot2)

sat_district_data_class_2020 <- read.csv("data/sat-district-data-class-2020.csv")
disadvantaged = sat_district_data_class_2020 %>%
  filter(Group == "Economically Disadvantaged")
censusu_poverty_estimates_2020 <- read_csv("data/censusu-poverty-estimates-2020.csv", skip = 3)
poverty_abridged_tx =  censusu_poverty_estimates_2020[,1:10] %>% 
  rename(CntyName =  Name) %>% 
  rename('PostalCode'='Postal Code') %>%
  rename('Poverty_Percent_All_Ages' = 'Poverty Percent, All Ages') %>%
  filter(PostalCode == 'TX') %>% filter(CntyName != 'Texas')
merged = inner_join(disadvantaged, poverty_abridged_tx, by = "CntyName")

colnames(merged)
lm1 =  lm(Math ~ Poverty_Percent_All_Ages, data = merged)
math_hat = predict(lm1,merged)
merged =  merged%>%
  mutate(math_pred =  math_hat) %>%
  mutate(resid = (Math - math_pred))

plot(lm1) #it's a straight line :(

ggplot(merged, aes(x=Poverty_Percent_All_Ages, y=Math)) +
  geom_smooth()

ggplot(merged, aes(x=Poverty_Percent_All_Ages, y=resid)) +
    geom_smooth()
  
  