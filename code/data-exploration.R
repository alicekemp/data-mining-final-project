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

#generate basic residuals, Math scores
lm1 =  lm(Math ~ Poverty_Percent_All_Ages, data = merged)
math_hat = predict(lm1,merged)
merged_math =  merged%>%
  mutate(math_pred =  math_hat) %>%
  mutate(resid = (Math - math_pred))

plot(lm1) #it's a straight line :(

ggplot(merged_math, aes(x=Poverty_Percent_All_Ages, y=Math)) +
  geom_smooth()

ggplot(merged_math, aes(x=Poverty_Percent_All_Ages, y=resid)) +
  geom_point() +
  geom_smooth()

#TSI readiness
lm2 =  lm(Above_TSI_Both_Rate ~ Poverty_Percent_All_Ages, data = merged)
math_hat = predict(lm2,merged)
merged_TSI =  merged%>%
  mutate(TSI_pred =  math_hat) %>%
  mutate(resid = (Above_TSI_Both_Rate - TSI_pred))

plot(lm2) #it's a straight line :(

ggplot(merged_TSI, aes(x=Poverty_Percent_All_Ages, y=Above_TSI_Both_Rate)) +
  geom_smooth()

ggplot(merged_TSI, aes(x=Poverty_Percent_All_Ages, y=resid)) +
  geom_point() +
  geom_smooth()

merged_TSI = merged_TSI %>%
  mutate(over_TSI =  ifelse(resid > 50, 1, 0))
###################

#computing difference in SAT Math scores for economically disadvantaged students and not
disadvantaged_delta = sat_district_data_class_2020 %>%
  filter(Group == ("Economically Disadvantaged") | Group == "Not Economically Disadvantaged") %>%
  group_by(DistName) %>%
  mutate(delta_math = c((-1)*diff(Math),0)) %>%
  filter(Group == ("Economically Disadvantaged"))
merged_delta = inner_join(disadvantaged_delta, poverty_abridged_tx, by = "CntyName")
lm3 = lm(delta_math ~ Poverty_Percent_All_Ages, data= merged_delta)

plot(lm3)
ggplot(merged_delta, aes(x=Poverty_Percent_All_Ages, y=delta_math)) +
  geom_point() +
  geom_smooth()
