library(tidyverse)
library(readr)

sat_district_data_class_2020 <- read_csv("data/sat-district-data-class-2020.csv")
sat_district_data_class_2020$CntyName = 
  substr(sat_district_data_class_2020$CntyName, 1, 
         nchar(sat_district_data_class_2020$CntyName)-7) #getting rid of "County"
numeric_categories = colnames(sat_district_data_class_2020)[8:17]
#c(Grads_Mskd, Exnees_Mskd, Crit_Mskd, TSI_Both_Mskd)
## addressing masking (< symbols, just changing <1000 to 1000)
sat_district_data_class_2020 = sat_district_data_class_2020 %>%
  mutate(across(.cols =everything(), .fns = ~str_replace(.x,',',''))) %>% #commas suck
  mutate(across(.cols = c(Grads_Mskd, Exnees_Mskd, Crit_Mskd, TSI_Both_Mskd), 
                .fns = ~ifelse(substr(.x,1,1)=='<', as.numeric(substr(.x,2,nchar(.x))), as.numeric(.x)))) %>%
  mutate(across(.cols = numeric_categories, .fns = ~as.numeric(.x)))


######################## checking some stuff
df = sat_district_data_class_2020 %>%
  filter(substr(Grads_Mskd,1,1) != '<') %>%
  mutate(Grads_Mskd = as.numeric(Grads_Mskd)) %>%
  filter(is.na(Grads_Mskd)) %>%
  select(c(Group, DistName, Grads_Mskd)) %>%
  filter(DistName == 'Killeen ISD')

df2 = sat_district_data_class_2020 %>%
  filter(DistName == 'Killeen ISD') %>%
  select(c(Group, DistName, Grads_Mskd))

dfmerge = full_join(df,df2, 'Group')

sapply(sat_district_data_class_2020, function(x) sum(is.na(x)))
sapply(sat_district_data_class_2020, class)
###############################

# county pop merge
USDA_county_pops_2020 <- read_csv("data/USDA-county-pops-2020.csv")

USDA_county_pops_2020 = USDA_county_pops_2020 %>% 
  rename('CntyName'='County name') %>%
  select(c('FIPS*', CntyName, 'RUC code', 'Pop. 2020', 'Change 2010-20')) %>%
  mutate(`Change 2010-20` = as.numeric(substr(`Change 2010-20`,1,nchar(`Change 2010-20`)-1))) %>%
  rename('Change_2010-20_pct' = 'Change 2010-20')

merged = inner_join(sat_district_data_class_2020, USDA_county_pops_2020, by = "CntyName")

#county poverty merge
USDA_county_poverty_2019 <- read_csv("data/USDA-county-poverty-2019.csv", skip = 2)
USDA_county_poverty_2019 = USDA_county_poverty_2019 %>%
  rename('total_poverty_percent' = 'Percent...7') %>%
  rename('child_poverty_percent' = 'Percent...10') %>%
  select('FIPS*', total_poverty_percent, child_poverty_percent)

merged = inner_join(merged, USDA_county_poverty_2019, by = "FIPS*") %>%
  rename('FIPS' = 'FIPS*')

#county education level merge
USDA_Education_level_college_2020 <- read_csv("data/USDA-Education-level-college-2020.csv")
USDA_Education_level_college_2020 = USDA_Education_level_college_2020 %>%
  head(255) %>%
  rename('Percent_college_grads_county_19' = '2015-2019') %>%
  mutate(Percent_college_grads_county_19 = as.numeric(substr(Percent_college_grads_county_19,1,nchar(Percent_college_grads_county_19)-1))) %>%
  mutate(FIPS = as.numeric(FIPS)) %>%
  select(FIPS, Percent_college_grads_county_19)

merged = inner_join(merged, USDA_Education_level_college_2020, by='FIPS')

#county income & unemployment merge
USDA_income_unemployment_2019_2020 <- 
  read_csv("data/USDA-income-unemployment-2019-2020.csv", skip = 1)
USDA_income_unemployment_2019_2020 = USDA_income_unemployment_2019_2020 %>%
  rename('unemployment_2019' = '2019') %>%
  rename('unemployment_2020' =  '2020') %>%
  rename('pct_state_median_HH_income' = '% of State Median HH Income') %>%
  mutate(pct_state_median_HH_income = as.numeric(substr(pct_state_median_HH_income,1,nchar(pct_state_median_HH_income)-1))) %>%
  select(FIPS, unemployment_2019, unemployment_2020, pct_state_median_HH_income) %>%
  mutate(FIPS = as.numeric(FIPS))

merged = inner_join(merged, USDA_income_unemployment_2019_2020, by='FIPS')

#school nutrition merge, merge starts with dim 57228 x 27
School_Meals_2019_20 <- 
  read_csv("data/School_Nutrition_Programs_-_Meal_Reimbursement_Information_-_Program_Year_2019-2020.csv")
School_Meals_2019_20 = School_Meals_2019_20 %>%
  select(ProgramYear, CEName, SiteID, SiteCounty, SiteName, EnrollmentQty, 
         FreeEligQty, RedcEligQty, PaidEligQty, TotalReimbursement, BreakfastDays, 
         LunchDays, SnackDays)

        #losing granularity here, averaging over the school district and not the school
School_Meals_Summary = School_Meals_2019_20 %>%
  rename('DistName' = 'CEName') %>%
  group_by(DistName)%>%
  summarise(Enrollment = mean(EnrollmentQty), FreeElig_mean = mean(FreeEligQty),
            RedcEligQty_mean = mean(RedcEligQty), PaidEligQty_mean = mean(PaidEligQty),
            TotalReimbursement_sum = sum(TotalReimbursement))

merged$DistName = toupper(merged$DistName)
merged = left_join(merged, School_Meals_Summary, by='DistName')

#merged_SchoolMeals = left_join(merged, School_Meals_Summary, by='DistName')

#abuse_neglect data merge (currently using 2020 data, but 2012-2021 is there)
abuse_neglect <- read_csv("data/CCI_4.2_Completed_Abuse_Neglect_Residential_Child_Care_Investigations__RCCI__FY2012-FY2021.csv")


abuse_neglect_2020 = abuse_neglect %>%
  filter(`Fiscal Year` ==  2020) %>%
  rename('CntyName'='County') %>%
  group_by(CntyName) %>%
  summarise(abuse_neglect_investigations = sum(`Completed RCCI Abuse/Neglect Investigations`))

merged = left_join(merged,abuse_neglect_2020,by='CntyName') #57228 x 33

#discipline merge
# -999 indicates counts or percentages are not available (i.e. masked) to comply 
# with Family Educational Rights and Privacy Act (FERPA).
TEA_discipline <- read_csv("data/TEA-2019-2020-discipline-data.csv") %>%
  rename('discipline_heading'='HEADING NAME')


## reshape to widen the data frame so there is 1 obs per district
TEA_discipline_adj = TEA_discipline %>%
  select(-c(`AGGREGATION LEVEL`,`REGION`,CHARTER_STATUS, SECTION, HEADING))
TEA_discipline_adj = TEA_discipline_adj %>%
  pivot_wider(id_cols = c(DISTNAME,DISTRICT), 
              names_from = discipline_heading, 
              values_from = YR20, 
              values_fn = sum)

## check where null values appear and remove columns with more than 230 nulls
null_list = as.data.frame(sapply(TEA_discipline_adj, function(x) sum(is.na(x))))
nullDF = list1 %>%
  filter(list1[1] <= 230)
valid_rownames =  rownames(nullDF)  

TEA_discipline_final = TEA_discipline_adj %>%
  select(valid_rownames) %>%
  select(-c(`HISPANIC/LATINO`, `WHITE`)) %>% #manually remove these because they are weird
  as.data.frame() %>%
  rename('DistName'='DISTNAME') %>%
  rename('District'='DISTRICT') %>%
  mutate(DistName = as.character(ifelse(DistName == "D'HANIS ISD", "D'HANIS SCHOOL", DistName))) %>%
  mutate(across(.cols = everything(), .fns = ~ifelse(.x == -999, NA, .x))) #-999 missing code into NAs

names(TEA_discipline_final) = make.names(colnames(TEA_discipline_final), unique = TRUE)

merged = left_join(merged,TEA_discipline_final,by=c('District', 'DistName')) #57288 x 53

#remove spaces in names
names(merged) = make.names(colnames(merged))
#i dont think the ATUS has Texas data
#what is Book2?


# random checking of stuff
testing = merged
testing = testing %>%
  mutate(across(.cols = everything(), .fns = ~ifelse(is.na(.x), 0,.x)))
sapply(testing, function(x) sum(x<0))

#making codebook
na_list = as.data.frame(sapply(merged, function(x) sum(is.na(x))))
class_list = as.data.frame(sapply(merged, class))
na_frame = merge(na_list,class_list,by=0)
names(na_frame) =  c('Variable','NA_count','object_class')

#saving objects
saveRDS(merged, file='r_objects/cleaned_ed_data.Rda') #can load in using load()
saveRDS(na_frame, file = 'r_objects/variable_NA_class.Rda')

write_csv(merged, 'r_objects/cleaned_ed_data')

