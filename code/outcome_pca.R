#Read me in
m2 = read.csv("r_objects/v2_cleaned_ed_data.csv")

m2[100:110,]

m3 = m2 %>%
  filter(Group == "All Students")

#recoding
m3 = m3 %>% 
  mutate(avg_sat_1819 = ifelse(avg_sat_1819 == -1, NA, avg_sat_1819)) %>% 
  mutate(avg_act_1819 = ifelse(avg_act_1819 == -1, NA, avg_act_1819)) %>% 
  mutate(avg_salary_prof = ifelse(avg_salary_prof == -2, NA, avg_salary_prof)) %>% 
  mutate(stud_teach_ratio = ifelse(stud_teach_ratio == -2.0, NA, stud_teach_ratio)) %>% 
  mutate(avg_salary_central = ifelse(avg_salary_central == -2.0, NA, avg_salary_central)) %>% 
  mutate(Charter = ifelse(Charter == "Y", 1,0)) %>% 
  mutate(attendance_rate = ifelse(attendance_rate == -1.0, NA, attendance_rate)) %>%
  mutate(avg_salary_school = ifelse(avg_salary_school == 1, NA, avg_salary_school)) %>% 
  mutate(tot_other_rev = ifelse(tot_other_rev == -11711, NA, 0))

testing = m3
testing = testing %>%
  mutate(across(.cols = everything(), .fns = ~ifelse(is.na(.x), 0,.x)))
sapply(testing, function(x) sum(x<0))

#muligan
testing <- testing %>%
  select(ERW, everything()) %>% 
  select(Math, everything()) %>% 
  select(Total, everything()) %>% 
  select(ann_grad_count_1819, everything()) %>% 
  select(avg_sat_1819, everything()) %>% 
  select(avg_act_1819, everything()) %>% 
  select(Above_Crit_Rate, everything()) %>% 
  select(Above_TSI_Both_Rate, everything())
  
#PCA
outcomes_PCA_table = prcomp(testing[,(1:8)], scale=FALSE, rank=1)

outcomes_PCA = as.data.frame(outcomes_PCA_table[["x"]])

outcome_PCA_variance_plot = plot(outcomes_PCA)

testing_PCA = cbind(testing, outcomes_PCA)

names(testing_PCA)[names(testing_PCA) == 'PC1'] <- 'PC_outcome'

testing_PCA <- testing_PCA %>%
  select(PC_outcome, everything())
