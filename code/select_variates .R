library(tidyverse)

m3 = read.csv("r_objects/testing_PCA.csv") %>%
  select(-c(Above_TSI_Both_Rate, Above_Crit_Rate, avg_act_1819, avg_sat_1819, ann_grad_count_1819, Total, Math, ERW, Group, District, County, CntyName, Region, RegnName, Grads_Mskd, Exnees_Mskd, Part_Rate, Crit_Mskd, TSI_Both_Mskd, FIPS, TotalReimbursement_sum, DISTRICT.DISCIPLINE.POPULATION, DistDesc, n_schools, attendance_rate, ann_dropout_rate, district_size, community_type, property_wealth, tax_rate, X))


m3 = m3[,-c(19:33)]

m4 = m3 %>%
  mutate(ani_per_stu = abuse_neglect_investigations / n_students,
         st_pct_minority = st_pct_black + st_pct_hisp + st_pct_native + st_pct_asian + st_pct_pac + st_pct_mult, 
         teach_pct_minority = teach_pct_black + teach_pct_hisp + teach_pct_native + teach_pct_asian + teach_pct_pac + teach_pct_mult, 
         st_to_teach_minority = abs(st_pct_minority - teach_pct_minority))

write.csv(m4, "r_objects/model_data.csv")