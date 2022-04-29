ed1 = read.csv("r_objects/cleaned_ed_data.csv")

dist_desc = read.csv("data/TEA-19-20-desc.csv") %>%
  select(District, NCES.Description, Charter.School..Y.N.)

colnames(dist_desc) = c("DistName", "DistDesc", "Charter") 


ed2 = merge(ed1, dist_desc, by = "DistName")

ed3_names = read.csv("data/district_data_19-20.csv") %>%
  select(DistName)
ed3 = read.csv("data/district_data_19-20.csv")
numerics = colnames(ed3)[-c(73:76)]

ed3_num = ed3 %>%
  mutate(across(numerics, .fns = ~as.numeric(.x)))

ed3_merge = cbind(ed3_names, ed3_num)
ed3_merge = ed3_merge[,-2]
ed_merged2 = merge(ed2, ed3_merge, by = "DistName")

write_csv(ed_merged2, 'r_objects/v2_cleaned_ed_data.csv')

