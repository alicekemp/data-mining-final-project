#pack
library(tidyverse)
library(rsample)
library(modelr)

#load data
ed = read.csv("r_objects/testing_PCA.csv")

#col select
ed_lin = ed %>% 
  select(PC_outcome, child_poverty_percent, pct_state_median_HH_income, avg_salary_school,  st_pct_black, st_pct_hisp, st_pct_white, st_pct_asian, st_pct_native, st_pct_pac, st_pct_mult)

ed_lin <- as.data.frame(lapply(ed, as.numeric))

#linear reg
lin_mod = lm(PC_outcome ~ child_poverty_percent + pct_state_median_HH_income + avg_salary_school + st_pct_black + st_pct_hisp + st_pct_white + st_pct_asian + st_pct_native + st_pct_pac + st_pct_mult, data = ed)

plot(lin_mod)
summary(lin_mod)
summary_stats(lin_mod)

#test
ed$lin_pred = predict(lin_mod, data = ed)
ed$lin_resid = resid(lin_mod)

rmse(lin_mod, ed_test)

write_csv(ed, "r_objects/merged_data_pred_resid.csv")


# rmse <- sqrt(sum((exp(pred1) - ed_test$PC_outcome)^2)/length(ed_test$PC_outcome))
# c(RMSE = rmse, R2=summary(lin_mod)$r.squared)
# 
# #cross valid
# ed_folds = crossv_kfold(ed, k=10)
# 
# fold_mod = map(ed_folds$train, ~ lm(PC_outcome ~ child_poverty_percent + pct_state_median_HH_income + avg_salary_school + st_pct_black + st_pct_hisp + st_pct_white + st_pct_asian + st_pct_native + st_pct_pac + st_pct_mul, data=.))
# 
# map2_dbl(fold_mod, ed_folds$test, modelr::rmse) %>% mean