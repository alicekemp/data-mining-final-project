#pack
library(rsample)
library(modelr)

#load data
ed = read.csv("r_objects/model_data.csv")

#col select
ed = ed %>% 
  select(PC_outcome, child_poverty_percent, pct_state_median_HH_income, avg_salary_school,  st_pct_black, st_pct_hisp, st_pct_white, st_pct_asian, st_pct_native, st_pct_pac, st_pct_mult)

ed <- as.data.frame(lapply(ed, as.numeric))

#train/test
ed_split = initial_split(ed, 0.8)
ed_train = training(ed_split)
ed_test = testing(ed_split)

#linear reg
lin_mod = lm(PC_outcome ~ child_poverty_percent + pct_state_median_HH_income + avg_salary_school + st_pct_black + st_pct_hisp + st_pct_white + st_pct_asian + st_pct_native + st_pct_pac + st_pct_mult, data = ed_train)

plot(lin_mod)
summary(lin_mod)
summary_stats(lin_mod)

#test
pred1 = predict(lin_mod, newdata = ed_test)
rmse(lin_mod, ed_test)
# rmse <- sqrt(sum((exp(pred1) - ed_test$PC_outcome)^2)/length(ed_test$PC_outcome))
# c(RMSE = rmse, R2=summary(lin_mod)$r.squared)

#cross valid
ed_folds = crossv_kfold(ed, k=10)

fold_mod = map(ed_folds$train, ~ lm(PC_outcome ~ child_poverty_percent + pct_state_median_HH_income + avg_salary_school + st_pct_black + st_pct_hisp + st_pct_white + st_pct_asian + st_pct_native + st_pct_pac + st_pct_mul, data=.))

map2_dbl(fold_mod, ed_folds$test, modelr::rmse) %>% mean