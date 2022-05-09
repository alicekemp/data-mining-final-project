#pack
library(tidyverse)
library(rsample)
library(modelr)
library(ggplot2)
library(grid)
library(gridExtra)

#load data
ed = read.csv("r_objects/model_data.csv")

#col select
ed_lin = ed %>% 
  select(PC_outcome, child_poverty_percent, pct_state_median_HH_income, avg_salary_school,  st_pct_black, st_pct_hisp, st_pct_white, st_pct_asian, st_pct_native, st_pct_pac, st_pct_mult)

#ed_lin <- as.data.frame(lapply(ed, as.numeric))

#linear reg
lin_mod = lm(PC_outcome ~ child_poverty_percent + pct_state_median_HH_income + avg_salary_school + st_pct_black + st_pct_hisp + st_pct_white + st_pct_asian + st_pct_native + st_pct_pac + st_pct_mult, data = ed)

plot(lin_mod)
summary(lin_mod)

#test
ed$lin_pred = predict(lin_mod, data = ed)
ed$lin_resid = resid(lin_mod)

ed <- ed %>%
  select(lin_pred, everything()) %>% 
  select(lin_resid, everything())

rmse(lin_mod, ed)

write_csv(ed, "r_objects/merged_data_pred_resid.csv")
save(lin_mod, file = 'r_objects/lin_mod.RData')


predVactual = ggplot(ed) +
  geom_point(aes(x=lin_pred,y=PC_outcome)) +
  geom_abline(slope = 1,intercept = 0,color="red") +
  xlab("Predicted Outcome (LM)") +
  ylab("Actual Outcome (PC)")
residVactual = ggplot(ed) +
  geom_point(aes(x=lin_resid,y=PC_outcome)) +
  geom_hline(yintercept = 0,color = 'red')+
  xlab("Predicted Outcome (LM)") +
  ylab("Actual Outcome (PC)")
grid.arrange(predVactual,residVactual,ncol=2,top="Linear Model Predictions and Residuals")
predVactual
residVactual


ggplot(ed) +
  geom_point(aes(x=PC_outcome,y=lin_pred)) +
  geom_hline(yintercept = 0,color = 'red')
  
  
plot(ed$lin_resid,ed$PC_outcome)
  
  
  
  
