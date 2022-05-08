if (!("librarian" %in% rownames(utils::installed.packages()))) {
  utils::install.packages("librarian")}
librarian::shelf(rattle, tidyverse, haven, mosaic, stargazer, rpart, rpart.plot, caret, dplyr, mosaic, here, rsample, modelr, randomForest, randomForestExplainer, pdp, scales, vip, kableExtra)

data = read.csv("r_objects/model_scaled_data.csv")
mod = data %>%
  select(-c(lin_pred, X.1, X, PC_outcome, DistName, tot_staff_fte, tot_teach_fte, Pop..2020, n_students, DISTRICT.CUMULATIVE.YEAR.END.ENROLLMENT, Enrollment, unemployment_2020)) %>%
  mutate(RUC.code = as.factor(RUC.code))

## make some forests
over_perf = mod %>% filter(lin_resid > 0)
over_split = initial_split(over_perf, 0.8)
over_train = training(over_split)
over_test = testing(over_split)

#### random forest - over only 
rf_over = randomForest(lin_resid ~ . , data = over_train, na.action = na.omit, mtry = 81, ntree = 50)
yhat_rf_over = predict(rf_over, newdata = over_test)
yhat_rf_over = na.omit(yhat_rf_over)
rmse_rf_over = sqrt(mean((yhat_rf_over - over_test$lin_resid)^2))
save(rf_over, file = "r_objects/rf_over.RData")

# create VIP table
imp_table_over = as.data.frame(rf_over$importance) %>% rownames_to_column("feature")
colnames(imp_table_over) = c("feature", "importance")
rf_over_vip = imp_table_over %>% arrange(desc(as.numeric(importance))) %>% top_n(5) 
save(rf_over_vip, file = "r_objects/rf_over_vip.RData")

# pdp of top 5 features by importance
feats_over = rf_over_vip[,1]
for (i in feats_over){
plot = partial(rf_over, pred.var = i, plot = TRUE, plot.engine = "ggplot2") + 
  ggtitle(paste("Partial Dependence Plot of ", i)) + 
  xlab(paste(i)) + 
  ylab("Predicted Resid")
print(plot)
}

## repeat for under performing districts
under_perf = mod %>% filter(lin_resid < 0)
under_split = initial_split(under_perf, 0.8)
under_train = training(under_split)
under_test = testing(under_split)

#### random forest - under only 
rf_under = randomForest(lin_resid ~ . , data = under_train, na.action = na.omit, mtry = 81, ntree = 50)
save(rf_under, file = "r_objects/rf_under.RData")

yhat_rf_under = predict(rf_under, newdata = under_test)
yhat_rf_under = na.omit(yhat_rf_under)
rmse_rf_under = sqrt(mean((yhat_rf_under - under_test$lin_resid)^2))

imp_table_under = as.data.frame(rf_under$importance) %>% rownames_to_column("feature")
colnames(imp_table_under) = c("feature", "importance")
rf_under_vip = imp_table_under %>% arrange(desc(as.numeric(importance))) %>% top_n(5) 
save(rf_under_vip, file = "r_objects/rf_under_vip.RData")

feats_under = rf_under_vip[,1]
under_plots = for (i in feats_under){
  plot = partial(rf_under, pred.var = i, plot = TRUE, plot.engine = "ggplot2") + 
    ggtitle(paste("Partial Dependence Plot of ", i)) + 
    xlab(paste(i)) + 
    ylab("Predicted Resid")
  print(plot)
}

#repeat for all districts
mod_split = initial_split(mod, 0.8)
mod_train = training(mod_split)
mod_test = testing(mod_split)

#### random forest - all
rf = randomForest(lin_resid ~ ., data = mod_train, na.action = na.omit, mtry = 83, ntree = 50)
yhat_rf = predict(rf, newdata = mod_test)
yhat_rf = na.omit(yhat_rf)
rmse_rf = sqrt(mean((yhat_rf - mod_test$lin_resid)^2))
save(rf, file = "r_objects/rf.RData")

# vip table
importance_table = as.data.frame(rf$importance) %>% rownames_to_column("feature") 
colnames(importance_table) = c("feature", "importance")
importance_table = importance_table %>% arrange(desc(as.numeric(importance))) %>% top_n(5) 
write.csv(importance_table, "figures/rf_imp_table.csv")
save(importance_table, file = "r_objects/rf_vip.RData")

# pdp 
# top 5 partial dependence plots 
feats_all = importance_table[,1]
par(mfrow = c(3,2))
for (i in feats_all){
  plot = pdp::partial(rf, pred.var = i, plot = TRUE, plot.engine = "ggplot2") + 
    ggtitle(paste("Partial Dependence Plot of ", i)) + 
    xlab(paste(i)) + 
    ylab("Predicted Resid")
  print(plot)
}

#save training data
save(over_train,under_train,mod_train, file = "r_objects/training_data.RData")
save.image(file = "all_objects.RData")
