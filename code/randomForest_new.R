if (!("librarian" %in% rownames(utils::installed.packages()))) {
  utils::install.packages("librarian")}
librarian::shelf(rattle, tidyverse, haven, mosaic, foreach, stargazer, rpart, rpart.plot, caret, dplyr, mosaic, here, rsample, modelr, purrr, randomForest, randomForestExplainer, gbm, pdp, clusterR, cluster, clue, factoextra, lme4, viridis, ggspatial, basemaps, sf, rgeos, maptools, fdm2id, ggmap, scales, vip, kableExtra)

data = read.csv("r_objects/model_scaled_data.csv")
mod = data %>%
  select(-c(lin_pred, X, PC_outcome, DistName, tot_staff_fte, tot_teach_fte)) %>%
  mutate(RUC.code = as.factor(RUC.code))

## make some forests
over_perf = mod %>% filter(lin_resid > 0)
over_split = initial_split(over_perf, 0.8)
over_train = training(over_split)
over_test = testing(over_split)

#### random forest - over only 
rf_over = randomForest(lin_resid ~ . - n_students -DISTRICT.CUMULATIVE.YEAR.END.ENROLLMENT, data = over_train, na.action = na.omit, mtry = 83, ntree = 50)
yhat_rf = predict(rf_over, newdata = over_test)
yhat_rf = na.omit(yhat_rf)
rmse_rf = sqrt(mean((yhat_rf - over_test$lin_resid)^2))

# create VIP table
imp_table_over = as.data.frame(rf_over$importance) %>% rownames_to_column("feature")
colnames(imp_table_over) = c("feature", "importance")
rf_over_vip = imp_table_over %>% arrange(desc(as.numeric(importance))) %>% top_n(5) 
save(rf_over_vip, file = "r_objects/rf_over_vip.RData")

# pdp of top 5 features by importance
feats = rf_over_vip[,1]
for (i in feats){
plot = partial(rf, pred.var = i, plot = TRUE, plot.engine = "ggplot2") + 
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

#### random forest - over only 
rf_under = randomForest(lin_resid ~ . - n_students -DISTRICT.CUMULATIVE.YEAR.END.ENROLLMENT, data = under_train, na.action = na.omit, mtry = 83, ntree = 50)
yhat_rf = predict(rf_under, newdata = under_test)
yhat_rf = na.omit(yhat_rf)
rmse_rf = sqrt(mean((yhat_rf - under_test$lin_resid)^2))

imp_table_under = as.data.frame(rf_under$importance)
colnames(imp_table_under) = "importance"
imp_table_under = imp_table_under %>% arrange(desc(importance)) %>% rownames_to_column( "feature")

feats = (imp_table_under[1:8,1])
under_plots = for (i in feats){
  plot = partial(rf, pred.var = i, plot = TRUE, plot.engine = "ggplot2") + 
    ggtitle(paste("Partial Dependence Plot of ", i)) + 
    xlab(paste(i)) + 
    ylab("Predicted Resid")
  print(plot)
}
#### random forest - all
rf = randomForest(lin_resid ~ . - n_students -DISTRICT.CUMULATIVE.YEAR.END.ENROLLMENT, data = mod_train, na.action = na.omit, mtry = 83, ntree = 50)
yhat_rf = predict(rf, newdata = mod_test)
yhat_rf = na.omit(yhat_rf)
rmse_rf = sqrt(mean((yhat_rf - mod_test$lin_resid)^2))

# vip table
importance_table = as.data.frame(rf$importance)
colnames(importance_table) = "importance"
importance_table %>% arrange(desc(importance))
write.csv(importance_table, "figures/rf_imp_table.csv")

# partial dependence plots
partial(rf, pred.var = "tot_operating_reevenue", plot = TRUE,
        plot.engine = "ggplot2") + 
  ggtitle("Partial Dependence Plot of Operating Revenue per Student") + 
  xlab("OpRev") + 
  ylab("Predicted Resid")


partial(rf, pred.var = "tot_expend_1819", plot = TRUE,
        plot.engine = "ggplot2") + 
  ggtitle("Partial Dependence Plot of Previous Year Expenditure per Student") + 
  xlab("Expenditure") + 
  ylab("Predicted Resid")

partial(rf, pred.var = "fund_balance", plot = TRUE,
        plot.engine = "ggplot2") + 
  ggtitle("Partial Dependence Plot of Fund Balance per Student") + 
  xlab("Fund Balance") + 
  ylab("Predicted Resid")

partial(rf, pred.var = "st_pct_asian", plot = TRUE,
        plot.engine = "ggplot2") + 
  ggtitle("Partial Dependence Plot of Percent Students Asian") + 
  xlab("Percent") + 
  ylab("Predicted Resid")

partial(rf, pred.var = "teach_pct_black", plot = TRUE,
        plot.engine = "ggplot2") + 
  ggtitle("Partial Dependence Plot of Percent Teachers Black") + 
  xlab("Percent") + 
  ylab("Predicted Resid")

partial(rf, pred.var = "st_pct_hisp", plot = TRUE,
        plot.engine = "ggplot2") + 
  ggtitle("Partial Dependence Plot of Percent Students Hispanic") + 
  xlab("Percent") + 
  ylab("Predicted Resid")

partial(rf, pred.var = "st_pct_ecodis", plot = TRUE,
        plot.engine = "ggplot2") + 
  ggtitle("Partial Dependence Plot of Percent Students \n Economically Disadvantaged") + 
  xlab("Percent") + 
  ylab("Predicted Resid")



