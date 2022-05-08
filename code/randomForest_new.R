if (!("librarian" %in% rownames(utils::installed.packages()))) {
  utils::install.packages("librarian")}
librarian::shelf(rattle, tidyverse, haven, mosaic, foreach, stargazer, rpart, rpart.plot, caret, dplyr, mosaic, here, rsample, modelr, purrr, randomForest, randomForestExplainer, gbm, pdp, clusterR, cluster, clue, factoextra, lme4, viridis, ggspatial, basemaps, sf, rgeos, maptools, fdm2id, ggmap, scales, vip, kable, kableExtra)

data = read.csv("r_objects/merged_data_pred_resid.csv")

## scale count variables
data$PaidEligQty_mean = data$PaidEligQty_mean / data$n_students
data$FreeElig_mean = data$FreeElig_mean / data$n_students
data$RedcEligQty_mean = data$RedcEligQty_mean / data$n_students
data$abuse_neglect_investigations = data$abuse_neglect_investigations / data$n_students
data$DISTRICT.DISCIPLINE.RECORD.COUNT = data$DISTRICT.DISCIPLINE.RECORD.COUNT / data$n_students
data$X05.OUT.OF.SCHOOL.SUSPENSION = data$X05.OUT.OF.SCHOOL.SUSPENSION / data$n_students
data$X06.IN.SCHOOL.SUSPENSION = data$X06.IN.SCHOOL.SUSPENSION / data$n_students
data$tot_operating_reevenue = data$tot_operating_reevenue / data$n_students
data$tot_other_rev = data$tot_other_rev / data$n_students
data$fund_balance = data$fund_balance / data$n_students
data$net_assets_charter = data$net_assets_charter / data$n_students
data$tot_expend_1819 = data$tot_expend_1819 / data$n_students

write.csv(data, "data/model_scaled_data.csv")

mod = data %>%
  select(-c(lin_pred, X, PC_outcome, DistName, tot_staff_fte, tot_teach_fte)) %>%
  mutate(RUC.code = as.factor(RUC.code))

set.seed(123)
mod_split = initial_split(mod, 0.8)
mod_train = training(mod_split)
mod_test = testing(mod_split)

## make some trees
ed_tree = rpart(lin_resid ~ ., method = "anova", data=mod_train,
                   control = rpart.control(minsplit=10, minbucket = 10, cp=.02, xval=5))
ed_tree$variable.importance
fancyRpartPlot(ed_tree)
# prune the tree based on the optimal cp value
plotcp(ed_tree)
printcp(ed_tree)

## make some forests
#### random forest
rf = randomForest(lin_resid ~ . - n_students -DISTRICT.CUMULATIVE.YEAR.END.ENROLLMENT, data = mod_train, na.action = na.omit, mtry = 20, ntree = 50)
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



