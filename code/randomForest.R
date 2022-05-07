if (!("librarian" %in% rownames(utils::installed.packages()))) {
  utils::install.packages("librarian")}
librarian::shelf(rattle, tidyverse, haven, mosaic, foreach, stargazer, rpart, rpart.plot, caret, dplyr, mosaic, here, rsample, modelr, purrr, randomForest, randomForestExplainer, gbm, pdp, clusterR, cluster, clue, factoextra, lme4, viridis, ggspatial, basemaps, sf, rgeos, maptools, fdm2id, ggmap, scales, vip, kable, kableExtra)

# import cleaned dataset
ed = read.csv("r_objects/model_data.csv")
head(ed)

# select top 20% performing districts by performance score
set.seed(666)
ed_top20 = ed %>%
  slice_max(.,order_by = PC_outcome, prop = 0.2)
 
edtop_split = initial_split(ed_top20, 0.8)
edtop_train = training(edtop_split)
edtop_test = testing(edtop_split)
head(edtop_train)
#### decision tree
tree_top = rpart(PC_outcome ~ . -DistName - X, method="anova",data=edtop_train,
                            control=rpart.control(minsplit=5, minbucket = 20, cp=1e-6, xval=5))
#bestcp=tree_top$cptable[which.min(tree_top$cptable[,"xerror"]),"CP"]
#tree_top2 = prune(tree_top,cp=bestcp)
fancyRpartPlot(tree_top)


#### random forest
rf_top = randomForest(PC_outcome ~ ., data = edtop_train, na.action = na.omit, mtry = 20, ntree = 50)
yhat_rftop = predict(rf_top, newdata = edtop_test)
yhat_rftop = na.omit(yhat_rftop)
rmse_rftop = sqrt(mean((yhat_rftop - edtop_test$PC_outcome)^2))

# select bottom 20% performing districts by performance score
ed_bot20 = ed %>%
  slice_min(.,order_by = PC_outcome, prop = 0.2)

edbot_split = initial_split(ed_bot20, 0.8)
edbot_train = training(edbot_split)
edbot_test = testing(edbot_split)
rf_bot = randomForest(PC_outcome ~ ., data = edbot_train, na.action = na.omit, mtry = 20, ntree = 50)

yhat_rfbot = predict(rf_bot, newdata = edbot_test)
yhat_rfbot = na.omit(yhat_rfbot)
rmse_rfbot = sqrt(mean((yhat_rfbot - edbot_test$PC_outcome)^2))

# repeat for all districts
ed_split = initial_split(ed, 0.8)
ed_train = training(ed_split)
ed_test = testing(ed_split)
rf = randomForest(PC_outcome ~ ., data = ed_train, na.action = na.omit, mtry = 90, ntree = 50)

yhat_rf = predict(rf, newdata = ed_test)
yhat_rf = na.omit(yhat_rf)
rmse_rf = sqrt(mean((yhat_rf - ed_test$PC_outcome)^2))
