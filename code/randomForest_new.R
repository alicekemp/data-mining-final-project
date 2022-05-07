if (!("librarian" %in% rownames(utils::installed.packages()))) {
  utils::install.packages("librarian")}
librarian::shelf(rattle, tidyverse, haven, mosaic, foreach, stargazer, rpart, rpart.plot, caret, dplyr, mosaic, here, rsample, modelr, purrr, randomForest, randomForestExplainer, gbm, pdp, clusterR, cluster, clue, factoextra, lme4, viridis, ggspatial, basemaps, sf, rgeos, maptools, fdm2id, ggmap, scales, vip, kable, kableExtra)

data = read.csv("r_objects/merged_data_pred_resid.csv") 
head(data)

set.seed(123)
data_split = initial_split(data, 0.8)
data_train = training(data_split)
data_test = testing(data_split)

## make some trees
ed_tree = rpart(lin_resid ~ . - X - PC_outcome - DistName, method = "anova", data=data_train,
                   control = rpart.control(minsplit=10, minbucket = 10, cp=.01, xval=5))
fancyRpartPlot(ed_tree)
# prune the tree based on the optimal cp value
rpart::plotcp(ed_tree)

## make some forests
#### random forest
rf = randomForest(lin_resid ~ . - X - PC_outcome - DistName, data = data_train, na.action = na.omit, mtry = 20, ntree = 50)
yhat_rf = predict(rf, newdata = data_test)
yhat_rf = na.omit(yhat_rf)
rmse_rf = sqrt(mean((yhat_rf - data_test$lin_resid)^2))

partial(rf, pred.var = "st_pct_asian", plot = TRUE,
        plot.engine = "ggplot2") + 
  ggtitle("Partial Dependence Plot of Percent Students Asian") + 
  xlab("Percent") + 
  ylab("Predicted Resid")

partial(rf, pred.var = "st_pct_ecodis", plot = TRUE,
        plot.engine = "ggplot2") + 
  ggtitle("Partial Dependence Plot of Percent Students Economically Disadvantaged") + 
  xlab("Percent") + 
  ylab("Predicted Resid")
