if (!("librarian" %in% rownames(utils::installed.packages()))) {
  utils::install.packages("librarian")}
librarian::shelf(rattle, tidyverse, haven, mosaic, stargazer, rpart, rpart.plot, caret, dplyr, mosaic, here, rsample, modelr, randomForest, randomForestExplainer,pdp, scales, vip, kableExtra)

mod = read.csv("r_objects/model_scaled_data.csv")

set.seed(123)
mod_split = initial_split(mod, 0.8)
mod_train = training(mod_split)
mod_test = testing(mod_split)

## make some trees
ed_tree = rpart(lin_resid ~ ., method = "anova", data=mod_train,
                control = rpart.control(minsplit=20, minbucket = 20, cp=.02, xval=5))
rpart.plot(ed_tree, type = 2, digits = 2, varlen = -15, main = "Fig 1: CART for Predicted Residuals")

yhat_tree = predict(ed_tree, newdata = mod_test)
yhat_tree = na.omit(yhat_tree)
rmse_tree = sqrt(mean((yhat_tree - mod_test$lin_resid)^2))
## vip 
vip_tree = data.frame(ed_tree$variable.importance)
tree_vip = vip_tree %>% rownames_to_column( "feature") 
colnames(tree_vip) = c("feature", "importance")
tree_vip_top = tree_vip %>% top_n(8) 
save(tree_vip_top, file = "r_objects/tree_vip.Rdata")
save(ed_tree, file = "r_objects/tree.Rdata")

#plotcp(ed_tree)
#printcp(ed_tree)