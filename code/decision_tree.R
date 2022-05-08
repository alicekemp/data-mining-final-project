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
                control = rpart.control(minsplit=10, minbucket = 10, cp=.02, xval=5))
fancyRpartPlot(ed_tree)
## vip 
vip_tree = data.frame(ed_tree$variable.importance)
tree_vip = vip_tree %>% rownames_to_column( "feature") 
colnames(tree_vip) = c("feature", "importance")
tree_vip_top = tree_vip %>% top_n(8) 
save(tree_vip_top, file = "r_objects/tree_vip.Rdata")
save(ed_tree, file = "r_objects/tree.Rdata")

#plotcp(ed_tree)
#printcp(ed_tree)