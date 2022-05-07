if (!("librarian" %in% rownames(utils::installed.packages()))) {
  utils::install.packages("librarian")}
librarian::shelf(rattle, tidyverse, haven, mosaic, foreach, stargazer, rpart, rpart.plot, caret, dplyr, mosaic, here, rsample, modelr, purrr, randomForest, randomForestExplainer, gbm, pdp, clusterR, cluster, clue, factoextra, lme4, viridis, ggspatial, basemaps, sf, rgeos, maptools, fdm2id, ggmap, scales, vip, kable, kableExtra)

# import cleaned dataset
ed = read.csv("r_objects/model_data.csv")
head(ed)

# create top and bottom percentile classes based on PCA outcome and assign dummies to top and bottom 25%
ed_q = ed %>% mutate(
  q75= quantile(PC_outcome, probs = 0.75, na.rm = TRUE),
  q25 = quantile(PC_outcome, probs = 0.25, na.rm = TRUE),
  quant = case_when(
    PC_outcome >= q75 ~ "top25",
    PC_outcome <= q25 ~ "bottom25")
)

# decision tree on quant performance class
ed_split = initial_split(ed_q, 0.8)
ed_train = training(ed_split)
ed_test = testing(ed_split)

quant_tree = rpart(quant ~ . -PC_outcome -DistName - X - q75 - q25, method = "class", data=ed_train,
                 control=rpart.control(minsplit=5, minbucket = 10, cp=1e-6, xval=5))
bestcp = quant_tree$cptable[which.min(quant_tree$cptable[,"xerror"]),"CP"]
quant_tree2 = prune(quant_tree,cp=bestcp)
quant_im = fancyRpartPlot(quant_tree)

# random forest
rf_quant = randomForest(quant ~ . -PC_outcome -DistName - X - q75 - q25, data = ed_train, na.action = na.omit, mtry = 20, ntree = 50)
yhat_rfquant = predict(rf_quant, newdata = ed_test)
rmse_rfquant = sqrt(mean((yhat_rftop - edtop_test$PC_outcome)^2))







