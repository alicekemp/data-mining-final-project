if (!("librarian" %in% rownames(utils::installed.packages()))) {
  utils::install.packages("librarian")}
librarian::shelf(tidyverse, haven, mosaic, foreach, stargazer, rpart, rpart.plot, caret, dplyr, mosaic, here, rsample, modelr, purrr, randomForest, gbm, pdp, clusterR, cluster, clue, factoextra, lme4, viridis, ggspatial, basemaps, sf, rgeos, maptools, fdm2id, ggmap, scales, vip, kable, kableExtra)

# import cleaned dataset
ed = read.csv("r_objects/cleaned_ed_data.csv")

# select top 10% performing district groups by total score
ed_top10 = ed %>%
  slice_max(.,order_by = Total, prop = 0.1)
colnames(ed_top10)
rf1 = randomForest(Total ~ ., data = ed_top10, na.action = na.omit, ntree = 30)
plot(rf1)

tree1 = rpart(Total ~ Group + Part_Rate + Above_TSI_Both_Rate + Pop..2020 + Change_2010.20_pct + total_poverty_percent + child_poverty_percent + Percent_college_grads_county_19 + unemployment_2019 + unemployment_2020 + pct_state_median_HH_income + FreeElig_mean + RedcEligQty_mean + PaidEligQty_mean + abuse_neglect_investigations + COUNT.OF.STUDENTS.SUSPENDED.IN.SCHOOL, data = ed_top10, control = rpart.control(cp = 0.0001))
best_cp = tree1$cptable[which.min(tree1$cptable[,"xerror"]),"CP"] # find best cp
tree_pruned = prune(tree1, cp = best_cp) # prune tree to best cp
par(xpd=TRUE)
prp(tree_pruned, faclen = 0, cex = 0.8, box.palette = "auto", extra = 100)

