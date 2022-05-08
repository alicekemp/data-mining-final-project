if (!("librarian" %in% rownames(utils::installed.packages()))) {
  utils::install.packages("librarian")}
librarian::shelf(rattle, tidyverse, haven, mosaic, foreach, stargazer, rpart, rpart.plot, caret, dplyr, mosaic, here, rsample, modelr, purrr, randomForest, randomForestExplainer, gbm, pdp, clusterR, cluster, clue, factoextra, lme4, viridis, ggspatial, basemaps, sf, rgeos, maptools, fdm2id, ggmap, scales, vip, kableExtra)

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

write.csv(data, "r_objects/model_scaled_data.csv")
