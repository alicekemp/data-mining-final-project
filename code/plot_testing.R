load("all_objects.RData")
library(ggplot2)
library(gridExtra)
# top 5 partial dependence plots 
feats_all = importance_table[,1]
par(mfrow = c(3,2))
for (i in feats_all){
  
  plot = pdp::partial(rf, pred.var = i, plot = TRUE, plot.engine = "ggplot2") + 
    ggtitle(paste(i)) + 
    xlab(paste(i)) + 
    ylab("Predicted Resid")
  plot_name = paste(i,"plot", sep = "_")
  assign(plot_name, plot)
}

par(mfrow = c(3,2))
description = textGrob(
"Models Generated using Random Forest Regression,
used to predict the Residuals of the inital Linear Model.
These variables carry the most Importance in identifying 
the source of the Variation in educational outcomes
for All School Districts"
)
gridExtra::grid.arrange(arrangeGrob(FreeElig_mean_plot, 
                                    avg_salary_prof_plot,
                                    staff_pct_profsupport_plot,
                                    RUC.code_plot,
                                    exp_pct_cartecheduc_plot,
                                    description),
                        top="Partial Dependence Plots for All Schools")


yhat_forest =  predict(rf,newdata=mod_test)
mod_test = cbind(mod_test,yhat_forest)

ggplot(mod_test, aes(x=yhat_forest,y=lin_resid))+
  geom_jitter() +
  geom_abline(slope = 1,intercept = 0,color="black",linetype="dashed") +
  geom_smooth() +
  xlab("Predicted Residual (randomForest)") +
  ylab("Actual Residuals (Linear Model)")



