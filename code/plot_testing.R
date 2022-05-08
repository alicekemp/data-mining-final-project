load("all_objects.RData")
library(ggplot2)
library(gridExtra)
# top 5 partial dependence plots 
feats = importance_table[,1]
par(mfrow = c(3,2))
for (i in feats){
  
  pdp::partial(rf, pred.var = i, plot = TRUE, plot.engine = "ggplot2") + 
    ggtitle(paste("Partial Dependence Plot of ", i)) + 
    xlab(paste(i)) + 
    ylab("Predicted Resid")
  plot = recordPlot()
  plot_name = paste(i,"plot", sep = "_")
  assign(plot_name, plot)
}

par(mfrow = c(3,2))

gridExtra::grid.arrange(arrangeGrob(FreeElig_mean_plot, avg_salary_prof_plot))

