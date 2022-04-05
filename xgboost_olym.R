if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, rio, tidyverse, xgboost, caTools, ggplot2, Ckmeans.1d.dp,
               DALEXtra, mlr, caret, DiagrammeR, SHAPforxgboost, rmarkdown)

data <- import("olympicmedals.dta")

data = data %>% 
  select(-c(cc, year, lpop, lrgdpepc, sptinc992j_p90p100, sptinc992j_p99p100))

set.seed(1)
split = sample.split(data$points, SplitRatio = 0.75)
training_set = subset(data, split == TRUE)
testing_set = subset(data, split == FALSE)

training_set = training_set %>% 
  as.matrix()

testing_set = testing_set %>% 
  as.matrix()

model = xgboost(data = training_set[,-8], label = training_set[,8], nrounds = 40)


pred_y = predict(model,testing_set[,-8])


mse = mean((testing_set[8] - pred_y)^2)
mae = caret::MAE(testing_set[8], pred_y)
rmse = caret::RMSE(testing_set[8], pred_y)

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)

########################################################
###################Linear Model#########################
########################################################

data2 <- import("olympicmedals.dta")
data2 <- data2 %>% 
  filter(!is.na(data2$lpop))

data2 <- data2 %>% 
  filter(!is.na(data2$lrgdpepc))

split2 = sample.split(data2$points, SplitRatio = 0.74)
training_set2 = subset(data2, split2 == TRUE)
testing_set2 = subset(data2, split2 == FALSE)

model2 <- lm(points ~ lpop + lrgdpepc, data=training_set2)
pred_y2 = predict(model2,testing_set2[,-12])

mse2 = mean((testing_set2[,12] - pred_y2)^2)
mae2 = caret::MAE(testing_set2[,12], pred_y2)
rmse2 = caret::RMSE(testing_set2[,12], pred_y2)

cat("MSE: ", mse2, "MAE: ", mae2, " RMSE: ", rmse2)
#######################################################

xgb_imp <- xgb.importance(feature_names = model$feature_names,
                          model = model)
xgb.ggplot.importance(xgb_imp, n_clusters = 1)

xgb.plot.tree(model = model, trees = 0)

shap <- shap.prep(xgb_model = model, X_train = testing_set[,-8])
shap.plot.summary(shap)

shap.plot.dependence(shap, "rgdpe", color_feature = "auto", 
                     alpha = 0.5, jitter_width = 0.1)


##############Model again###############################


