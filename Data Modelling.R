library(tidyverse)
library(pls)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(caret)
library(xgboost)
# Samler ml_resultater af modeller
ml_resultat <- as.data.frame(matrix(ncol = 6, nrow = 2))
colnames(ml_resultat) <-  c("Lineær Regression", "PCA", "PLS", "Decision Tree", "Random Forest", "XGBoost")
rownames(ml_resultat) <- c("r-squared", "rmse")

# Laver dataframe
colnames(data_2022_daglig)[1] <- "datehour"
colnames(data_2022_daglig)[28] <- "trafik"
colnames(merged_atm_pres)[1] <- "datehour"

data_2022_time <- inner_join(data_2022_time, merged_atm_pres, by = "datehour")

colnames(data_2022_time)[31] <- "pres"

data_modelling <- data_2022_time %>%
  select(datehour, c_NOx_HCAB, c_NOx_HCOE, trafik, speed, hum, temp, dir_factor, pres)


# Fjerne NA værdier
data_modelling <- na.omit(data_modelling)

rownames(data_modelling) <- NULL

#2020-2022
data_modelling <- data_modelling[34776:59479,]

set.seed(123)

data_modelling$nord <- ifelse(data_modelling$dir_factor=="N",1,0)
data_modelling$nordøst <- ifelse(data_modelling$dir_factor=="NE",1,0)
data_modelling$øst <- ifelse(data_modelling$dir_factor=="E",1,0)
data_modelling$sydøst <- ifelse(data_modelling$dir_factor=="SE",1,0)
data_modelling$syd <- ifelse(data_modelling$dir_factor=="S",1,0)
data_modelling$sydvest <- ifelse(data_modelling$dir_factor=="SW",1,0)
data_modelling$vest <- ifelse(data_modelling$dir_factor=="W",1,0)
data_modelling$nordvest <- ifelse(data_modelling$dir_factor=="NW",1,0)

# Laver træning- og testdata
train_indices <- createDataPartition(data_modelling$c_NOx_HCAB, times = 1, p = .8, list = FALSE)
df_train <- data_modelling[train_indices, ]
df_test <- data_modelling[-train_indices, ]

# LM model
lm_model <- lm(c_NOx_HCAB ~ c_NOx_HCOE + temp + trafik + speed + hum + dir_factor + pres, data = df_train)
summary(lm_model)
lm_pred <- predict(lm_model, newdata = df_test)
ml_resultat[2,1] <- sqrt(mean((lm_pred - df_test$c_NOx_HCAB)^2))
ml_resultat[1,1] <- summary(lm_model)$r.squared

# PCA model
pca_model <- pcr(c_NOx_HCAB ~ c_NOx_HCOE + trafik + speed + temp + nord + nordøst + øst + sydøst + syd + sydvest + vest + nordvest, 
           data = df_train,
           scale = TRUE,
           validation = "CV")
summary(pca_model)
loadings <- pca_model$loadings
w.indicators.1 <- loadings[1:12]^2
sum(w.indicators.1)
validationplot(pca_model, val.type = "MSEP")
optimal_components <- 9

pca_pred <- predict(pca_model, newdata = df_test, ncomp = optimal_components)
actual_values <- df_test$c_NOx_HCAB
ml_resultat[2,2] <- sqrt(mean((pca_pred - actual_values)^2))

ss_total <- sum((actual_values - mean(actual_values))^2)
ss_residual <- sum((actual_values - pca_pred)^2)
ml_resultat[1,2] <- 1 - (ss_residual / ss_total)

# PLS Model
pls_model <- plsr(c_NOx_HCAB ~ c_NOx_HCOE + trafik + speed + temp + nord + nordøst + øst + sydøst + syd + sydvest + vest + nordvest, 
     data = df_train,
     scale = TRUE,
     validation = "CV")

summary(pls_model)

plot(RMSEP(pls_model), main = "RMSEP vs Number of Components")
optimal_components <- 2
pls_pred <- predict(pls_model, newdata = df_test, ncomp = optimal_components)
actual_values <- df_test$c_NOx_HCAB
ml_resultat[2,3] <- sqrt(mean((pls_pred - actual_values)^2))

ss_total <- sum((actual_values - mean(actual_values))^2)
ss_residual <- sum((actual_values - pls_pred)^2)
ml_resultat[1,3] <- 1 - (ss_residual / ss_total)


# Decision tree
tree_model <- rpart(c_NOx_HCAB ~ c_NOx_HCOE + temp + trafik + speed + hum + dir_factor + pres, 
                    data = df_train)
summary(tree_model)
rpart.plot(tree_model)
predictions_tree <- predict(tree_model, newdata = df_test)
actual_values <- df_test$c_NOx_HCAB
ml_resultat[2,4] <- sqrt(mean((predictions_tree - actual_values)^2))

ss_total <- sum((actual_values - mean(actual_values))^2)
ss_residual <- sum((actual_values - predictions_tree)^2)
ml_resultat[1,4] <- 1 - (ss_residual / ss_total)


# Random forest
rf_model <- randomForest(c_NOx_HCAB ~ c_NOx_HCOE + temp + trafik + speed + hum + dir_factor + pres,
                         data = df_train)
rf_predict <- predict(rf_model, newdata = df_test)
actual_values <- df_test$c_NOx_HCAB
ml_resultat[2,5] <- sqrt(mean((rf_predict - actual_values)^2))

ss_total <- sum((actual_values - mean(actual_values))^2)
ss_residual <- sum((actual_values - rf_predict)^2)
ml_resultat[1,5] <- 1 - (ss_residual / ss_total)

# xgboost
target_train <- df_train$c_NOx_HCAB
features_train <- as.matrix(df_train[, setdiff(names(df_train[, -c(1, 8)]), "c_NOx_HCAB")])
features_test <- df_test %>%
  select(-c("datehour", "dir_factor", "c_NOx_HCAB")) %>%
  as.matrix()

xgboost_model <- xgboost(
  data = features_train,
  label = target_train,
  objective = "reg:squarederror",
  nrounds = 300,
  max_depth = 5,
  verbose = 1
)

xgboost_pred <- predict(xgboost_model, features_test)
actual_values <- df_test$c_NOx_HCAB
ml_resultat[2,6] <- sqrt(mean((xgboost_pred - actual_values)^2))

ss_total <- sum((actual_values - mean(actual_values))^2)
ss_residual <- sum((actual_values - xgboost_pred)^2)
ml_resultat[1,6] <- 1 - (ss_residual / ss_total)




#### Neural Network ####
library(keras3)
library(tensorflow)
install_tensorflow()

library(reticulate)

# Installer TensorFlow manuelt
py_install("tensorflow==2.14.0")


# Adskiller værdier
train_x <- as.matrix(df_train[, c("c_NOx_HCOE", "trafik", "speed", "hum", "temp", "pres", "nord",
                                  "nordøst", "øst", "sydøst", "syd", "sydvest", "vest", "nordvest")])
train_y <- df_train$c_NOx_HCAB              
test_x <- as.matrix(df_test[, c("c_NOx_HCOE", "trafik", "speed", "hum", "temp", "pres", "nord", 
                                "nordøst", "øst", "sydøst", "syd", "sydvest", "vest", "nordvest")])
test_y <- df_test$c_NOx_HCAB

# Normaliserer data
train_x <- scale(train_x)
test_x <- scale(test_x)

model <- keras_model_sequential()

model %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 1)

model %>% compile(
  loss = "mse",       # Mean Squared Error for regression
  optimizer = optimizer_adam(),
  metrics = c("mae")  # Mean Absolute Error
)

# Træner model
history <- model %>% fit(
  x = train_x,
  y = train_y,
  epochs = 50,
  batch_size = 16,
  validation_split = 0.2,
  verbose = 1
)

# Evaluate the model
results <- model %>% evaluate(test_x, test_y)
print(results)

# Predict on test data
nn_pred <- model %>% predict(test_x)

# Regner RMSE og R2
rmse <- sqrt(mean((nn_pred - actual_values)^2))

ss_total <- sum((actual_values - mean(actual_values))^2)
ss_residual <- sum((actual_values - nn_pred)^2)
r2 <- 1 - (ss_residual / ss_total)

saveRDS(rf_model, "rf_model.rds")






avg_traffic <- data_modelling %>%
  mutate(day_month_hour = format(datehour, "%m_%d_%H")) %>%
  group_by(day_month_hour) %>%
  summarise(avg_trafik = mean(trafik, na.rm = TRUE)) %>%
  arrange(day_month_hour)
