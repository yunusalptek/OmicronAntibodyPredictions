# Load libraries
library(caret)
library(glmnet)
library(xgboost)
library(Metrics)
library(plotmo)

# Read data
train <- fread("./project/volume/data/interim/train.csv")
test <- fread("./project/volume/data/interim/test.csv")
submit <- fread("./project/volume/data/interim/ex_sub.csv")

# Reorder train so similar rows are together
train <- train[order(ic50_Omicron, age, days_sinceDose2)]
train <- train[!duplicated(train$ic50_Omicron)]

# Save data
y.train <- train$ic50_Omicron
y.test <- test$ic50_Omicron

# Use dummy variables
dummies <- dummyVars(ic50_Omicron ~ ., data = train)

# Predict
x.train <- predict(dummies, newdata = train)
x.test <- predict(dummies, newdata = test)

# Make matrices
dtrain <- xgb.DMatrix(x.train, label = y.train, missing = NA)
dtest <- xgb.DMatrix(x.test, missing = NA)

# CV split
num_folds <- 10
folds <- split(seq_len(nrow(train)), rep(1 : num_folds, each = ceiling(nrow(train) / num_folds), length.out = nrow(train)))

# Keep track of hyper-parameter tuning
tuning_table <- if (file.exists("./project/volume/data/processed/tuning_table.csv")) fread("./project/volume/data/processed/tuning_table.csv") else data.table()

# CV
param <- list(
  objective = "reg:linear",
  booster = "gbtree",
  eval_metric = "rmse",
  tree_method = 'hist',
  eta = 0.1,
  max_depth = 10,
  min_child_weight = 20,
  gamma = .05,
  subsample = 1,
  colsample_bytree = .9
)

# Set up model
xgboost_model <- xgb.cv(
  params = param,
  nfold = num_folds,
  nrounds = 900,
  missing = NA,
  data = dtrain,
  print_every_n = 1,
  early_stopping_rounds = 25)

# Extract best number of trees
best_num_trees <- xgboost_model$best_iteration

# Calculate test error at best_num_trees
test_error <- xgboost_model$evaluation_log[best_num_trees, test_rmse_mean]

# Log results in tuning_table
new_row <- as.data.table(c(param, best_num_trees = best_num_trees, test_error = test_error))
tuning_table <- rbindlist(list(tuning_table, new_row), use.names = TRUE, fill = TRUE)

# Save to CSV file
fwrite(tuning_table, "./project/volume/data/processed/tuning_table.csv")

# Train final model with best_num_trees
xgboost_model <- xgb.train(
  params = param,
  nrounds = best_num_trees,
  missing = NA,
  data = dtrain,
  watchlist = list(train = dtrain),
  print_every_n = 1)

# Predict on test
prediction <- predict(xgboost_model, newdata = dtest)

# Update submit
submit$ic50_Omicron <- prediction

# Save model
saveRDS(xgboost_model, "./project/volume/models/xgboost_model.rds")

# Write to processed
fwrite(submit, "./project/volume/data/processed/submit.csv")