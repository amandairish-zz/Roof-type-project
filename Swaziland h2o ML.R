library(h2o)
library (tidyverse)
library(caret)

h2o.init()

# Import a sample binary outcome train/test set into H2O
setwd("/Users/amandairish/Desktop/Malaria project/Swaziland")
swazi <- read_csv("SWZ_OSM_Sentinel_112017.csv")

# Process data to get rid of skewness, center & scale variables (caret package)
swazi <- as.data.frame(swazi)  # need to do this to get next step to work
swazi.pre <- preProcess(swazi[, 4:10],
                        method = c("BoxCox", "center", "scale"))
swazi.pre

# Apply the transformations
swazi.trans <- predict(swazi.pre, swazi)

# Convert back to tibble & finish processing 
swazi.trans <- as_tibble(swazi.trans) %>%
  filter(LULC != "Asphalt") # get rid of Asphalt as LULC class

# Subset columns for training/testing dataset - try with all 3 classes
swazi_testtrain <- select(swazi.trans, -(c("Latitude", "Longitude")))
head(swazi_testtrain)

# Set seed for reproducibility
set.seed(88)

# Partition data into training and test data
trainIndex <- createDataPartition(swazi_testtrain$LULC, p = 0.8,
                                  list = FALSE,
                                  times = 1)
train_swazi <- swazi_testtrain[trainIndex,]
test_swazi <- swazi_testtrain[-trainIndex,]

# Convert to h2o files
train <- as.h2o(train_swazi)
test <- as.h2o(test_swazi)

# Identify predictors and response
y <- "LULC"
x <- setdiff(names(train), y)

# For binary classification, response should be a factor
train[,y] <- as.factor(train[,y])
test[,y] <- as.factor(test[,y])

# Number of CV folds (to generate level-one data for stacking)
nfolds <- 10

# There are a few ways to assemble a list of models to stack toegether:
# 1. Train individual models and put them in a list
# 2. Train a grid of models
# 3. Train several grids of models
# Note: All base models must have the same cross-validation folds and
# the cross-validated predicted values must be kept.


# 1. Generate a 2-model ensemble (GBM + RF)

# Train & Cross-validate a GBM
my_gbm <- h2o.gbm(x = x,
                  y = y,
                  training_frame = train,
                  distribution = "multinomial",
                  ntrees = 10,
                  max_depth = 20,
                  min_rows = 2,
                  learn_rate = 0.2,
                  nfolds = nfolds,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)

# Train & Cross-validate a RF
my_rf <- h2o.randomForest(x = x,
                          y = y,
                          training_frame = train,
                          ntrees = 100,
                          nfolds = nfolds,
                          fold_assignment = "Modulo",
                          keep_cross_validation_predictions = TRUE,
                          seed = 1)

# Train a stacked ensemble using the GBM and RF above
ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train,
                                model_id = "my_ensemble_multinomial",
                                base_models = list(my_gbm, my_rf))

# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = test)

# Compare to base learner performance on the test set
perf_gbm_test <- h2o.performance(my_gbm, newdata = test)
perf_rf_test <- h2o.performance(my_rf, newdata = test)
# h2o uses mean per class error as metric to eval multinomial classification
baselearner_best_mpce_test <- min(h2o.mean_per_class_error(perf_gbm_test), h2o.mean_per_class_error(perf_rf_test))
ensemble_mpce_test <- h2o.mean_per_class_error(perf)


# Generate predictions on a test set (if neccessary)
pred <- h2o.predict(ensemble, newdata = test)


# 2. Generate a random grid of models and stack them together

# GBM Hyperparamters
learn_rate_opt <- c(0.01, 0.03)
max_depth_opt <- c(3, 4, 5, 6, 9)
sample_rate_opt <- c(0.7, 0.8, 0.9, 1.0)
col_sample_rate_opt <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
hyper_params <- list(learn_rate = learn_rate_opt,
                     max_depth = max_depth_opt,
                     sample_rate = sample_rate_opt,
                     col_sample_rate = col_sample_rate_opt)

search_criteria <- list(strategy = "RandomDiscrete",
                        max_models = 3,
                        seed = 1)

gbm_grid <- h2o.grid(algorithm = "gbm",
                     grid_id = "gbm_grid_binomial",
                     x = x,
                     y = y,
                     training_frame = train,
                     ntrees = 10,
                     seed = 1,
                     nfolds = nfolds,
                     fold_assignment = "Modulo",
                     keep_cross_validation_predictions = TRUE,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)

# Train a stacked ensemble using the GBM grid
ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train,
                                model_id = "ensemble_gbm_grid_binomial",
                                base_models = gbm_grid@model_ids)

# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = test)

# Compare to base learner performance on the test set
# Need to troubleshoot this section 3/21/19
.getauc <- function(mm) h2o.auc(h2o.performance(h2o.getModel(mm), newdata = test))
baselearner_aucs <- sapply(gbm_grid@model_ids, .getauc)
baselearner_best_auc_test <- max(baselearner_aucs)
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("Best Base-learner Test AUC:  %s", baselearner_best_auc_test))
print(sprintf("Ensemble Test AUC:  %s", ensemble_auc_test))

# Generate predictions on a test set (if neccessary)
pred <- h2o.predict(ensemble, newdata = test)