library(h2o)
library (tidyverse)
library(caret)

localH2O <- h2o.init(ip='localhost', nthreads=-1,
                     min_mem_size='100G', max_mem_size='200G')

# Import train/test set into H2O
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

# Generate a grid of models and stack them together

# GBM Hyperparamters
learn_rate_opt <- c(0.001, 0.005, 0.01, 0.1, 0.2, 0.3)
max_depth_opt <- c(3, 4, 5, 6, 9)
ntrees_opt <- c(10, 20, 40, 60, 80, 100)
gbm_hyper_params <- list(learn_rate = learn_rate_opt,
                     max_depth = max_depth_opt,
                     ntrees = ntrees_opt)

search_criteria <- list(strategy = "RandomDiscrete",
                        max_models = 20,
                        seed = 1)

gbm_grid <- h2o.grid(algorithm = "gbm",
                     grid_id = "gbm_grid_multinomial",
                     x = x,
                     y = y,
                     training_frame = train,
                     seed = 1,
                     nfolds = nfolds,
                     fold_assignment = "Modulo",
                     keep_cross_validation_predictions = TRUE,
                     hyper_params = gbm_hyper_params,
                     search_criteria = search_criteria)

# Get the grid results, sorted by accuracy
gbm_gridperf <- h2o.getGrid(grid_id = "gbm_grid_multinomial",
                            sort_by = "accuracy",
                            decreasing = TRUE)
print(gbm_gridperf)

# Grab the top GBM model, chosen by validation AUC
best_gbm <- h2o.getModel(gbm_gridperf@model_ids[[1]])
length(gbm_gridperf@model_ids)
gbm_2 <- h2o.getModel(gbm_gridperf@model_ids[[2]])
gbm_3 <- h2o.getModel(gbm_gridperf@model_ids[[3]])
gbm_4 <- h2o.getModel(gbm_gridperf@model_ids[[4]])
gbm_5 <- h2o.getModel(gbm_gridperf@model_ids[[5]])

# Now let's evaluate the model performance on a test set
# so we get an honest estimate of top model performance
best_gbm_perf <- h2o.performance(model = best_gbm,
                                  newdata = test)
h2o.mean_per_class_error(best_gbm_perf) # can't assess accuracy - ?
#0.2611111

# Look at the hyperparamters for the best model
print(best_gbm@model[["model_summary"]])



# RF hyperparameters

#ntrees_opt_rf <- c(10, 20, 50, 100)
#mtries_opt <- c(1:4)
#max_deph_opt_rf <- c(2:6)

#rf_hyper_params <- list(ntrees = ntrees_opt_rf,
#                        mtries = mtries_opt,
#                        max_depth = max_deph_opt_rf)

# Kept coming up with error messages when trying to code this way -?? not sure why.
# Code below works so will just go with that.

search_criteria_rf <- list(strategy = "RandomDiscrete",
                           stopping_rounds = 10,
                           stopping_tolerance = 0.00001,
                           stopping_metric = "misclassification",
                           seed = 1)

rf_grid <- h2o.grid(algorithm = "randomForest",
                     grid_id = "rf_grid_multinomial",
                     x = x,
                     y = y,
                     training_frame = train,
                     seed = 1,
                     nfolds = nfolds,
                     fold_assignment = "Modulo",
                     keep_cross_validation_predictions = TRUE,
                     hyper_params = list(ntrees = c(10,20,50,100,200),
                                         mtries = c(2,3,4),
                                         max_depth = c(3,4,5,6),
                                         seed = 21),
                     search_criteria = search_criteria_rf)

# Get the grid results, sorted by mean per class error
rf_gridperf <- h2o.getGrid(grid_id = "rf_grid_multinomial",
                           sort_by = "accuracy",
                           decreasing = TRUE)
print(rf_gridperf)

# Grab the top RF model, chosen by mean per class error
best_rf <- h2o.getModel(rf_gridperf@model_ids[[1]])
length(rf_gridperf@model_ids)
rf_2 <- h2o.getModel(rf_gridperf@model_ids[[2]])
rf_3 <- h2o.getModel(rf_gridperf@model_ids[[3]])
rf_4 <- h2o.getModel(rf_gridperf@model_ids[[4]])
rf_5 <- h2o.getModel(rf_gridperf@model_ids[[5]])

# Now let's evaluate the model performance on a test set
# so we get an honest estimate of top model performance
best_rf_perf <- h2o.performance(model = best_rf,
                                 newdata = test)
h2o.mean_per_class_error(best_rf_perf)
#0.2944444

# Look at the hyperparamters for the best model
print(best_rf@model[["model_summary"]])


# Naive Bayes hyperparameters

# create tuning grid
nb_hyper_params <- list(
  laplace = seq(0, 5, by = 0.5)
)

# build grid search 
nb_grid <- h2o.grid(
  algorithm = "naivebayes",
  grid_id = "nb_grid",
  x = x, 
  y = y, 
  training_frame = train, 
  nfolds = 10,
  seed = 1,
  fold_assignment = "Modulo",
  keep_cross_validation_predictions = TRUE,
  hyper_params = nb_hyper_params
)

# Sort the grid models by mean per class error
nb_gridperf <- h2o.getGrid("nb_grid", 
                           sort_by = "accuracy",
                           decreasing = TRUE)
print(nb_gridperf)

# Grab the top NB model, chosen by mean per class error
best_nb <- h2o.getModel(nb_gridperf@model_ids[[1]])
length(nb_gridperf@model_ids)
nb_2 <- h2o.getModel(nb_gridperf@model_ids[[2]])
nb_3 <- h2o.getModel(nb_gridperf@model_ids[[3]])
nb_4 <- h2o.getModel(nb_gridperf@model_ids[[4]])
nb_5 <- h2o.getModel(nb_gridperf@model_ids[[5]])

# Now let's evaluate the model performance on a test set
# so we get an honest estimate of top model performance
best_nb_perf <- h2o.performance(model = best_nb,
                                newdata = test)
h2o.mean_per_class_error(best_nb_perf)
#0.377778

# Look at the hyperparamters for the best model
print(best_nb@model[["model_summary"]])



# GLM hyperparameters

glm_hyper_params <- list(
  alpha = c(0.01,0.1,0.3,0.5,0.7,0.9), 
  lambda = c(1e-4,1e-5,1e-6,1e-7,1e-8)
)

glm_search_criteria <- list(
  strategy = "RandomDiscrete", 
  seed = 42,
  stopping_metric = "AUTO",
  stopping_tolerance = 0.001,
  stopping_rounds = 2)

glm_grid <- h2o.grid(
  algorithm = "glm",
  family = "multinomial",
  grid_id = "glm_grid",
  x = x, 
  y = y, 
  training_frame = train, 
  nfolds = 10,
  seed = 1,
  fold_assignment = "Modulo",
  keep_cross_validation_predictions = TRUE,
  hyper_params = glm_hyper_params,
  search_criteria = glm_search_criteria
)
  
# Sort the grid models by mean per class error
glm_gridperf <- h2o.getGrid("glm_grid", 
                           sort_by = "accuracy")
print(glm_gridperf)

# Grab the top NB model, chosen by mean per class error
best_glm <- h2o.getModel(glm_gridperf@model_ids[[1]])
length(glm_gridperf@model_ids)
glm_2 <- h2o.getModel(glm_gridperf@model_ids[[2]])
glm_3 <- h2o.getModel(glm_gridperf@model_ids[[3]])
glm_4 <- h2o.getModel(glm_gridperf@model_ids[[4]])
glm_5 <- h2o.getModel(glm_gridperf@model_ids[[5]])

# Now let's evaluate the model performance on a test set
# so we get an honest estimate of top model performance
best_glm_perf <- h2o.performance(model = best_glm,
                                newdata = test)
h2o.mean_per_class_error(best_glm_perf)
#0.24444444

# Look at the hyperparamters for the best model
print(best_glm@model[["model_summary"]])



# Create a stacked ensemble learner from the best individual models
ensemble1 <- h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train,
                                model_id = "ensemble_best_multinomial",
                                base_models = c(best_glm, best_rf,
                                                best_nb, best_gbm)
)

# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble1, newdata = test)
perf


# Create a stacked ensemble learner from all base models
ensemble2 <- h2o.stackedEnsemble(x = x,
                                 y = y,
                                 training_frame = train,
                                 model_id = "ensemble_multinomial",
                                 base_models = c(best_glm, glm_2, glm_3, glm_4, glm_5,
                                                 best_rf, rf_2, rf_3, rf_4, rf_5,
                                                 best_nb, nb_2, nb_3, nb_4, nb_5, 
                                                 best_gbm, gbm_2, gbm_3, gbm_4, gbm_5)
)

# Eval ensemble performance on a test set
perf2 <- h2o.performance(ensemble2, newdata = test)
perf2






# Check performance of ensemble vs GLM, the best-performing algorith (think they will be the same...)
best_glm_perf 
#ensemble_auc_test <- h2o.auc(perf)
#print(sprintf("Best Base-learner Test AUC:  %s", baselearner_best_auc_test))
#print(sprintf("Ensemble Test AUC:  %s", ensemble_auc_test))


# Compare to base learner performance on the test set
# Need to troubleshoot this section 3/21/19 - can't use auc b/c multiclass classification
.getauc <- function(mm) h2o.auc(h2o.performance(h2o.getModel(mm), newdata = test))
baselearner_aucs <- sapply(gbm_grid@model_ids, .getauc)
baselearner_best_auc_test <- max(baselearner_aucs)
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("Best Base-learner Test AUC:  %s", baselearner_best_auc_test))
print(sprintf("Ensemble Test AUC:  %s", ensemble_auc_test))

# Generate predictions on a test set (if neccessary)
pred <- h2o.predict(ensemble, newdata = test)

h2o.shutdown()
