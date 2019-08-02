# multi-class classification for roof types in Swaziland using SL
## with tuning component algorithms
## last updated 4/28/19 by Amanda Irish

library(RhpcBLASctl)
library(caret)
library(SuperLearner)
library(randomForest)
library(xgboost)
library(gam)
library(glmnet)
library(cvAUC)
library(irr)
library(tidyverse)
library(namespace)

# Load data
setwd("/Users/amandairish/Desktop/Malaria project/Swaziland")
swazi <- read.csv("SWZ_OSM_Sentinel_112017.csv")

# Get rid of asphalt as outcome class
swazi <- swazi %>%
  filter(LULC != "Asphalt")

# Change LULC so it is a factor variable w/ 3 levels instead of string variable
swazi$LULC <- as.factor(as.character(swazi$LULC))
levels(swazi$LULC)

# Exploratory analysis of data
head(swazi)

# Summary statistics
summary(swazi$B2)
summary(swazi$B3)
summary(swazi$B4)
summary(swazi$B8)
summary(swazi$NL)
summary(swazi$ndvi)
summary(swazi$ndwi)

# Histograms
ggplot(data = swazi, mapping = aes(x = B2)) +  # R skew
  geom_histogram(binwidth = 0.01)

ggplot(data = swazi, mapping = aes(x = B3)) +  # R skew
  geom_histogram(binwidth = 0.01)

ggplot(data = swazi, mapping = aes(x = B4)) +  # mostly normal, sl R skew w/ 1(?) outlier >0.4
  geom_histogram(binwidth = 0.01)

ggplot(data = swazi, mapping = aes(x = B8)) +  # same as B4
  geom_histogram(binwidth = 0.01)

ggplot(data = swazi, mapping = aes(x = NL)) +  # very R skew
  geom_histogram(binwidth = 1)

ggplot(data = swazi, mapping = aes(x = ndvi)) +  # normal
  geom_histogram(binwidth = 0.01)

ggplot(data = swazi, mapping = aes(x = ndwi)) +  # slight R skew
  geom_histogram(binwidth = 0.01)

# Calculate skewness
skew.values <- apply(swazi[4:10], 2, skewness)  # apply skewness function to columns 4-10
skew.values  # should be close to 0 if normally distributed, >0 means R skew

# Process data to get rid of skewness, center & scale variables
swazi <- as.data.frame(swazi)  # need to do this to get next step to work
swazi.pre <- preProcess(swazi[, 4:10],
                        method = c("BoxCox", "center", "scale"))
swazi.pre

# Apply the transformations
swazi.trans <- predict(swazi.pre, swazi)

# Convert back to tibble
swazi.trans <- as_tibble(swazi.trans)

# Subset columns for training/testing dataset - try with all 3 classes
swazi.testtrain <- select(swazi.trans, -(c("Latitude", "Longitude")))
head(swazi.testtrain)

# Set seed for reproducibility
set.seed(88)

# Partition data into training and test data
trainIndex <- createDataPartition(swazi.testtrain$LULC, p = 0.9,
                                  list = FALSE,
                                  times = 1)
train <- swazi.testtrain[trainIndex,]
test <- swazi.testtrain[-trainIndex,]

table(train$LULC)
table(test$LULC)

# outcome
Y <- train$LULC
Y_test <- test$LULC

# independent variables - B2, B3, B4, B8, NL, NDVI, NDWI
X <- train[, 2:8]
X_test <- test[, 2:8]

# create the 3 binary outcome variables
Y_T  <- as.numeric(Y  == "Tile")
Y_M  <- as.numeric(Y == "Metal")
Y_Th <- as.numeric(Y == "Thatch")

# Setup parallel computation - use all cores on our computer.
num_cores = RhpcBLASctl::get_num_cores()

# How many cores does this computer have?
num_cores

# Use 2 of those cores for parallel SuperLearner.
# Replace "2" with "num_cores" (without quotes) to use all cores.
options(mc.cores = 4)

# Check how many parallel workers we are using (on macOS/Linux).
getOption("mc.cores")

# We need to set a different type of seed that works across cores.
# Otherwise the other cores will go rogue and we won't get repeatable results.
# This version is for the "multicore" parallel system in R.
set.seed(1, "L'Ecuyer-CMRG")

# Hyperparameter tuning for learners

# xgboost
tune.xgboost <- list(ntrees = c(20, 50, 100, 200, 500),
                     max_depth = 1:4,
                     shrinkage = c(0.001, 0.01, 0.1))

# Set detailed names = T so we can see the configuration for each function.
# Also shorten the name prefix.
learners.xgboost <-  create.Learner("SL.xgboost", tune = tune.xgboost, 
                                    detailed_names = T, name_prefix = "xgb")

# 36 configurations - not too shabby.
length(learners.xgboost$names)
learners.xgboost$names


# random forest
tune.rf <- list(ntrees = c(20, 50, 100, 200, 500),
                max_depth = 1:6,
                mtry = round(c(1, sqrt(ncol(X)), ncol(X))))

# Create a randomForest learner that optimizes over mtry, ntrees, and max_depth
learners.rf <- create.Learner("SL.randomForest", tune = tune.rf, detailed_names = T,
                              name_prefix = "rf")
learners.rf
length(learners.rf$names)

# ksvm
tune.ksvm <- list(C = c(0.001, 0.01, 0.1, 1, 10, 100, 1000))
learners.ksvm <- create.Learner("SL.ksvm", tune = tune.ksvm, detailed_names = T,
                                name_prefix = "ksvm")
learners.ksvm

# glmnet
# Optimize elastic net over alpha, with a custom environment and detailed names.
tune.enet <- list(alpha = seq(0, 1, length.out=5))
learners.enet <- create.Learner("SL.glmnet", tune = tune.enet, detailed_names = T,
                                name_prefix = "enet")
learners.enet


# write a function to review meta-weights (coefficients) from a CV.SuperLearner object
review_weights = function(cv_sl) {
  meta_weights = coef(cv_sl)
  means = colMeans(meta_weights)
  sds = apply(meta_weights, MARGIN = 2,  FUN = sd)
  mins = apply(meta_weights, MARGIN = 2, FUN = min)
  maxs = apply(meta_weights, MARGIN = 2, FUN = max)
  # Combine the stats into a single matrix.
  sl_stats = cbind("mean(weight)" = means, "sd" = sds, "min" = mins, "max" = maxs)
  # Sort by decreasing mean weight.
  sl_stats[order(sl_stats[, 1], decreasing = T), ]
}

print(review_weights(cv_sl), digits = 3)



# SL library
SL.library <- c("SL.randomForest", "SL.xgboost", "SL.ksvm",
                "SL.glmnet", "SL.glm", "SL.gam", learners.enet$names,
                learners.ksvm$names, learners.rf$names, learners.xgboost$names,
                "SL.mean")


# fit CV.SL using default method (NNLS) since have had problems
# with convergence using method.AUC and other issues with NNloglik
# While this is running check CPU using in Activity Monitor / Task Manager.
# These took about 30 min or so 4/29/19
system.time({
  fit_T  <- CV.SuperLearner(Y = Y_T,  
                            X = X, 
                            V = 10, 
                            SL.library = SL.library, 
                            verbose = FALSE, 
                            family = binomial(),
                            parallel = "multicore", 
                            cvControl = list(stratifyCV = TRUE))
})

system.time({
  fit_M  <- CV.SuperLearner(Y = Y_M,  
                            X = X, 
                            V = 10, 
                            SL.library = SL.library, 
                            verbose = FALSE, 
                            family = binomial(),
                            parallel = "multicore",
                            cvControl = list(stratifyCV = TRUE))
})

system.time({
  fit_Th <- CV.SuperLearner(Y = Y_Th, 
                            X = X, 
                            V = 10, 
                            SL.library = SL.library, 
                            verbose = FALSE, 
                            family = binomial(), 
                            parallel = "multicore", 
                            cvControl = list(stratifyCV = TRUE))
})


# examine the fits of the CV.SLs
summary(fit_T)
fit_T$coef
table(simplify2array(fit_T$whichDiscreteSL))
#plot(fit_T) # this doesn't work with NNloglik

summary(fit_M)
fit_M$coef
table(simplify2array(fit_M$whichDiscreteSL))
#plot(fit_M)

summary(fit_Th)
fit_Th$coef
table(simplify2array(fit_Th$whichDiscreteSL))
#plot(fit_Th)


# Apply method.NNloglik SL predictions of roof types 
SL_pred <- data.frame(pred_T = fit_T$SL.predict, pred_M = fit_M$SL.predict, pred_Th = fit_Th$SL.predict)
SL_pred

# Apply method.NNloglik discrete SL predictions of roof types
SL_discrete_pred <-data.frame(pred_T = fit_T$discreteSL.predict, pred_M = fit_M$discreteSL.predict, pred_Th = fit_Th$discreteSL.predict)
SL_discrete_pred

Classify <- apply(SL_pred, 1, function(xx) c("Tile", "Metal", "Thatch")[unname(which.max(xx))])
Classify_dp <- apply(SL_discrete_pred, 1, function(xx) c("Tile", "Metal", "Thatch")[unname(which.max(xx))])

SL_pred_table <- table(Classify, Y)
SL_pred_table

Discrete_SL_table <- table(Classify_dp, Y)
Discrete_SL_table


# Calculate kappa statistics
# SL prediction
SLP <- data.frame("pred" = Classify, "actual" = Y)
kappa2(SLP[,c(1,2)], "unweighted")

# Discrete prediction
DP <- data.frame("pred_dp" = Classify_dp, "actual" = Y)
kappa2(DP[,c(1,2)], "unweighted")



# First need to call SL functions instead of CV.SuperLearner in order to be able to predict

set.seed(451)

# Fit SL functions w/ method.NNloglik
fit_T_SL  <- SuperLearner(Y = Y_T,  X = X, SL.library = SL.library, verbose = FALSE, family = binomial())
fit_M_SL  <- SuperLearner(Y = Y_M,  X = X, SL.library = SL.library, verbose = FALSE, family = binomial())
fit_Th_SL <- SuperLearner(Y = Y_Th, X = X, SL.library = SL.library, verbose = FALSE, family = binomial())

fit_T_SL
fit_M_SL
fit_Th_SL


# 3/11/19 got this error message when trying to predict with next chunk of code:
#Error: Length of logical index vector for `[` must equal number of columns (or 1):
#  * `.data` has 8 columns
#* Index vector has length 7

# Think it is b/c test has LULC class?? But why would this be a problem?

# Try removing LULC class
test2 <- test %>%
  select(-"LULC")

# Then use 20% retained from original dataset
pred_T_test  <- predict(fit_T_SL, newdata = test2)
pred_M_test  <- predict(fit_M_SL, newdata = test2)
pred_Th_test <- predict(fit_Th_SL, newdata = test2) 

# Attach LULC - this doesn't seem to be necessary (??)
LULC <- as.vector(test$LULC)

pred_T_2 <- as.data.frame(cbind(pred_T_test$pred, LULC))
pred_M_2 <- as.data.frame(cbind(pred_M_test$pred, LULC))
pred_Th_2 <- as.data.frame(cbind(pred_Th_test$pred, LULC))

# from online SL intro:
str(pred_T_test)
#summary(pred_T_vdf$library.predict)
#qplot(pred_T_vdf$pred) + theme_bw()
#qplot(Y_vdf, pred_T_vdf$pred) + theme_classic()


SL_pred_test <- data.frame(pred_T = pred_T_test$pred, pred_M = pred_M_test$pred, pred_Th = pred_Th_test$pred)
SL_pred_test

# NOTE: if need to re-run kappas, make sure "Thatch" is spelled "Thach" to match original dataset.
Classify_test <- as.factor(apply(SL_pred_test, 1, function(xx) c("Tile", "Metal", "Thatch")[unname(which.max(xx))]))

confusionMatrix(Classify_test, test$LULC)


# Add prediction probabilities & classifications to vdf
vdf$pred_T = pred_T_vdf$pred
vdf$pred_M = pred_M_vdf$pred
vdf$pred_Th = pred_Th_vdf$pred
vdf$pred_roof_type = Classify_vdf
head(vdf)

# Write out new dataset with roof predictions
write.csv(vdf, file = "Validation sentinel data with roof type pred.csv")



#####################
# Evaluate with ROC curves

# CV SL predictions
# Tile
roc.t.cvsl <- as.data.frame(cbind(fit_T$SL.predict, fit_T$Y))
colnames(roc.t.cvsl) <- c("predictions", "labels")
AUC(roc.t.cvsl$predictions, roc.t.cvsl$labels)

# Metal
roc.m.cvsl <- as.data.frame(cbind(fit_M$SL.predict, fit_M$Y))
colnames(roc.m.cvsl) <- c("predictions", "labels")
AUC(roc.m.cvsl$predictions, roc.m.cvsl$labels)

# Thatch
roc.th.cvsl <- as.data.frame(cbind(fit_Th$SL.predict, fit_Th$Y))
colnames(roc.th.cvsl) <- c("predictions", "labels")
AUC(roc.th.cvsl$predictions, roc.th.cvsl$labels)

# Plot curves with pROC package
library(pROC)

roc1 <- roc(roc.m.cvsl$labels,
            roc.m.cvsl$predictions, percent=TRUE,
            # arguments for auc
            auc=c(100, 90), auc.correct=TRUE,
            # arguments for ci
            ci=TRUE, boot.n=100, ci.alpha=0.9, stratified=FALSE,
            # arguments for plot
            plot=TRUE, grid=TRUE,
            print.auc=TRUE, show.thres=TRUE)

