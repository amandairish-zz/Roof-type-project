## multi-class classification for roof types in Swaziland using SL
## last updated 3/8/19 by Amanda Irish

library(RhpcBLASctl)
library(caret)
library(SuperLearner)
library(randomForest)
library(gbm)
library(xgboost)
library(arm)
library(gam)
library(glmnet)
library(LogicReg)
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
trainIndex <- createDataPartition(swazi.testtrain$LULC, p = 0.85,
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

# SL library
SL.library <- c("SL.randomForest", "SL.gbm", "SL.ksvm",
                "SL.glmnet", "SL.glm", "SL.gam", "SL.xgboost", 
                "SL.bayesglm", "SL.lda", "SL.mean")


# fit CV.SL using method.AUC
# While this is running check CPU using in Activity Monitor / Task Manager.
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

# fit_A  <- CV.SuperLearner(Y = Y_A,  X = X, V = 10, SL.library = SL.library, verbose = FALSE, method = "method.NNLS", family = binomial(), cvControl = list(stratifyCV = TRUE))

# examine the fits of the CV.SLs
summary(fit_T)
fit_T$coef
table(simplify2array(fit_T$whichDiscreteSL))
plot(fit_T)

summary(fit_M)
fit_M$coef
table(simplify2array(fit_M$whichDiscreteSL))
plot(fit_M)

summary(fit_Th)
fit_Th$coef
table(simplify2array(fit_Th$whichDiscreteSL))
plot(fit_Th)


# Apply method.NNLS SL predictions of roof types 
SL_pred <- data.frame(pred_T = fit_T$SL.predict, pred_M = fit_M$SL.predict, pred_Th = fit_Th$SL.predict)
SL_pred

# Apply method.NNLS discrete SL predictions of roof types - NOTE: need to add back in Ashpalt if decide to use in future
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



# Apply method.NNLS SL predictions of roof types 
SL_pred_nnls <- data.frame(pred_T = fit_T2$SL.predict, pred_M = fit_M2$SL.predict, pred_Th = fit_Th2$SL.predict)
SL_pred_nnls

# Apply method.NNLS discrete SL predictions of roof types - NOTE: need to add back in Ashpalt if decide to use in future
SL_discrete_pred_nnls <-data.frame(pred_T = fit_T2$discreteSL.predict, pred_M = fit_M2$discreteSL.predict, pred_Th = fit_Th2$discreteSL.predict)
SL_discrete_pred_nnls

Classify_nnls <- apply(SL_pred_nnls, 1, function(xx) c("Tile", "Metal", "Thatch")[unname(which.max(xx))])
Classify_dp_nnls <- apply(SL_discrete_pred_nnls, 1, function(xx) c("Tile", "Metal", "Thatch")[unname(which.max(xx))])

SL_pred_nnls_table <- table(Classify_nnls, Y)
SL_pred_nnls_table

Discrete_SL_nnls_table <- table(Classify_dp_nnls, Y)
Discrete_SL_nnls_table

# Calculate kappa statistics
# method.NNLS SL prediction
SLP_nnls <- data.frame("pred" = Classify_nnls, "actual" = Y)
kappa2(SLP_nnls[,c(1,2)], "unweighted")

# method.NNLS Discrete prediction
DP_nnls <- data.frame("pred_dp" = Classify_dp_nnls, "actual" = Y)
kappa2(DP_nnls[,c(1,2)], "unweighted")


# Do the same as the above but with method.AUC
# Apply method.auc SL predictions of roof types 
SL_pred_auc <- data.frame(pred_T = fit_T3$SL.predict, pred_M = fit_M3$SL.predict, pred_Th = fit_Th3$SL.predict)
SL_pred_auc

# Apply method.auc discrete SL predictions of roof types - NOTE: need to add back in Ashpalt if decide to use in future
#SL_discrete_pred_auc <-data.frame(pred_T = fit_T3$discreteSL.predict, pred_M = fit_M3$discreteSL.predict, pred_Th = fit_Th3$discreteSL.predict)
#SL_discrete_pred_auc

Classify_auc <- apply(SL_pred_auc, 1, function(xx) c("Tile", "Metal", "Thatch")[unname(which.max(xx))])
#Classify_dp_auc <- apply(SL_discrete_pred_auc, 1, function(xx) c("Tile", "Metal", "Thatch")[unname(which.max(xx))])

SL_pred_auc_table <- table(Classify_auc, Y)
SL_pred_auc_table

#Discrete_SL_auc_table <- table(Classify_dp_auc, Y)
#Discrete_SL_auc_table

# Calculate kappa statistics
# method.auc SL prediction
SLP_auc <- data.frame("pred" = Classify_auc, "actual" = Y)
kappa2(SLP_auc[,c(1,2)], "unweighted")


# Based on kappa statistic performance, method.auc slightly outperforms method.NNLS 
# & method.nnloglik so should will method.auc moving forward (although substantively no real difference).
# More difficult to us discrete SL if desired since method.auc does not select which is
# the best-performing algorithm (whichDiscreteSL returnes "NULL" for all)

# First need to call SL functions instead of CV.SuperLearner in order to be able to predict (???)

# Modify SL library used to get rid of those that didn't work well
SL.library2 <- c("SL.gbm", "SL.gam", "SL.ksvm", "SL.glmnet", "SL.randomForest")

set.seed(451)

# Fit SL functions w/ method.NNLS & new SL library
fit_T_SL  <- SuperLearner(Y = Y_T,  X = X, SL.library = SL.library2, verbose = FALSE, family = binomial())
fit_T_SL_auc  <- SuperLearner(Y = Y_T,  X = X, SL.library = SL.library2, verbose = FALSE, method = "method.AUC", family = binomial())
  # For the above, got this Warning message:
  #In method$computeCoef(Z = Z, Y = Y, libraryNames = libraryNames,  :
  #optim didn't converge when estimating the super learner coefficients, reason (see ?optim):  52  optim message:  ERROR: ABNORMAL_TERMINATION_IN_LNSRCH
fit_M_SL  <- SuperLearner(Y = Y_M,  X = X, SL.library = SL.library2, verbose = FALSE, family = binomial())
fit_M_SL_auc  <- SuperLearner(Y = Y_M,  X = X, SL.library = SL.library2, verbose = FALSE, method = "method.AUC", family = binomial())
  # no error message with this method.auc for metal
fit_Th_SL <- SuperLearner(Y = Y_Th, X = X, SL.library = SL.library2, verbose = FALSE, family = binomial())
fit_Th_SL_auc <- SuperLearner(Y = Y_Th, X = X, SL.library = SL.library2, verbose = FALSE, method = "method.AUC", family = binomial())
  # same warning message as for tile


fit_T_SL
fit_T_SL_auc
fit_M_SL
fit_M_SL_auc
fit_Th_SL
fit_Th_SL_auc

#fit_A_SL


# predict back on known data (df) so have comparison maps
# keep getting error message "test vector does not match model" 6/13/18

#pred_T <- predict(fit_T_SL, train, onlySL=T)  #tried changing df to train - still same error message
#pred_M  <- predict(fit_M_SL, newdata = df)
#pred_Th <- predict(fit_Th_SL, newdata = df) 
#pred_A  <- predict(fit_A_SL, newdata = df_new)

#SL_pred_df <- data.frame(pred_T = pred_T$pred, pred_M = pred_M$pred, pred_Th = pred_Th$pred)

#Classify_df <- apply(SL_pred_df, 1, function(xx) c("Tile", "Metal", "Thatch")[unname(which.max(xx))])
#Classify_df

# Add prediction probabilities & classifications to df
#df$pred_T = pred_T$pred
#df$pred_M = pred_M$pred
#df$pred_Th = pred_Th$pred
#df$pred_roof_type = Classify_df
#head(df)

# Write out new dataset with roof predictions
#write.csv(df, file = "Swazi sentinel CVSL data known and pred.csv")

# 3/11/19 got this error message when trying to predict with next chunk of code:
#Error: Length of logical index vector for `[` must equal number of columns (or 1):
#  * `.data` has 8 columns
#* Index vector has length 7

# Think it is b/c test has LULC class?? But why would this be a problem?

# Try removing LULC class
test2 <- test %>%
  select(-"LULC")

# Then use 15% retained from original dataset
pred_T_test  <- predict(fit_T_SL, newdata = test2)
pred_M_test  <- predict(fit_M_SL, newdata = test2)
pred_Th_test <- predict(fit_Th_SL, newdata = test2) 

# and for AUC
pred_T_test_auc  <- predict(fit_T_SL_auc, newdata = test2)
pred_M_test_auc  <- predict(fit_M_SL_auc, newdata = test2)
pred_Th_test_auc <- predict(fit_Th_SL_auc, newdata = test2) 

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

SL_pred_test_auc <- data.frame(pred_T = pred_T_test_auc$pred, pred_M = pred_M_test_auc$pred, pred_Th = pred_Th_test_auc$pred)
SL_pred_test_auc

# NOTE: if need to re-run kappas, make sure "Thatch" is spelled "Thach" to match original dataset.
Classify_test <- as.factor(apply(SL_pred_test, 1, function(xx) c("Tile", "Metal", "Thatch")[unname(which.max(xx))]))
Classify_test_auc <- as.factor(apply(SL_pred_test_auc, 1, function(xx) c("Tile", "Metal", "Thatch")[unname(which.max(xx))]))

confusionMatrix(Classify_test, test$LULC)
confusionMatrix(Classify_test_auc, test$LULC)


# Add prediction probabilities & classifications to vdf
vdf$pred_T = pred_T_vdf$pred
vdf$pred_M = pred_M_vdf$pred
vdf$pred_Th = pred_Th_vdf$pred
vdf$pred_roof_type = Classify_vdf
head(vdf)

# Write out new dataset with roof predictions
write.csv(vdf, file = "Validation sentinel data with roof type pred.csv")



# Next use entirely new dataset
df_new <- read.csv("SWA_OSM_Sentinel_113017.csv")
head(df_new)

pred_T <- predict(fit_T_SL, newdata = df_new)
pred_M  <- predict(fit_M_SL, newdata = df_new)
pred_Th <- predict(fit_Th_SL, newdata = df_new) 
#pred_A  <- predict(fit_A_SL, newdata = df_new)

SL_pred_new <- data.frame(pred_T = pred_T$pred, pred_M = pred_M$pred, pred_Th = pred_Th$pred)

Classify_new <- apply(SL_pred_new, 1, function(xx) c("Tile", "Metal", "Thatch")[unname(which.max(xx))])
Classify_new

# Add prediction probabilities & classifications to df_new
df_new$pred_T <-  pred_T$pred
df_new$pred_M <-  pred_M$pred
df_new$pred_Th <-  pred_Th$pred
df_new$pred_roof_type <-  Classify_new
head(df_new)

# Write out new dataset with roof predictions
write.csv(df_new, file = "Swazi Sentinel data with roof type pred - no tuning.csv")

# Save all objects in global environment
save(list = ls(), file = "Swazi SL roof pred global env - no tuning.RData")

# Save workspace image
save.image(file = "Swazi SL roof pred image - no tuning.RData")


#######################

# For epi 264: predict back on known data (df) so have comparison maps

pred_T <- predict(fit_T_SL, newdata = df)
pred_M  <- predict(fit_M_SL, newdata = df)
pred_Th <- predict(fit_Th_SL, newdata = df) 
#pred_A  <- predict(fit_A_SL, newdata = df_new)

SL_pred_df <- data.frame(pred_T = pred_T$pred, pred_M = pred_M$pred, pred_Th = pred_Th$pred)

Classify_df <- apply(SL_pred_df, 1, function(xx) c("Tile", "Metal", "Thatch")[unname(which.max(xx))])
Classify_df

# Add prediction probabilities & classifications to df
df$pred_T = pred_T$pred
df$pred_M = pred_M$pred
df$pred_Th = pred_Th$pred
df$pred_roof_type = Classify_df
head(df)

# Write out new dataset with roof predictions
write.csv(df, file = "Swazi sentinel CVSL data known and pred.csv")



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

