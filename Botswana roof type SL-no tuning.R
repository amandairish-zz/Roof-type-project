## multi-class classification for roof types using SL
## last updated 6/2/18 by Amanda Irish

library(SuperLearner)
library(randomForest)
library(gbm)
library(xgboost)
library(cvAUC)
library(irr)
library(ggplot2)

# Set wd
setwd("/Users/amandairish/Desktop/Malaria project")

# load training data
df <- read.csv("BW_OSM_Sentinel_081017.csv")
head(df)

# remove asphalt from outcome types
df <- df[!(df$LULC=="Asphalt"),]
df$LULC

# change LULC so it is a factor variable w/ 3 levels instead of 4
#df$pred_roof_type <- as.factor(as.character(df$pred_roof_type))  # This wasn't needed 6/2/18
df$LULC <- as.factor(as.character(df$LULC))
levels(df$LULC)


# create test set from df: approx. 10% stratified random sample of observations
set.seed(12345)
sp <- split(df, list(df$LULC)) # want to stratify on outcome so have same # of each outcome type in validation set
sample_sp <- lapply(sp, function(f) f[sample(1:nrow(f), 30, FALSE),])
vdf <- do.call(rbind, sample_sp) # create the validation dataframe

# create training set from df: deleting observations used in vdf
train <- df[!(df$Latitude %in% vdf$Latitude),]

# outcome
Y <- train$LULC
Y_vdf <- vdf$LULC

# independent variables
X <- train[, 4:10]
X_vdf <- vdf[, 4:10]

# create the 4 binary outcome variables
Y_T  <- as.numeric(Y  == "Tile")
Y_M  <- as.numeric(Y == "Metal")
Y_Th <- as.numeric(Y == "Thach")
# Y_A  <- as.numeric(Y == "Asphalt")


### MOVED tuning to separate R script 6/2/18
### RANDOM FOREST TUNING

# add hyperparameter options to randomForest algorithm to add to SL library
# just change # trees
#SL.rf.moretrees <-  function(...) {
#  SL.randomForest(..., ntree = 3000)
#}

# change mtry (# features randomly chosen within each decision tree node)
#mtry_seq <- floor(sqrt(ncol(X)) * c(0.5, 1, 2))
#mtry_seq

#learners <- create.Learner("SL.randomForest", tune = list(mtry = mtry_seq))
#learners


### XGBOOST TUNING

# Create 2 * 2 * 1 * 3 = 12 combinations of hyperparameters.
#learners_xgboost <- create.Learner("SL.xgboost", tune = list(ntrees = c(100, 500), 
#                                                             max_depth = c(1, 2), 
#                                                             minobspernode = 10,
#                                                             shrinkage = c(0.1, 0.01, 0.001)))
#learners_xgboost



# SL library
SL.library <- c("SL.randomForest", "SL.gbm", "SL.svm", "SL.ksvm",
                "SL.glmnet", "SL.glm", "SL.gam", "SL.xgboost", 
                "SL.bayesglm", "SL.mean")


# non-negative log likelihood loss function
# using CV.SuperLearner with 10-fold cross-validated risk estimates

# fit CV.SL using method.NNloglik
fit_T  <- CV.SuperLearner(Y = Y_T,  X = X, V = 10, SL.library = SL.library, verbose = FALSE, method = "method.NNloglik", family = binomial(), cvControl = list(stratifyCV = TRUE))
fit_M  <- CV.SuperLearner(Y = Y_M,  X = X, V = 10, SL.library = SL.library, verbose = FALSE, method = "method.NNloglik", family = binomial(), cvControl = list(stratifyCV = TRUE))
fit_Th <- CV.SuperLearner(Y = Y_Th, X = X, V = 10, SL.library = SL.library, verbose = FALSE, method = "method.NNloglik", family = binomial(), cvControl = list(stratifyCV = TRUE))
# fit_A  <- CV.SuperLearner(Y = Y_A,  X = X, V = 10, SL.library = SL.library, verbose = FALSE, method = "method.NNLS", family = binomial(), cvControl = list(stratifyCV = TRUE))

# examine the fits of the CV.SLs
summary(fit_T)
fit_T$coef
fit_T$whichDiscreteSL


summary(fit_M)
fit_M$coef
fit_M$whichDiscreteSL

summary(fit_Th)
fit_Th$coef
fit_Th$whichDiscreteSL

#summary(fit_A)
#fit_A$coef
#fit_A$whichDiscreteSL


# Use recombineCVSL to look at other methods
set.seed(12345)
fit_T2 <- recombineCVSL(fit_T, method = "method.NNLS", verbose = TRUE, saveAll = TRUE, parallel = "multicore")
fit_T3 <- recombineCVSL(fit_T, method = "method.AUC", verbose = TRUE, saveAll = TRUE, parallel = "multicore")

fit_M2 <- recombineCVSL(fit_M, method = "method.NNLS", verbose = TRUE, saveAll = TRUE, parallel = "multicore")
fit_M3 <- recombineCVSL(fit_M, method = "method.AUC", verbose = TRUE, saveAll = TRUE, parallel = "multicore")

fit_Th2 <- recombineCVSL(fit_Th, method = "method.NNLS", verbose = TRUE, saveAll = TRUE, parallel = "multicore")
fit_Th3 <- recombineCVSL(fit_Th, method = "method.AUC", verbose = TRUE, saveAll = TRUE, parallel = "multicore")



# examine & compare fits
fit_T2$coef
fit_T2$whichDiscreteSL
fit_T2$SL.predict
#fit_T2$discreteSL.predict
fit_T3$coef

fit_M2$coef
fit_M2$whichDiscreteSL
fit_M3$coef

fit_Th2$coef
fit_Th2$whichDiscreteSL
fit_Th3$coef


#fit_T2  <- CV.SuperLearner(Y = Y_T,  X = X, V = 10, SL.library = SL.library, verbose = FALSE, method = "method.AUC", family = binomial(), cvControl = list(stratifyCV = TRUE), parallel = "multicore")
#fit_M2  <- CV.SuperLearner(Y = Y_M,  X = X, V = 10, SL.library = SL.library, verbose = FALSE, method = "method.AUC", family = binomial(), cvControl = list(stratifyCV = TRUE), parallel = "multicore")
#fit_Th2 <- CV.SuperLearner(Y = Y_Th, X = X, V = 10, SL.library = SL.library, verbose = FALSE, method = "method.AUC", family = binomial(), cvControl = list(stratifyCV = TRUE), parallel = "multicore")
#fit_A2  <- CV.SuperLearner(Y = Y_A,  X = X, V = 10, SL.library = SL.library, verbose = FALSE, method = "method.AUC", family = binomial(), cvControl = list(stratifyCV = TRUE), parallel = "multicore")

#summary(fit_T2)
#fit_T2$coef
#fit_T2$AllSL

#summary(fit_M2)
#fit_M2$coef
#fit_M2$AllSL

#summary(fit_Th2)
#fit_Th2$coef
#fit_Th2$AllSL

#summary(fit_A2)
#fit_A2$coef
#fit_A2$AllSL


# Apply method.NNloglik SL predictions of roof types 
SL_pred <- data.frame(pred_T = fit_T$SL.predict, pred_M = fit_M$SL.predict, pred_Th = fit_Th$SL.predict)
SL_pred

# Apply method.NNloglik discrete SL predictions of roof types - NOTE: need to add back in Ashpalt if decide to use in future
SL_discrete_pred <-data.frame(pred_T = fit_T$discreteSL.predict, pred_M = fit_M$discreteSL.predict, pred_Th = fit_Th$discreteSL.predict)
SL_discrete_pred

Classify <- apply(SL_pred, 1, function(xx) c("Tile", "Metal", "Thach")[unname(which.max(xx))])
Classify_dp <- apply(SL_discrete_pred, 1, function(xx) c("Tile", "Metal", "Thach")[unname(which.max(xx))])

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

# Apply method.nnls discrete SL predictions of roof types - NOTE: need to add back in Ashpalt if decide to use in future
SL_discrete_pred_nnls <-data.frame(pred_T = fit_T2$discreteSL.predict, pred_M = fit_M2$discreteSL.predict, pred_Th = fit_Th2$discreteSL.predict)
SL_discrete_pred_nnls

Classify_nnls <- apply(SL_pred_nnls, 1, function(xx) c("Tile", "Metal", "Thach")[unname(which.max(xx))])
Classify_dp_nnls <- apply(SL_discrete_pred_nnls, 1, function(xx) c("Tile", "Metal", "Thach")[unname(which.max(xx))])

SL_pred_nnls_table <- table(Classify_nnls, Y)
SL_pred_nnls_table

Discrete_SL_nnls_table <- table(Classify_dp_nnls, Y)
Discrete_SL_nnls_table

# Calculate kappa statistics
# method.nnls SL prediction
SLP_nnls <- data.frame("pred" = Classify_nnls, "actual" = Y)
kappa2(SLP_nnls[,c(1,2)], "unweighted")

# method.nnls Discrete prediction
DP_nnls <- data.frame("pred_dp" = Classify_dp_nnls, "actual" = Y)
kappa2(DP_nnls[,c(1,2)], "unweighted")


# Do the same as the above but with method.AUC
# Apply method.auc SL predictions of roof types 
SL_pred_auc <- data.frame(pred_T = fit_T3$SL.predict, pred_M = fit_M3$SL.predict, pred_Th = fit_Th3$SL.predict)
SL_pred_auc

# Apply method.auc discrete SL predictions of roof types - NOTE: need to add back in Ashpalt if decide to use in future
SL_discrete_pred_auc <-data.frame(pred_T = fit_T3$discreteSL.predict, pred_M = fit_M3$discreteSL.predict, pred_Th = fit_Th3$discreteSL.predict)
SL_discrete_pred_auc

Classify_auc <- apply(SL_pred_auc, 1, function(xx) c("Tile", "Metal", "Thach")[unname(which.max(xx))])
Classify_dp_auc <- apply(SL_discrete_pred_auc, 1, function(xx) c("Tile", "Metal", "Thach")[unname(which.max(xx))])

SL_pred_auc_table <- table(Classify_auc, Y)
SL_pred_auc_table

Discrete_SL_auc_table <- table(Classify_dp_auc, Y)
Discrete_SL_auc_table


# Calculate kappa statistics
# method.auc SL prediction
SLP_auc <- data.frame("pred" = Classify_auc, "actual" = Y)
kappa2(SLP_auc[,c(1,2)], "unweighted")

# method.auc Discrete prediction
DP_auc <- data.frame("pred_dp" = Classify_dp_auc, "actual" = Y)
kappa2(DP_auc[,c(1,2)], "unweighted")


# Based on kappa statistic performance, method.auc slightly outperforms 
# method.NNLS & method.NNloglik. Previously, method.nnloglik had outperformed
# the other methods, so the code below reflects that. Will want to do further
# analysis moving forward to see if this is still true after tuning the 
# algorithms, so have not changed code below yet. Should also note that there 
# substantively no real difference b/w any of the methods, & can use the discrete 
# SL if desired since no real advantage of SL seen in CV.SuperLearner 
# validation above


# First need to call SL functions instead of CV.SuperLearner in order to be able to predict (???)

# Modify SL library used to get rid of those that didn't work well
SL.library2 <- c("SL.randomForest", "SL.xgboost", "SL.gbm", "SL.gam", "SL.glmnet")

# Fit SL functions w/ method.nnloglik & new SL library
fit_T_SL  <- SuperLearner(Y = Y_T,  X = X, SL.library = SL.library2, verbose = FALSE, method = "method.NNloglik", family = binomial())
fit_M_SL  <- SuperLearner(Y = Y_M,  X = X, SL.library = SL.library2, verbose = FALSE, method = "method.NNloglik", family = binomial())
fit_Th_SL <- SuperLearner(Y = Y_Th, X = X, SL.library = SL.library2, verbose = FALSE, method = "method.NNloglik", family = binomial())
#fit_A_SL  <- SuperLearner(Y = Y_A,  X = X, SL.library = SL.library2, verbose = FALSE, method = "method.AUC", family = binomial())

fit_T_SL
fit_M_SL
fit_Th_SL
#fit_A_SL


## missing something here... See Swazi line 274
## missing the predict back on same data. But likely won't be using that - just
## did that to make maps of training data. Will neeed to figure out if need to
## make maps of training data again.





# Then use 10% retained from original dataset
pred_T_vdf  <- predict(fit_T_SL, newdata = vdf)
pred_M_vdf  <- predict(fit_M_SL, newdata = vdf)
pred_Th_vdf <- predict(fit_Th_SL, newdata = vdf) 
#pred_A_vdf  <- predict(fit_A_SL, newdata = vdf) 

# from online SL intro:
#str(pred_T_vdf)
#summary(pred_T_vdf$library.predict)
#qplot(pred_T_vdf$pred) + theme_bw()
#qplot(Y_vdf, pred_T_vdf$pred) + theme_classic()


SL_pred_vdf <- data.frame(pred_T = pred_T_vdf$pred, pred_M = pred_M_vdf$pred, pred_Th = pred_Th_vdf$pred)
SL_pred_vdf

# NOTE: if need to re-run kappas, make sure "Thatch" is spelled "Thach" to match original dataset.
Classify_vdf <- apply(SL_pred_vdf, 1, function(xx) c("Tile", "Metal", "Thach")[unname(which.max(xx))])

SL_pred_vdf_table <- table(Classify_vdf, Y_vdf)
SL_pred_vdf_table

# Calculate kappa statistics for validation data
SLP_vdf <- data.frame("pred" = Classify_vdf, "actual" = Y_vdf)
kappa2(SLP_vdf[,c(1,2)], "unweighted")

# Add prediction probabilities & classifications to vdf
vdf$pred_T = pred_T_vdf$pred
vdf$pred_M = pred_M_vdf$pred
vdf$pred_Th = pred_Th_vdf$pred
vdf$pred_roof_type = Classify_vdf
head(vdf)

# Write out new dataset with roof predictions
write.csv(vdf, file = "Validation sentinel data with roof type pred-no tuning.csv")



# Next use entirely new dataset
df_new <- read.csv("/Users/amandairish/Desktop/Malaria project/BW_OSM_Sentinel_082817.csv")
head(df_new)

pred_T <- predict(fit_T_SL, newdata = df_new)
pred_M  <- predict(fit_M_SL, newdata = df_new)
pred_Th <- predict(fit_Th_SL, newdata = df_new) 
#pred_A  <- predict(fit_A_SL, newdata = df_new)

SL_pred_new <- data.frame(pred_T = pred_T$pred, pred_M = pred_M$pred, pred_Th = pred_Th$pred)

Classify_new <- apply(SL_pred_new, 1, function(xx) c("Tile", "Metal", "Thatch")[unname(which.max(xx))])
Classify_new

# Add prediction probabilities & classifications to df_new
df_new$pred_T = pred_T$pred
df_new$pred_M = pred_M$pred
df_new$pred_Th = pred_Th$pred
df_new$pred_roof_type = Classify_new
head(df_new)

# Write out new dataset with roof predictions
write.csv(df_new, file = "Sentinel data with roof type pred-no tuning.csv")

# Save all objects in global environment
save(list = ls(), file = "SL roof pred Botswana global env-no tuning.RData")

# Save workspace image
save.image(file = "SL roof pred image Botswana-no tuning.RData")


#######################

# For epi 264: predict back on known data (df) so have comparison maps

pred_T <- predict(fit_T_SL, newdata = df)
pred_M  <- predict(fit_M_SL, newdata = df)
pred_Th <- predict(fit_Th_SL, newdata = df) 
#pred_A  <- predict(fit_A_SL, newdata = df_new)

SL_pred_df <- data.frame(pred_T = pred_T$pred, pred_M = pred_M$pred, pred_Th = pred_Th$pred)

Classify_df <- apply(SL_pred_df, 1, function(xx) c("Tile", "Metal", "Thatch")[unname(which.max(xx))])
Classify_df

# Add prediction probabilities & classifications to df_new
df$pred_T = pred_T$pred
df$pred_M = pred_M$pred
df$pred_Th = pred_Th$pred
df$pred_roof_type = Classify_df
head(df)

# Write out new dataset with roof predictions
write.csv(df, file = "Sentinel data known and pred.csv")
