#### SET WORKING DIRECTORY #####
# setwd("~/Desktop/STATISTICAL PROJECT")
setwd("~/Documenti/Statistical_Learning/II Semestre/Progetto")
################################

###############################
## LIBRARY ##
library(corrplot)
library(leaps)
library(pROC)
library(class)
library(MASS)
###############################

##### OPEN FILE #####
#wdbc <- read.csv("wdbc.data",header=FALSE)
wdbc <- read.csv("./Dati/Dataset_2/wdbc.data",header=FALSE)
#####################

##### ADD NAMES TO THE COLUMNS #####
colnames(wdbc) = c('id', 'diagnosis', 'radius_mean', 'texture_mean', 'perimeter_mean', 'area_mean', 
                   'smoothness_mean', 'compactness_mean', 'concavity_mean', 'concave_pts_mean', 'symmetry_mean',
                   'fractal_dim_mean','radius_SE', 'texture_SE', 'perimeter_SE', 'area_SE', 
                   'smoothness_SE', 'compactness_SE', 'concavity_SE', 'concave_pts_SE', 'symmetry_SE',
                   'fractal_dim_SE','radius_worst', 'texture_worst', 'perimeter_worst', 'area_worst', 
                   'smoothness_worst', 'compactness_worst', 'concavity_worst', 'concave_pts_worst', 'symmetry_worst',
                   'fractal_dim_worst')

##### CREATE MATRIX FOR EACH SET OF CELL (MEAN, SE, WORST) AND DIVIDE IN BASE OF THE TYPE (M, B)#####
##### MEAN #####
wdbc_df <- as.data.frame(wdbc[-1])
wdbc_df$diagnosis[1]
# B -> 1 M -> 0
wdbc_df$diagnosis <- ifelse(wdbc_df$diagnosis == "B", 1, 0)
wdbc_df$diagnosis[1]

# Train e Test set
set.seed(161)
sample <- sample(1:569, size=455, replace= FALSE)
wdbc_train <- wdbc_df[sample,] #trining set
wdbc_test <- wdbc_df[-sample,] #test set

attach(wdbc_df)

# Summary del model (tolte alte correlazioni) su tutto il dataset
lr_model_0 <- glm(diagnosis ~ . - texture_worst - area_SE - perimeter_SE - area_mean - perimeter_mean
                  - area_worst - radius_worst - perimeter_worst - concave_pts_mean,
                  data = wdbc_df, family = binomial)
summary(lr_model_0)

### GUardando la correlation matrix abbiamo tenuto quelli che funzionano meglio

# lr_model_1 <- glm(diagnosis ~ . - texture_worst - area_SE - perimeter_SE - area_mean - perimeter_mean
#                   - area_worst - radius_worst - perimeter_worst - concave_pts_mean,
#                   data = wdbc_train, family = binomial)
# 
# summary(lr_model_1)


## COMMENTO:
# Stampando i summary dei due modelli, abbiao visto che quello totale (lr_model_0) performa meglio
# Questa cosa ha senso perche' non ha a che fare con un fattore di casualita': la divisione del df.
# Per questa ragione, ora procederemo a scegliere il miglior modello sul df completo e poi lo applicheremo
# al train sperando il Signore che vada comunque bene. Altrimenti pace e vedremo cosa fare

# Tolgo il piu' alto: txture_SE
lr_model_1 <- glm(diagnosis ~ . - texture_worst - area_SE - perimeter_SE - area_mean - perimeter_mean
                  - area_worst - radius_worst - perimeter_worst - concave_pts_mean - texture_SE,
                  data = wdbc_df, family = binomial)
summary(lr_model_1)

# Tolgo il piu' alto: fractal_dim_mean
lr_model_2 <- glm(diagnosis ~ . - texture_worst - area_SE - perimeter_SE - area_mean - perimeter_mean
                  - area_worst - radius_worst - perimeter_worst - concave_pts_mean - texture_SE
                  - fractal_dim_mean,
                  data = wdbc_df, family = binomial)
summary(lr_model_2)

# Tolgo il piu' alto: smoothness_worst
lr_model_3 <- glm(diagnosis ~ . - texture_worst - area_SE - perimeter_SE - area_mean - perimeter_mean
                  - area_worst - radius_worst - perimeter_worst - concave_pts_mean - texture_SE
                  - fractal_dim_mean - smoothness_worst,
                  data = wdbc_df, family = binomial)
summary(lr_model_3)

# Tolgo il piu' alto: concave_pts_SE
lr_model_4 <- glm(diagnosis ~ . - texture_worst - area_SE - perimeter_SE - area_mean - perimeter_mean
                  - area_worst - radius_worst - perimeter_worst - concave_pts_mean - texture_SE
                  - fractal_dim_mean - smoothness_worst - concave_pts_SE,
                  data = wdbc_df, family = binomial)
summary(lr_model_4)

# Tolgo il piu' alto: concavity_worst
lr_model_5 <- glm(diagnosis ~ . - texture_worst - area_SE - perimeter_SE - area_mean - perimeter_mean
                  - area_worst - radius_worst - perimeter_worst - concave_pts_mean - texture_SE
                  - fractal_dim_mean - smoothness_worst - concave_pts_SE - concavity_worst,
                  data = wdbc_df, family = binomial)
summary(lr_model_5)

# Tolgo il piu' alto: smoothness_mean
lr_model_6 <- glm(diagnosis ~ . - texture_worst - area_SE - perimeter_SE - area_mean - perimeter_mean
                  - area_worst - radius_worst - perimeter_worst - concave_pts_mean - texture_SE
                  - fractal_dim_mean - smoothness_worst - concave_pts_SE - concavity_worst
                  - smoothness_mean,
                  data = wdbc_df, family = binomial)
summary(lr_model_6)

# Tolgo il piu' alto: symmetry_mean
lr_model_7 <- glm(diagnosis ~ . - texture_worst - area_SE - perimeter_SE - area_mean - perimeter_mean
                  - area_worst - radius_worst - perimeter_worst - concave_pts_mean - texture_SE
                  - fractal_dim_mean - smoothness_worst - concave_pts_SE - concavity_worst
                  - smoothness_mean - symmetry_mean,
                  data = wdbc_df, family = binomial)
summary(lr_model_7)

# Tolgo il piu' alto: compactness_worst
lr_model_8 <- glm(diagnosis ~ . - texture_worst - area_SE - perimeter_SE - area_mean - perimeter_mean
                  - area_worst - radius_worst - perimeter_worst - concave_pts_mean - texture_SE
                  - fractal_dim_mean - smoothness_worst - concave_pts_SE - concavity_worst
                  - smoothness_mean - symmetry_mean - compactness_worst,
                  data = wdbc_df, family = binomial)
summary(lr_model_8)

# Tolgo il piu' alto: compactness_SE
lr_model_9 <- glm(diagnosis ~ . - texture_worst - area_SE - perimeter_SE - area_mean - perimeter_mean
                  - area_worst - radius_worst - perimeter_worst - concave_pts_mean - texture_SE
                  - fractal_dim_mean - smoothness_worst - concave_pts_SE - concavity_worst
                  - smoothness_mean - symmetry_mean - compactness_worst - compactness_SE,
                  data = wdbc_df, family = binomial)
summary(lr_model_9)

# Tolgo il piu' alto: concavity_SE
lr_model_10 <- glm(diagnosis ~ . - texture_worst - area_SE - perimeter_SE - area_mean - perimeter_mean
                  - area_worst - radius_worst - perimeter_worst - concave_pts_mean - texture_SE
                  - fractal_dim_mean - smoothness_worst - concave_pts_SE - concavity_worst
                  - smoothness_mean - symmetry_mean - compactness_worst - compactness_SE - concavity_SE,
                  data = wdbc_df, family = binomial)
summary(lr_model_10)

# Tolgo il piu' alto: symmetry_SE
lr_model_11 <- glm(diagnosis ~ . - texture_worst - area_SE - perimeter_SE - area_mean - perimeter_mean
                   - area_worst - radius_worst - perimeter_worst - concave_pts_mean - texture_SE
                   - fractal_dim_mean - smoothness_worst - concave_pts_SE - concavity_worst
                   - smoothness_mean - symmetry_mean - compactness_worst - compactness_SE - concavity_SE
                   - symmetry_SE,
                   data = wdbc_df, family = binomial)
summary(lr_model_11)

# Tolgo il piu' alto: symmetry_worst
lr_model_12 <- glm(diagnosis ~ . - texture_worst - area_SE - perimeter_SE - area_mean - perimeter_mean
             - area_worst - radius_worst - perimeter_worst - concave_pts_mean - texture_SE
             - fractal_dim_mean - smoothness_worst - concave_pts_SE - concavity_worst
             - smoothness_mean - symmetry_mean - compactness_worst - compactness_SE - concavity_SE
             - symmetry_SE - symmetry_worst,
             data = wdbc_df, family = binomial)
summary(lr_model_12)


# DAto questo modello, che quardando i p-value risulta essere il migliore, lo apllico al train set
lr_train <- glm(diagnosis ~ . - texture_worst - area_SE - perimeter_SE - area_mean - perimeter_mean 
                - area_worst - radius_worst - perimeter_worst - concave_pts_mean - texture_SE
                - fractal_dim_mean - smoothness_worst - concave_pts_SE - concavity_worst
                - smoothness_mean - symmetry_mean - compactness_worst - compactness_SE - concavity_SE
                - symmetry_SE - symmetry_worst,
                data = wdbc_train, family = binomial)
summary(lr_train)

# Dato che performa bene anche sul train, vediamo il test e la confusion Matrix
predictions <- predict(lr_train, wdbc_test, type = "response")
logistic_predictions <- rep(0, length(predictions))
logistic_predictions[predictions > 0.5] <- 1
CM <- table(logistic_predictions, wdbc_test$diagnosis) 
# rearrange rows and columns
CM <- CM[2:1, 2:1]
CM <- addmargins(CM, margin = c(1, 2))
CM


## COMMENTO: per poter plottare la ROC curve e vedere delle differenze calcolo train e test 
lr_prova_train <- glm(diagnosis ~ . - texture_worst - area_SE - perimeter_SE - area_mean - perimeter_mean
                  - area_worst - radius_worst - perimeter_worst - concave_pts_mean,
                  data = wdbc_train, family = binomial)

# Predictions su test di lr_prova_train
predictions_prova <- predict(lr_prova_train, wdbc_test, type = "response")

# ROC Curve
roc.out12 <- roc(wdbc_test$diagnosis, predictions, levels=c("0", "1"))
plot(roc.out12, print.auc=FALSE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate", col = 4)

roc.out_prova <- roc(wdbc_test$diagnosis, predictions_prova, levels=c("0", "1"))
lines(roc.out_prova, col = 10 , print.auc=FALSE)

# Add legend
legend("bottomright", legend = c("AUC_1 = 0.9921", "AUC_2 = 0.9419"), col = c(4, 10), lty = 1)

