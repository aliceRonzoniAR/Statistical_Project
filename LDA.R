#### SET WORKING DIRECTORY #####
#setwd("~/Desktop/STATISTICAL PROJECT/ file divisi")
setwd("~/Documenti/Statistical_Learning/II Semestre/Progetto")

###############################
## LIBRARY ##
library(corrplot)
library(leaps)
library(pROC)
library(class)
library(MASS)
library(car)
#library(tidyverse)
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

##### CREATE MATRIX#####
# Transform the df into a matrix (all columns except ID)
wdbc_as_matrix <- as.matrix(wdbc[-c(1, 2)])
wdbc_as_matrix <- as.matrix(wdbc_as_matrix)

# trasformo la matrice in dataframe
wdbc_df <- as.data.frame(wdbc_as_matrix)

################################################################################
##### MODELS #####
################################################################################



##### CREATE MATRIX FOR EACH SET OF CELL (MEAN, SE, WORST) AND DIVIDE IN BASE OF THE TYPE (M, B)#####
##### MEAN #####
wdbc_df <- as.data.frame(wdbc[-1])
wdbc_df$diagnosis[1]
# B -> 1 M -> 0
wdbc_df$diagnosis <- ifelse(wdbc_df$diagnosis == "B", 1, 0)
wdbc_df$diagnosis[1]


#### DIVISION TRAINING-VALIDATION-TEST ####
set.seed(161)
sample <- sample(1:569, size=455, replace= FALSE)

# Dati non normalizzati
wdbc_train <- wdbc_df[sample,] #trining set
wdbc_test <- wdbc_df[-sample,] #test set

diagnosis <- wdbc$diagnosis[sample]
#diagnosis

############################################

#### LDA model con tutte e 30 le features e su tutto il dataset ###
lda_model <- lda(diagnosis ~.,  data = wdbc_df)
lda_model
plot(lda_model)
plot(lda_model, type="density")
lda_pred<-predict(lda_model,wdbc_test,type = "response")
#il trashold é 0.5 di base
lda_class <- lda_pred$class
CM <-table(lda_class,wdbc_test$diagnosis)
CM <- CM[2:1, 2:1]
CM <- addmargins(CM, margin = c(1, 2))
CM

err<-mean(lda_class!=wdbc_test$diagnosis)
err

precision <- 70/76
precision

#### LDA sul modello ottenuto dopo il VIF della logistic regression
lda_model_0 <- lda(diagnosis ~ . -area_worst - perimeter_mean-radius_mean-perimeter_SE-area_SE-fractal_dim_SE-perimeter_worst -concave_pts_mean-compactness_worst 
                   -concave_pts_SE-fractal_dim_worst-texture_worst-compactness_mean-radius_worst-compactness_SE ,
                   data = wdbc_train)
lda_model_0
plot(lda_model_0)
plot(lda_model_0, type="density")
lda_pred_0<-predict(lda_model_0,wdbc_test,type = "response",xlim=c(-10,10),ylim=c(0,2))
#il trashold é 0.5 di base
lda_class_0 <- lda_pred_0$class
CM <- table(lda_class_0,wdbc_test$diagnosis)
CM <- CM[2:1, 2:1]
CM <- addmargins(CM, margin = c(1, 2))
CM

err_0<-mean(lda_class_0!=wdbc_test$diagnosis)
err_0

ggplot(as.data.frame(lda_pred_0), aes(x=LD1, y=0,col=wdbc_test$diagnosis )) + geom_point(alpha=0.5) #da capire il significato ihihihih

### MODELLO DELLA LOGISTIC REGRESSION 23
lda_model_lr <- lda(diagnosis ~ . -area_worst - perimeter_mean-radius_mean-perimeter_SE-area_SE-fractal_dim_SE-perimeter_worst -concave_pts_mean-compactness_worst 
                    -concave_pts_SE-fractal_dim_worst-texture_worst-compactness_mean-radius_worst-compactness_SE -texture_SE -smoothness_mean-concavity_worst-smoothness_SE
                    -symmetry_mean-concavity_SE-concave_pts_worst,
                    data = wdbc_train)
lda_model_lr
plot(lda_model_lr)
plot(lda_model_lr, type="density")

lda_pred_lr <- predict(lda_model_lr, wdbc_test, type = "response", xlim=c(-10,10), ylim=c(0,2))
#il trashold é 0.5 di base 
lda_class_lr <- lda_pred_lr$class
CM_lr <- table(lda_class_lr, wdbc_test$diagnosis)
CM_lr <- CM_lr[2:1, 2:1]
CM_lr <- addmargins(CM_lr, margin = c(1, 2))
CM_lr

err_lr <- mean(lda_class_lr != wdbc_test$diagnosis)
err_lr

precision <- 71/79
precision