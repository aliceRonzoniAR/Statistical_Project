#### SET WORKING DIRECTORY #####
# setwd("~/Desktop/STATISTICAL PROJECT")
setwd("~/Documenti/Statistical_Learning/II Semestre/Progetto")

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

##### CREATE MATRIX#####
# Transform the df into a matrix (all columns except ID)
wdbc_as_matrix <- as.matrix(wdbc[-c(1, 2)])
wdbc_as_matrix <- as.matrix(wdbc_as_matrix)

# trasformo la matrice in dataframe
wdbc_df <- as.data.frame(wdbc_as_matrix)

################################################################################
                            ##### MODELS #####
################################################################################

#### DIVISION TRAINING-VALIDATION-TEST ####
set.seed(161)
sample <- sample(1:569, size=455, replace= FALSE)

# Dati non normalizzati
wdbc_train <- wdbc_df[sample,] #trining set
wdbc_test <- wdbc_df[-sample,] #test set

diagnosis <- wdbc$diagnosis[sample]
diagnosis

############# K-NN ############# 
## Prova 1
kmax <- 50 #cerco il K per K-nn che mi fornisce l'errore piú piccolo
err <- rep(0,kmax)
for (l in 1:kmax){
  knn_predictor <- knn(wdbc_train, wdbc_test, wdbc$diagnosis[sample], k=l)
  err[l] <- mean(knn_predictor != wdbc$diagnosis[-sample])
}
k <- which.min(err)
k

#faccio l'analisi con il k trovato
knn_predictor <- knn(wdbc_train, wdbc_test, wdbc$diagnosis[sample], k)
err_k <- mean(knn_predictor != wdbc$diagnosis[-sample])
err_k

# Confusion Matrix
CM <- table(knn_predictor, wdbc$diagnosis[-sample])
CM <- addmargins(CM, margin = c(1, 2))
CM

##### B & M PLOT ####
## Scelgo due colonne poco  correlate: radius_mean e symmetry_mean di wdbc_df
BB <- knn_predictor =="B"
plot(wdbc_df$radius_mean[-sample], wdbc_df$symmetry_mean[-sample], col = BB + 2)  
legend(20, 0.30, legend=c("Benign", "Malign"),
       col = c(2,3), pch = 1, cex = 0.8,
       title = "Data types", text.font=4)
#####################

# Prova 2
# Faccio lo stesso bel modello di logistic regression su k-nn per vedere se migliora/peggiora la situa
wdbc_train_lr <- wdbc_train[-c(23, 15, 14, 5, 4, 25, 22, 24, 9, 13, 11, 26, 19, 28, 6, 10, 27, 17, 18, 20)]
wdbc_test_lr <- wdbc_test[-c(23, 15, 14, 5, 4, 25, 22, 24, 9, 13, 11, 26, 19, 28, 6, 10, 27, 17, 18, 20)]

kmax <- 50 #cerco il K per K-nn che mi fornisce l'errore piú piccolo
err_lr <- rep(0,kmax)
for (l in 1:kmax){
  knn_predictor_lr <- knn(wdbc_train_lr, wdbc_test_lr, wdbc$diagnosis[sample], k=l)
  err_lr[l] <- mean(knn_predictor_lr != wdbc$diagnosis[-sample])
}
k <- which.min(err_lr)
k

#faccio l'analisi con il k trovato
knn_predictor_lr <- knn(wdbc_train_lr, wdbc_test_lr, wdbc$diagnosis[sample], k)
err_k_lr <- mean(knn_predictor_lr != wdbc$diagnosis[-sample])
err_k_lr

# Confusion Matrix
CM_lr <- table(knn_predictor_lr, wdbc$diagnosis[-sample])
CM_lr <- addmargins(CM_lr, margin = c(1, 2))
CM_lr

# Prova 3
# Faccio lo stesso bel modello di logistic regression (la prima versione, confusion matrix) su k-nn per vedere se migliora/peggiora la situa
wdbc_train_lr <- wdbc_train[-c(23, 15, 14, 5, 4, 25, 22, 24, 9)]
wdbc_test_lr <- wdbc_test[-c(23, 15, 14, 5, 4, 25, 22, 24, 9)]

kmax <- 50 #cerco il K per K-nn che mi fornisce l'errore piú piccolo
err_lr <- rep(0,kmax)
for (l in 1:kmax){
  knn_predictor_lr <- knn(wdbc_train_lr, wdbc_test_lr, wdbc$diagnosis[sample], k=l)
  err_lr[l] <- mean(knn_predictor_lr != wdbc$diagnosis[-sample])
}
k <- which.min(err_lr)
k

#faccio l'analisi con il k trovato
knn_predictor_lr <- knn(wdbc_train_lr, wdbc_test_lr, wdbc$diagnosis[sample], k)
err_k_lr <- mean(knn_predictor_lr != wdbc$diagnosis[-sample])
err_k_lr

# Confusion Matrix
CM_lr <- table(knn_predictor_lr, wdbc$diagnosis[-sample])
CM_lr <- addmargins(CM_lr, margin = c(1, 2))
CM_lr


## Calcolo i valori di FPR, TPR, PPR, NPR per ogni prova
FPR1 <- 4/43
TPR1 <- 68/71
PPR1 <- 68/72
NPR1 <- 39/42

FPR2 <- 10/43
TPR2 <- 68/71
PPR2 <- 68/78
NPR2 <- 33/36

FPR3 <- 10/43
TPR3 <- 70/71
PPR3 <- 70/80
NPR3 <- 33/34
