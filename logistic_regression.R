#### SET WORKING DIRECTORY #####
setwd("~/Desktop/STATISTICAL PROJECT/file divisi")
#setwd("~/Documenti/Statistical_Learning/II Semestre/Progetto")
################################

###############################
## LIBRARY ##
library(corrplot)
library(leaps)
library(pROC)
library(class)
library(MASS)
library(car)
library(ggplot2)
###############################

##### OPEN FILE #####
wdbc <- read.csv("wdbc.data",header=FALSE)
#wdbc <- read.csv("./Dati/Dataset_2/wdbc.data",header=FALSE)
######################

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



# Summary del model su tutto il dataset
lr_model_0 <- glm(diagnosis ~ . ,
                  data = wdbc_train, family = binomial)
summary(lr_model_0)


vif(lr_model_0)

# Tolgo il vif piu' alto:area_worst

lr_model_1 <- glm(diagnosis ~ . -area_worst,
                  data = wdbc_train, family = binomial)
summary(lr_model_1)


vif(lr_model_1)


# Tolgo il vif piu' alto:perimeter_mean
lr_model_2 <- glm(diagnosis ~ . -area_worst - perimeter_mean,
                  data = wdbc_train, family = binomial)
summary(lr_model_2)


vif(lr_model_2)

# Tolgo il vif piu' alto:radius_mean
lr_model_3 <- glm(diagnosis ~ . -area_worst - perimeter_mean-radius_mean,
                  data = wdbc_train, family = binomial)

vif(lr_model_3)

# Tolgo il vif piu' alto:perimeter_SE
lr_model_4 <- glm(diagnosis ~ . -area_worst - perimeter_mean-radius_mean-perimeter_SE,
                  data = wdbc_train, family = binomial)
vif(lr_model_4)


# Tolgo il vif piu' alto: area_SE
lr_model_5 <- glm(diagnosis ~ . -area_worst - perimeter_mean-radius_mean-perimeter_SE-area_SE,
                  data = wdbc_train, family = binomial)
vif(lr_model_5)



# Tolgo il vif piu' alto:fractal_dim_SE 
lr_model_6 <- glm(diagnosis ~ . -area_worst - perimeter_mean-radius_mean-perimeter_SE-area_SE-fractal_dim_SE ,
                  data = wdbc_train, family = binomial)
vif(lr_model_6)


# Tolgo il vif piu' alto:perimeter_worst
lr_model_7 <- glm(diagnosis ~ . -area_worst - perimeter_mean-radius_mean-perimeter_SE-area_SE-fractal_dim_SE-perimeter_worst ,
                  data = wdbc_train, family = binomial)

vif(lr_model_7)


# Tolgo il vif piu' alto: concave_pts_mean
lr_model_8 <- glm(diagnosis ~ . -area_worst - perimeter_mean-radius_mean-perimeter_SE-area_SE-fractal_dim_SE-perimeter_worst -concave_pts_mean,
                  data = wdbc_train, family = binomial)
vif(lr_model_8)


# Tolgo il vif piu' alto: compactness_worst 
lr_model_9 <- glm(diagnosis ~ . -area_worst - perimeter_mean-radius_mean-perimeter_SE-area_SE-fractal_dim_SE-perimeter_worst -concave_pts_mean-compactness_worst ,
                  data = wdbc_train, family = binomial)
vif(lr_model_9)


# Tolgo il vif piu' alto: concave_pts_SE 
lr_model_10 <- glm(diagnosis ~ . -area_worst - perimeter_mean-radius_mean-perimeter_SE-area_SE-fractal_dim_SE-perimeter_worst -concave_pts_mean-compactness_worst 
                   -concave_pts_SE,
                  data = wdbc_train, family = binomial)

vif(lr_model_10)



# Tolgo il vif piu' alto: fractal_dim_worst
lr_model_11 <- glm(diagnosis ~ . -area_worst - perimeter_mean-radius_mean-perimeter_SE-area_SE-fractal_dim_SE-perimeter_worst -concave_pts_mean-compactness_worst 
                   -concave_pts_SE-fractal_dim_worst,
                   data = wdbc_train, family = binomial)

vif(lr_model_11)

# Tolgo il vif piu' alto: concavity_mean 
lr_model_12 <- glm(diagnosis ~ . -area_worst - perimeter_mean-radius_mean-perimeter_SE-area_SE-fractal_dim_SE-perimeter_worst -concave_pts_mean-compactness_worst 
                   -concave_pts_SE-fractal_dim_worst-texture_worst ,
                   data = wdbc_train, family = binomial)

vif(lr_model_12)

# Tolgo il vif piu' alto: compactness_mean
lr_model_13 <- glm(diagnosis ~ . -area_worst - perimeter_mean-radius_mean-perimeter_SE-area_SE-fractal_dim_SE-perimeter_worst -concave_pts_mean-compactness_worst 
                   -concave_pts_SE-fractal_dim_worst-texture_worst-compactness_mean,
                   data = wdbc_train, family = binomial)

vif(lr_model_13)


# Tolgo il vif piu' alto: radius_worst
lr_model_14 <- glm(diagnosis ~ . -area_worst - perimeter_mean-radius_mean-perimeter_SE-area_SE-fractal_dim_SE-perimeter_worst -concave_pts_mean-compactness_worst 
                   -concave_pts_SE-fractal_dim_worst-texture_worst-compactness_mean-radius_worst,
                   data = wdbc_train, family = binomial)

vif(lr_model_14)

# Tolgo il vif piu' alto: compactness_SE
lr_model_15 <- glm(diagnosis ~ . -area_worst - perimeter_mean-radius_mean-perimeter_SE-area_SE-fractal_dim_SE-perimeter_worst -concave_pts_mean-compactness_worst 
                   -concave_pts_SE-fractal_dim_worst-texture_worst-compactness_mean-radius_worst-compactness_SE ,
                   data = wdbc_train, family = binomial)

vif(lr_model_15)
summary(lr_model_15)

# Tolgo il vif piu' alto: symmetry_worst 
lr_model_16 <- glm(diagnosis ~ . -area_worst - perimeter_mean-radius_mean-perimeter_SE-area_SE-fractal_dim_SE-perimeter_worst -concave_pts_mean-compactness_worst 
                   -concave_pts_SE-fractal_dim_worst-texture_worst-compactness_mean-radius_worst-compactness_SE ,
                   data = wdbc_train, family = binomial)

vif(lr_model_16)
summary(lr_model_16)


## COMMENTO:
# una volta aver scelto un modello decente grazie a vif, dopo aver quindi tolto/ diminnuito
# la collinearitá, scegliamo le features migliori per il nostro modello con il back stepwise coso

# Tolgo il piu' alto: texture_SE
lr_model_17 <- glm(diagnosis ~ . -area_worst - perimeter_mean-radius_mean-perimeter_SE-area_SE-fractal_dim_SE-perimeter_worst -concave_pts_mean-compactness_worst 
                   -concave_pts_SE-fractal_dim_worst-texture_worst-compactness_mean-radius_worst-compactness_SE -texture_SE ,
                   data = wdbc_train, family = binomial)
summary(lr_model_17)

# Tolgo il piu' alto: smoothness_mean
lr_model_18 <- glm(diagnosis ~ . -area_worst - perimeter_mean-radius_mean-perimeter_SE-area_SE-fractal_dim_SE-perimeter_worst -concave_pts_mean-compactness_worst 
                   -concave_pts_SE-fractal_dim_worst-texture_worst-compactness_mean-radius_worst-compactness_SE -texture_SE -smoothness_mean,
                   data = wdbc_train, family = binomial)
summary(lr_model_18)


# Tolgo il piu' alto: concavity_worst
lr_model_19 <- glm(diagnosis ~ . -area_worst - perimeter_mean-radius_mean-perimeter_SE-area_SE-fractal_dim_SE-perimeter_worst -concave_pts_mean-compactness_worst 
                   -concave_pts_SE-fractal_dim_worst-texture_worst-compactness_mean-radius_worst-compactness_SE -texture_SE -smoothness_mean-concavity_worst,
                   data = wdbc_train, family = binomial)
summary(lr_model_19)

# Tolgo il piu' alto: smoothness_SE
lr_model_20 <- glm(diagnosis ~ . -area_worst - perimeter_mean-radius_mean-perimeter_SE-area_SE-fractal_dim_SE-perimeter_worst -concave_pts_mean-compactness_worst 
                   -concave_pts_SE-fractal_dim_worst-texture_worst-compactness_mean-radius_worst-compactness_SE -texture_SE -smoothness_mean-concavity_worst-smoothness_SE,
                   data = wdbc_train, family = binomial)

summary(lr_model_20)

# Tolgo il piu' alto: symmetry_mean
lr_model_21 <- glm(diagnosis ~ . -area_worst - perimeter_mean-radius_mean-perimeter_SE-area_SE-fractal_dim_SE-perimeter_worst -concave_pts_mean-compactness_worst 
                   -concave_pts_SE-fractal_dim_worst-texture_worst-compactness_mean-radius_worst-compactness_SE -texture_SE -smoothness_mean-concavity_worst-smoothness_SE
                   -symmetry_mean,
                   data = wdbc_train, family = binomial)

summary(lr_model_21)

# Tolgo il piu' alto: concavity_SE
lr_model_22 <- glm(diagnosis ~ . -area_worst - perimeter_mean-radius_mean-perimeter_SE-area_SE-fractal_dim_SE-perimeter_worst -concave_pts_mean-compactness_worst 
                   -concave_pts_SE-fractal_dim_worst-texture_worst-compactness_mean-radius_worst-compactness_SE -texture_SE -smoothness_mean-concavity_worst-smoothness_SE
                   -symmetry_mean-concavity_SE,
                   data = wdbc_train, family = binomial)

summary(lr_model_22)
# Tolgo il piu' alto: concave_pts_worst
lr_model_23 <- glm(diagnosis ~ . -area_worst - perimeter_mean-radius_mean-perimeter_SE-area_SE-fractal_dim_SE-perimeter_worst -concave_pts_mean-compactness_worst 
                   -concave_pts_SE-fractal_dim_worst-texture_worst-compactness_mean-radius_worst-compactness_SE -texture_SE -smoothness_mean-concavity_worst-smoothness_SE
                   -symmetry_mean-concavity_SE-concave_pts_worst,
                   data = wdbc_train, family = binomial)

summary(lr_model_23)
vif(lr_model_23)


####################
#vediamo il test e la confusion Matrix with different tresholds
########### t=0.5
predictions <- predict(lr_model_23, wdbc_test, type = "response")
logistic_predictions <- rep(0, length(predictions))
logistic_predictions[predictions > 0.5] <- 1
CM <- table(logistic_predictions, wdbc_test$diagnosis) 
# rearrange rows and columns
CM <- CM[2:1, 2:1]
CM <- addmargins(CM, margin = c(1, 2))
CM
err<-mean(logistic_predictions!=wdbc_test$diagnosis)
err
####################
# ROC Curve
roc.out23 <- roc(wdbc_test$diagnosis, predictions, levels=c("0", "1"))
plot(roc.out23, legacy.axes = TRUE)
plot(roc.out23, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")
####################

######################## GRAFICO LOGISTIC ####################################

#logistic curve con dati nella curva
predicted_data<- data.frame(predicted = predictions, BM = wdbc_test$diagnosis)
predicted_data<- predicted_data[order(predicted_data$predicted, decreasing = FALSE),]
predicted_data$rank<-  1:nrow(predicted_data)

a<- ggplot(data = predicted_data, aes(x = rank, y = predicted)) +
  geom_point(aes(color = as.factor(BM)), alpha = 1, shape = 1, stroke = 1) +
  xlab("Index")+ylab("Predicted probability")+
  ggtitle("Estimated Logistic Curve - Simple GLM")
plot(a)


########################################

#logistic curve con linea a separare i dati 
plot(wdbc_test$texture_mean, wdbc_test$diagnosis, pch=20)

# function to compute the inverse of the logit
inv.logit <- function(beta0, beta1,beta2,beta3,beta4,beta5,beta6,beta7,beta8, x1,x2,x3,x4,x5,x6,x7,x8) {
  y <- exp(beta0+beta1*(x1)+beta2*(x2)+beta3*(x3)+beta4*(x4)+beta5*(x5)+beta6*(x6)+beta7*(x7)+beta8*(x8))
  return(y/(1+y))
}
x1 <- seq(9, 40, length=100)#texture_mean
x2<- seq(140, 2500, length=100) # area_mean
x3 <- seq(0, 0.5, length=100) # concavity_mean
x4 <- seq(0.04, 0.1, length=100)#fractal_dim_mean
x5 <- seq(0.1, 2.9, length=100)#radius_SE
x6 <- seq(0.007,0.08, length=100)# symmetry_SE 
x7 <- seq(0.07, 0.3, length=100)# smoothness_worst 
x8 <- seq(0.1, 0.7, length=100)#  symmetry_worst
beta.hat <- coefficients(lr_model_23)
beta0.hat <- beta.hat[1]
beta1.hat <- beta.hat[2]
beta2.hat <- beta.hat[3]
beta3.hat <- beta.hat[4]
beta4.hat <- beta.hat[5]
beta5.hat <- beta.hat[6]
beta6.hat <- beta.hat[7]
beta7.hat <- beta.hat[8]
beta8.hat <- beta.hat[9]
y <- inv.logit(beta0.hat, beta1.hat, beta2.hat, beta3.hat,beta4.hat,beta5.hat,beta6.hat,beta7.hat,beta8.hat, x1,x2,x3,x4,x5,x6,x7,x8)
lines(x, y, col="blue", lwd=1.5)
############################################################





############ t=0.4
predictions <- predict(lr_model_23, wdbc_test, type = "response")
logistic_predictions <- rep(0, length(predictions))
logistic_predictions[predictions > 0.4] <- 1
CM <- table(logistic_predictions, wdbc_test$diagnosis) 
# rearrange rows and columns
CM <- CM[2:1, 2:1]
CM <- addmargins(CM, margin = c(1, 2))
CM
err<-mean(logistic_predictions!=wdbc_test$diagnosis)
err

# ROC Curve
roc.out23 <- roc(wdbc_test$diagnosis, predictions, levels=c("0", "1"))
plot(roc.out23, legacy.axes = TRUE)
plot(roc.out23, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")
####################



############ t=0.3
predictions <- predict(lr_model_23, wdbc_test, type = "response")
logistic_predictions <- rep(0, length(predictions))
logistic_predictions[predictions > 0.3] <- 1
CM <- table(logistic_predictions, wdbc_test$diagnosis) 
# rearrange rows and columns
CM <- CM[2:1, 2:1]
CM <- addmargins(CM, margin = c(1, 2))
CM
err<-mean(logistic_predictions!=wdbc_test$diagnosis)
err

# ROC Curve
roc.out23 <- roc(wdbc_test$diagnosis, predictions, levels=c("0", "1"))
plot(roc.out23, legacy.axes = TRUE)
plot(roc.out23, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")
####################

