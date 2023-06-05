#### SET WORKING DIRECTORY #####
# setwd("~/Desktop/STATISTICAL PROJECT")
setwd("~/Documenti/Statistical_Learning/II Semestre/Progetto")
################################

###############################
          ## LIBRARY ##
library(corrplot)
library(leaps)
library(pROC)
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

##### CHECK FOR MISSING VALUES #####
sum(is.na(wdbc))
####################################

#### SUMMARY ####
# Toglierei il summary di ID visto che non serve a niente
summary(wdbc)
####################################

#### DATA PROPORTION ####
table(wdbc$diagnosis) #quanti B e quanti M 
table(wdbc$diagnosis)/length(wdbc$diagnosis) #proporzione di B e M
#####################################

##### CREATE MATRIX FOR EACH SET OF CELL (MEAN, SE, WORST) AND DIVIDE IN BASE OF THE TYPE (M, B)#####
##### MEAN #####
wdbc_mean <- wdbc[,3:12]
wdbc_mean <- as.matrix(wdbc_mean)

wdbc_mean_B <- wdbc_mean[wdbc[,'diagnosis']=="B",]
wdbc_mean_M <- wdbc_mean[wdbc[,'diagnosis']=="M",]

# trasformo queste due matrici in dataframe per poter plottare piu' facilmente
wdbc_mean_B <- as.data.frame(wdbc_mean_B)
wdbc_mean_M <- as.data.frame(wdbc_mean_M)
wdbc_mean <- as.data.frame(wdbc_mean)

##### CALCOLO LA PERCENTUALE DI B E M SU TUTTO IL DATASET #####
dim_B <- dim(wdbc_mean_B)[1]
dim_M <- dim(wdbc_mean_M)[1]

# Pie Chart with Percentages
slices <- c(dim_B, dim_M)
lbls <- c("Benign", "Malign")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=c("darkgreen", "red"),
    main="Percentage of Benign and Malign")

###################################################
                ##### PLOT #####
###################################################

##### BOX PLOT #####
# Radius_mean
boxplot(wdbc_mean$radius_mean,
        xlab = "Radius_mean",
        main = "Boxplot of radius mean")

# Texture_mean
boxplot(wdbc_mean$texture_mean,
        xlab = "Texture_mean",
        main = "Boxplot of texture mean")

# Perimeter_mean
boxplot(wdbc_mean$perimeter_mean,
        xlab = "Perimeter_mean",
        main = "Boxplot of perimeter mean")

# Area_mean
boxplot(wdbc_mean$area_mean,
        xlab = "Area_mean",
        main = "Boxplot of area mean")

# Smoothness_mean
boxplot(wdbc_mean$smoothness_mean,
        xlab = "Smoothness_mean",
        main = "Boxplot of smoothness mean")

# Compactness_mean
boxplot(wdbc_mean$compactness_mean,
        xlab = "Compactness_mean",
        main = "Boxplot of compactness mean")

# Concavity_mean
boxplot(wdbc_mean$concavity_mean,
        xlab = "Concavity_mean",
        main = "Boxplot of concavity mean")

# Concave_pts_mean
boxplot(wdbc_mean$concave_pts_mean,
        xlab = "Concave_pts_mean",
        main = "Boxplot of concave points mean")

# Radius_mean
boxplot(wdbc_mean$symmetry_mean,
        xlab = "Symmetry_mean",
        main = "Boxplot of symmetry mean")

# Fractal_dim_mean
boxplot(wdbc_mean$fractal_dim_mean,
        xlab = "Fractal_dim_mean",
        main = "Boxplot of fractal dimension mean")

##### DENSITY #####

# Density plot of RADIUS_MEAN of B e M
plot(density(wdbc_mean_B$radius_mean), col = "darkgreen", main ="Density plot", xlab = "Radius mean", xlim = c(0, 35), ylim = c(0, 0.23))
lines(density(wdbc_mean_M$radius_mean), col = "red")
lines(density(wdbc_mean$radius_mean), col = "lightblue")
legend(23, 0.225,  legend=c("Benign", "Malign", "Mean"),
       col=c("darkgreen","red", "lightblue"), lty=1, cex = 0.8,
       title="Line types", text.font=4)

# Density plot of TEXTURE_MEAN of B e M
plot(density(wdbc_mean_M$texture_mean), col = "red", main ="Density plot", xlab = "Texture mean")
lines(density(wdbc_mean_B$texture_mean), col = "darkgreen")
lines(density(wdbc_mean$texture_mean), col = "lightblue")
legend(30, 0.12,  legend=c("Benign", "Malign", "Mean"),
       col=c("darkgreen","red", "lightblue"), lty=1, cex = 0.8,
       title="Line types", text.font=4)

# Density plot of PERIMETER_MEAN of B e M
plot(density(wdbc_mean_B$perimeter_mean), col = "darkgreen", main ="Density plot", xlab = "Perimeter mean", xlim = c(0, 220) )
lines(density(wdbc_mean_M$perimeter_mean), col = "red")
lines(density(wdbc_mean$perimeter_mean), col = "lightblue")
legend(145, 0.033,  legend=c("Benign", "Malign", "Mean"),
       col=c("darkgreen","red", "lightblue"), lty=1, cex = 0.8,
       title="Line types", text.font=4)

# Density plot of AREA_MEAN of B e M
plot(density(wdbc_mean_B$area_mean), col = "darkgreen", main ="Density plot", xlab = "Area mean", xlim = c(0, 3000))
lines(density(wdbc_mean_M$area_mean), col = "red")
lines(density(wdbc_mean$area_mean), col = "lightblue")
legend(2000, 0.003,  legend=c("Benign", "Malign", "Mean"),
       col=c("darkgreen","red", "lightblue"), lty=1, cex = 0.8,
       title="Line types", text.font=4)

# Density plot of SMOOTHNESS_MEAN of B e M
plot(density(wdbc_mean_B$smoothness_mean), col = "darkgreen", main ="Density plot", xlab = "Smoothness mean")
lines(density(wdbc_mean_M$smoothness_mean), col = "red")
lines(density(wdbc_mean$smoothness_mean), col = "lightblue")
legend(0.13, 30,  legend=c("Benign", "Malign", "Mean"),
       col=c("darkgreen","red", "lightblue"), lty=1, cex = 0.8,
       title="Line types", text.font=4)

# Density plot of COMPACTNESS_MEAN of B e M
plot(density(wdbc_mean_B$compactness_mean), col = "darkgreen", main ="Density plot", xlab = "Compactness mean", xlim = c(0, 0.45))
lines(density(wdbc_mean_M$compactness_mean), col = "red")
lines(density(wdbc_mean$compactness_mean), col = "lightblue")
legend(0.3, 13,  legend=c("Benign", "Malign", "Mean"),
       col=c("darkgreen","red", "lightblue"), lty=1, cex = 0.8,
       title="Line types", text.font=4)

# Density plot of CONCAVITY_MEAN of B e M
plot(density(wdbc_mean_B$concavity_mean), col = "darkgreen", main ="Density plot", xlab = "Concavity mean", xlim = c(0, 0.5))
lines(density(wdbc_mean_M$concavity_mean), col = "red")
lines(density(wdbc_mean$concavity_mean), col = "lightblue")
legend(0.32, 15,  legend=c("Benign", "Malign", "Mean"),
       col=c("darkgreen","red", "lightblue"), lty=1, cex = 0.8,
       title="Line types", text.font=4)

# Density plot of CONCAVE_MEAN of B e M
plot(density(wdbc_mean_B$concave_pts_mean), col = "darkgreen", main ="Density plot", xlab = "Concave mean", xlim = c(0, 0.25))
lines(density(wdbc_mean_M$concave_pts_mean), col = "red")
lines(density(wdbc_mean$concave_pts_mean), col = "lightblue")
legend(0.16, 30,  legend=c("Benign", "Malign", "Mean"),
       col=c("darkgreen","red", "lightblue"), lty=1, cex = 0.8,
       title="Line types", text.font=4)

# Density plot of SYMMETRY_MEAN of B e M
plot(density(wdbc_mean_B$symmetry_mean), col = "darkgreen", main ="Density plot", xlab = "Symmetry mean", xlim = c(0, 0.35))
lines(density(wdbc_mean_M$symmetry_mean), col = "red")
lines(density(wdbc_mean$symmetry_mean), col = "lightblue")
legend(0.22, 17,  legend=c("Benign", "Malign", "Mean"),
       col=c("darkgreen","red", "lightblue"), lty=1, cex = 0.8,
       title="Line types", text.font=4)

# Density plot of FRACTAL_MEAN of B e M
plot(density(wdbc_mean_B$fractal_dim_mean), col = "darkgreen", main ="Density plot", xlab = "Fractal mean", xlim = c(0, 0.11))
lines(density(wdbc_mean_M$fractal_dim_mean), col = "red")
lines(density(wdbc_mean$fractal_dim_mean), col = "lightblue")
legend(0.074, 75,  legend=c("Benign", "Malign", "Mean"),
       col=c("darkgreen","red", "lightblue"), lty=1, cex = 0.8,
       title="Line types", text.font=4)

##### FINE DENSITY PLOT #####


##### SCATTER PLOT #####
pairs(~radius_mean + texture_mean + perimeter_mean + area_mean + 
        smoothness_mean + compactness_mean + concavity_mean + concave_pts_mean +
        symmetry_mean + fractal_dim_mean, data = wdbc_mean, main = "Scatterplot mean")

pairs(~radius_mean + texture_mean + perimeter_mean + area_mean + 
        smoothness_mean + compactness_mean + concavity_mean + concave_pts_mean +
        symmetry_mean + fractal_dim_mean, data = wdbc_mean_B, main = "Scatterplot mean of Benign")

pairs(~radius_mean + texture_mean + perimeter_mean + area_mean + 
        smoothness_mean + compactness_mean + concavity_mean + concave_pts_mean +
        symmetry_mean + fractal_dim_mean, data = wdbc_mean_M, main = "Scatterplot mean of Malign")

##### FINE SCATTER PLOT #####

##### COVARIANCE MATRIX #####

# Benign
cov_mat_B <- cov(wdbc_mean_B) # creo una matrice delle covarianze



D_B <- diag(cov_mat_B) # salvo solo la diagonale della matrice
D_B <- diag(D_B) # faccio diventare la diagonale una matrice di tutti 0 con solo i valori nella diagonale

cor_mat_B <- solve(sqrt(D_B))%*%cov_mat_B%*%solve(sqrt(D_B)) #correlation matrix

cov2cor(cov_mat_B) # convert covariance matrix to correlation
cor_B <- cor(wdbc_mean_B)

colnames(cor_B) = c( 'radius', 'texture', 'perim', 'area', 
                         'smoot', 'compact', 'conc', 'conc_pts', 'sym',
                         'fractal') #cambio nomi per leggibilita 
rownames(cor_B)=c( 'radius', 'texture', 'perim', 'area', 
                       'smoot', 'compact', 'conc', 'conc_pts', 'sym',
                       'fractal') #cambio nomi per leggibilita 
#pairs(wdbc_mean_B, main = "Covariance Matrix of Benign")
corrplot.mixed(cor_B, diag = 'n',
               upper = 'square',
               lower = 'number',
               addgrid.col = 'black',
               tl.col = 'black')

# Malign
cov_mat_M <- cov(wdbc_mean_M)
D_M <- diag(cov_mat_M)
D_M <- diag(D_M)

cor_mat_M <- solve(sqrt(D_M))%*%cov_mat_M%*%solve(sqrt(D_M)) 

cov2cor(cov_mat_M)
cor_M<-cor(wdbc_mean_M)

colnames(cor_M) = c( 'radius', 'texture', 'perim', 'area', 
                     'smoot', 'compact', 'conc', 'conc_pts', 'sym',
                     'fractal') #cambio nomi per leggibilita 
rownames(cor_M)=c( 'radius', 'texture', 'perim', 'area', 
                   'smoot', 'compact', 'conc', 'conc_pts', 'sym',
                   'fractal') #cambio nomi per leggibilita 
#pairs(wdbc_mean_M)
corrplot.mixed(cor_M, diag = 'n',
               upper = 'square',
               lower = 'number',
               addgrid.col = 'black',
               tl.col = 'black')
# Mean
cov_mat_median <- cov(wdbc_mean)
D_median <- diag(cov_mat_median)
D_median <- diag(D_median)

cor_mat_median <- solve(sqrt(D_median))%*%cov_mat_median%*%solve(sqrt(D_median)) 

cov2cor(cov_mat_median)
cor_BM<- cor(wdbc_mean)

colnames(cor_BM) = c( 'radius', 'texture', 'perim', 'area', 
                     'smoot', 'compact', 'conc', 'conc_pts', 'sym',
                     'fractal') #cambio nomi per leggibilita 
rownames(cor_BM)=c( 'radius', 'texture', 'perim', 'area', 
                   'smoot', 'compact', 'conc', 'conc_pts', 'sym',
                   'fractal') #cambio nomi per leggibilita 
corrplot.mixed(cor_BM, diag = 'n',
               upper = 'square',
               lower = 'number',
               addgrid.col = 'black',
               tl.col = 'black')
#pairs(wdbc_mean)

##### FINE COVARIANCE MATRIX #####
################################################################################
                    ##### FINE PLOT #####
################################################################################

################################################################################
                    ##### MODELS #####
################################################################################

## Logistic Regression for Classification

# Creo il dataset in cui fare le previsioni
wdbc_mean_ds <- wdbc[,2:12]

# Change diagnosis values "B" -> 1, "M" -> 0
wdbc_mean_ds$diagnosis <- ifelse(wdbc_mean_ds$diagnosis == "B", 1, 0)
attach(wdbc_mean_ds)

# Divido il dataset in Train e Test
set.seed(123)
sample <- sample(c(T, F), nrow(wdbc_mean_ds), replace =TRUE, prob = c(0.8, 0.2))
train <- wdbc_mean_ds[sample, ] 
test <- wdbc_mean_ds[!sample, ] 

# Training phase on all data using all columns
lr_model <- glm(diagnosis ~ radius_mean + texture_mean + perimeter_mean + area_mean + smoothness_mean + compactness_mean +
                concavity_mean + concave_pts_mean + symmetry_mean + fractal_dim_mean,
              data = train, family = binomial)

summary(lr_model) # return a warning "glm.fit: si sono verificate probabilità stimate numericamente pari a 0 o 1"

# Prediction phase on test set
predictions <- predict(lr_model, test, type = "response")
# Se stampo predictions vedo che mancano dei valori...

# Stampo confusion matrix per vedere come sta classificando
logistic_predictions01 <- rep(0, length(predictions))
logistic_predictions01[predictions > 0.5] <- 1
CM <- table(logistic_predictions01, test$diagnosis) 
# rearrange rows and columns
CM <- CM[2:1, 2:1]
CM <- addmargins(CM, margin = c(1, 2))
CM

# Calcolo FPR = False Positive Rate
fpr_1 <- 5/44
fpr_1

# Calcolo TNR = True Negative Rate
tnr_1 <- 39/43
tnr_1

# Plotto ROC curve per vedere come procede
roc.out1 <- roc(test$diagnosis, predictions, levels=c("0", "1"))
plot(roc.out1)
plot(roc.out1, legacy.axes=TRUE)
plot(roc.out1,  print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")

#AUC
auc(roc.out1)

# Provo a vedere se eliminando alcune colonne la situazione migliora
regfit.full <- regsubsets(diagnosis ~. ,data = wdbc_mean_ds, nvmax=10)
reg.summary <- summary(regfit.full)
# An asterisk ("*") indicates that a given variable is included in the corresponding model.
reg.summary$outmat
reg.summary$rsq # OUTPUT R2

### Model2: Concave, Texture, Radius
lr_model2 <- glm(diagnosis ~ concave_pts_mean + texture_mean + radius_mean, data = train, family = binomial)
summary(lr_model2)
predictions2 <- predict(lr_model2, test, type = "response")

# Stampo confusion matrix per vedere come sta classificando
logistic_predictions01_2 <- rep(0, length(predictions2))
logistic_predictions01_2[predictions2 > 0.5] <- 1
CM_2 <- table(logistic_predictions01_2, test$diagnosis) 
CM_2 <- CM_2[2:1, 2:1]
CM_2 <- addmargins(CM_2, margin = c(1, 2))
CM_2

# Calcolo FPR = False Positive Rate
fpr_2 <- 5/44
fpr_2

# Calcolo TNR = True Negative Rate
tnr_2 <- 39/41
tnr_2

# Plotto ROC curve per vedere come procede
roc.out2 <- roc(test$diagnosis, predictions2, levels=c("0", "1"))
plot(roc.out2)
plot(roc.out2, legacy.axes=TRUE)
plot(roc.out2,  print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")

### Model 3: Dopo aver osservato la CORRELATION MATRIX elimino Perimeter e Area perchè fortemente correlato tra loro e con raggio
lr_model3 <- glm(diagnosis ~ radius_mean + texture_mean + smoothness_mean + compactness_mean + concavity_mean + 
                   concave_pts_mean + symmetry_mean + fractal_dim_mean, data = train, family = binomial)
summary(lr_model3)
predictions3 <- predict(lr_model3, test, type = "response")

# Stampo confusion matrix per vedere come sta classificando
logistic_predictions01_3 <- rep(0, length(predictions3))
logistic_predictions01_3[predictions3 > 0.5] <- 1
CM_3 <- table(logistic_predictions01_3, test$diagnosis) 
CM_3 <- CM_3[2:1, 2:1]
CM_3 <- addmargins(CM_3, margin = c(1, 2))
CM_3

# Calcolo FPR = False Positive Rate
fpr_3 <- 5/44
fpr_3

# Calcolo TNR = True Negative Rate
tnr_3 <- 39/44
tnr_3

# Plot ROC curve per vedere come procede
roc.out3 <- roc(test$diagnosis, predictions3, levels=c("0", "1"))
plot(roc.out3)
plot(roc.out3, legacy.axes=TRUE)
plot(roc.out3,  print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate")

## Plot all three ROC-curve
plot(roc.out1, col = "blue")
lines(roc.out2, col = "red")
lines(roc.out3, col = "green")

# Add legend
legend("bottomright", legend = c("AUC_1 = 0.977", "AUC_2 = 0.982", "AUC_3 = 0.987"), col = c("blue", "red", "green"), lty = 1)

## Confronto delle varie Confusion Matrix

## Conclusioni