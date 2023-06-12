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
###############################

##### OPEN FILE #####
wdbc <- read.csv("wdbc.data",header=FALSE)
#wdbc <- read.csv("./Dati/Dataset_2/wdbc.data",header=FALSE)
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
summary(wdbc[-1])
####################################

attach(wdbc)

#### DATA PROPORTION ####
table(diagnosis) #quanti B e quanti M 
table(diagnosis)/length(diagnosis) #proporzione di B e M
slices<-c(table(diagnosis))
lbls <- c("Benign", "Malign")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=c(3,2),
    main="Percentage of Benign and Malign")
#####################################

##### B & M PLOT ####
B <-diagnosis =="B"
par(mfrow=c(2, 2))

plot(radius_mean, symmetry_mean,col=B+2) #scelgo due colonne poco  correlate 
#legend(24, 0.31, legend=c("Benign", "Malign"),
#col=c(2,3),pch=1, cex = 0.8,
#title="Data types", text.font=4)
plot(radius_mean, perimeter_mean,col=B+2) #scelgo due colonne poco  correlate 
#legend(24, 0.31, legend=c("Benign", "Malign"),
#col=c(2,3),pch=1, cex = 0.8,
#title="Data types", text.font=4)
plot(radius_mean, fractal_dim_mean,col=B+2) #scelgo due colonne poco  correlate 
#legend(24, 0.31, legend=c("Benign", "Malign"),
#col=c(2,3),pch=1, cex = 0.8,
#title="Data types", text.font=4)
plot(radius_mean, concave_pts_mean,col=B+2) #scelgo due colonne poco  correlate 
#legend(24, 0.31, legend=c("Benign", "Malign"),
#col=c(2,3),pch=1, cex = 0.8,
#title="Data types", text.font=4)
par(mfrow=c(1, 1))
#####################

##### CREATE MATRIX FOR EACH SET OF CELL (MEAN, SE, WORST) AND DIVIDE IN BASE OF THE TYPE (M, B)#####
##### MEAN #####

# Transform the df into a matrix (all columns except ID)
wdbc_as_matrix <- as.matrix(wdbc[-c(1, 2)])
wdbc_as_matrix <- as.matrix(wdbc_as_matrix)

wdbc_B <- wdbc_as_matrix[wdbc[,'diagnosis']=="B",]
wdbc_M <- wdbc_as_matrix[wdbc[,'diagnosis']=="M",]

# trasformo queste due matrici in dataframe per poter plottare piu' facilmente
wdbc_B <- as.data.frame(wdbc_B)
wdbc_M <- as.data.frame(wdbc_M)
wdbc_df <- as.data.frame(wdbc_as_matrix)

###################################################
                ##### PLOT #####
###################################################

attach(wdbc_df)

##### BOX PLOT #####
# Radius
boxplot(radius_mean, radius_SE, radius_worst,
        xlab = "radius",
        main = "radius_mean VS radius_SE VS radius_worst")

# Texture
boxplot(texture_mean, texture_SE, texture_worst,
        xlab = "Texture",
        main = "texture_mean VS texture_SE VS texture_worst")

# Perimeter
boxplot(perimeter_mean, perimeter_SE, perimeter_worst,
        xlab = "Perimeter",
        main = "perimeter_mean VS perimeter_SE VS perimeter_worst")

# Area
boxplot(area_mean, area_SE, area_worst,
        xlab = "Area",
        main = "area_mean VS area_SE VS area_worst")

# Smoothness
boxplot(smoothness_mean, smoothness_SE, smoothness_worst,
        xlab = "Smoothness",
        main = "smoothness_mean VS smoothness_SE VS smoothness_worst")

# Compactness
boxplot(compactness_mean, compactness_SE, compactness_worst,
        xlab = "Compactness",
        main = "compactness_mean VS compactness_SE VS compactness_worst")

# Concavity
boxplot(concavity_mean, concavity_SE, concavity_worst,
        xlab = "Concavity",
        main = "concavity_mean VS concavity_SE VS concavity_worst")

# Concave_pts_mean
boxplot(concave_pts_mean, concave_pts_SE, concave_pts_worst,
        xlab = "Concave_pts",
        main = "concave_pts_mean VS concave_pts_SE VS concave_pts_worst")

# compactness_mean
boxplot(symmetry_mean, symmetry_SE, symmetry_worst,
        xlab = "Symmetry",
        main = "symmetry_mean VS symmetry_SE VS symmetry_worst")

# Fractal_dim_mean
boxplot(fractal_dim_mean, fractal_dim_SE, fractal_dim_worst,
        xlab = "Fractal_dim",
        main = "fractal_dim_mean VS fractal_dim_SE VS fractal_dim_worst")

##### DENSITY #####

# Density plot of compactness
par(mfrow=c(3, 1))
plot(density(wdbc_B$radius_mean), col = 2, main ="Density plot", xlab = "radius mean", xlim = c(0, 35), ylim = c(0, 0.23))
lines(density(wdbc_M$radius_mean), col = 3)
lines(density(wdbc$radius_mean), col = "lightblue")
plot(density(wdbc_B$radius_SE), col = 2, main ="Density plot", xlab = "radius SE", xlim = c(0, 35), ylim = c(0, 0.23))
lines(density(wdbc_M$radius_SE), col = 3)
lines(density(wdbc$radius_SE), col = "lightblue")
plot(density(wdbc_B$radius_worst), col = 2, main ="Density plot", xlab = "radius worst", xlim = c(0, 35), ylim = c(0, 0.23))
lines(density(wdbc_M$radius_worst), col = 3)
lines(density(wdbc$radius_worst), col = "lightblue")
# legend(23, 0.225,  legend=c("Benign", "Malign", "Mean"),
#        col=c(2,3, "lightblue"), lty=1, cex = 0.8,
#        title="Line types", text.font=4)

# Density plot of TEXTURE
plot(density(wdbc_B$texture_mean), col = 2, main ="Density plot", xlab = "Texture mean", ylim = c(0, 0.12))
lines(density(wdbc_M$texture_mean), col = 3)
lines(density(wdbc$texture_mean), col = "lightblue")
plot(density(wdbc_B$texture_SE), col = 2, main ="Density plot", xlab = "Texture SE", ylim = c(0, 0.12))
lines(density(wdbc_M$texture_SE), col = 3)
lines(density(wdbc$texture_SE), col = "lightblue")
plot(density(wdbc_B$texture_worst), col = 2, main ="Density plot", xlab = "Texture worst", ylim = c(0, 0.12))
lines(density(wdbc_M$texture_worst), col = 3)
lines(density(wdbc$texture_worst), col = "lightblue")
# legend(30, 0.12,  legend=c("Benign", "Malign", "Mean"),
#        col=c(2,3, "lightblue"), lty=1, cex = 0.8,
#        title="Line types", text.font=4)

# Density plot of PERIMETER
plot(density(wdbc_B$perimeter_mean), col = 2, main ="Density plot", xlab = "Perimeter mean",  xlim = c(0, 220) )
lines(density(wdbc_M$perimeter_mean), col = 3)
lines(density(wdbc$perimeter_mean), col = "lightblue")
plot(density(wdbc_B$perimeter_SE), col = 2, main ="Density plot", xlab = "Perimeter SE", xlim = c(0, 220) )
lines(density(wdbc_M$perimeter_SE), col = 3)
lines(density(wdbc$perimeter_SE), col = "lightblue")
plot(density(wdbc_B$perimeter_worst), col = 2, main ="Density plot", xlab = "Perimeter worst", xlim = c(0, 220) )
lines(density(wdbc_M$perimeter_worst), col = 3)
lines(density(wdbc$perimeter_worst), col = "lightblue")
# legend(145, 0.033,  legend=c("Benign", "Malign", "Mean"),
#        col=c(2,3, "lightblue"), lty=1, cex = 0.8,
#        title="Line types", text.font=4)

# Density plot of AREA
plot(density(wdbc_B$area_mean), col = 2, main ="Density plot", xlab = "Area mean", xlim = c(0, 3000))
lines(density(wdbc_M$area_mean), col = 3)
lines(density(wdbc$area_mean), col = "lightblue")
plot(density(wdbc_B$area_SE), col = 2, main ="Density plot", xlab = "Area SE", xlim = c(0, 3000))
lines(density(wdbc_M$area_SE), col = 3)
lines(density(wdbc$area_SE), col = "lightblue")
plot(density(wdbc_B$area_worst), col = 2, main ="Density plot", xlab = "Area worst",  xlim = c(0, 3000))
lines(density(wdbc_M$area_worst), col = 3)
lines(density(wdbc$area_worst), col = "lightblue")
# legend(2000, 0.003,  legend=c("Benign", "Malign", "Mean"),
#        col=c(2,3, "lightblue"), lty=1, cex = 0.8,
#        title="Line types", text.font=4)

# Density plot of SMOOTHNESS 
plot(density(wdbc_B$smoothness_mean), col = 2, main ="Density plot", xlab = "Smoothness mean")
lines(density(wdbc_M$smoothness_mean), col = 3)
lines(density(wdbc$smoothness_mean), col = "lightblue")
plot(density(wdbc_B$smoothness_SE), col = 2, main ="Density plot", xlab = "Smoothness SE")
lines(density(wdbc_M$smoothness_SE), col = 3)
lines(density(wdbc$smoothness_SE), col = "lightblue")
plot(density(wdbc_B$smoothness_worst), col = 2, main ="Density plot", xlab = "Smoothness worst")
lines(density(wdbc_M$smoothness_worst), col = 3)
lines(density(wdbc$smoothness_worst), col = "lightblue")
# legend(0.13, 30,  legend=c("Benign", "Malign", "Mean"),
#        col=c(2,3, "lightblue"), lty=1, cex = 0.8,
#        title="Line types", text.font=4)

# Density plot of COMPACTNESS
plot(density(wdbc_B$compactness_mean), col = 2, main ="Density plot", xlab = "compactness mean", xlim = c(0, 0.45))
lines(density(wdbc_M$compactness_mean), col = 3)
lines(density(wdbc$compactness_mean), col = "lightblue")
plot(density(wdbc_B$compactness_SE), col = 2, main ="Density plot", xlab = "compactness SE",  xlim = c(0, 0.45))
lines(density(wdbc_M$compactness_SE), col = 3)
lines(density(wdbc$compactness_SE), col = "lightblue")
plot(density(wdbc_B$compactness_worst), col = 2, main ="Density plot", xlab = "compactness worst",  xlim = c(0, 0.45))
lines(density(wdbc_M$compactness_worst), col = 3)
lines(density(wdbc$compactness_worst), col = "lightblue")
# legend(0.3, 13,  legend=c("Benign", "Malign", "Mean"),
#        col=c(2,3, "lightblue"), lty=1, cex = 0.8,
#        title="Line types", text.font=4)

# Density plot of CONCAVITY
plot(density(wdbc_B$concavity_mean), col = 2, main ="Density plot", xlab = "Concavity mean", xlim = c(0, 0.5))
lines(density(wdbc_M$concavity_mean), col = 3)
lines(density(wdbc$concavity_mean), col = "lightblue")
plot(density(wdbc_B$concavity_SE), col = 2, main ="Density plot", xlab = "Concavity SE",  xlim = c(0, 0.5))
lines(density(wdbc_M$concavity_SE), col = 3)
lines(density(wdbc$concavity_SE), col = "lightblue")
plot(density(wdbc_B$concavity_worst), col = 2, main ="Density plot", xlab = "Concavity worst",  xlim = c(0, 0.5))
lines(density(wdbc_M$concavity_worst), col = 3)
lines(density(wdbc$concavity_worst), col = "lightblue")
# legend(0.32, 15,  legend=c("Benign", "Malign", "Mean"),
#        col=c(2,3, "lightblue"), lty=1, cex = 0.8,
#        title="Line types", text.font=4)

# Density plot of CONCAVE_PTS
plot(density(wdbc_B$concave_pts_mean), col = 2, main ="Density plot", xlab = "Concave mean", xlim = c(0, 0.25))
lines(density(wdbc_M$concave_pts_mean), col = 3)
lines(density(wdbc$concave_pts_mean), col = "lightblue")
plot(density(wdbc_B$concave_pts_SE), col = 2, main ="Density plot", xlab = "Concave SE", xlim = c(0, 0.25))
lines(density(wdbc_M$concave_pts_SE), col = 3)
lines(density(wdbc$concave_pts_SE), col = "lightblue")
plot(density(wdbc_B$concave_pts_worst), col = 2, main ="Density plot", xlab = "Concave worst", xlim = c(0, 0.25))
lines(density(wdbc_M$concave_pts_worst), col = 3)
lines(density(wdbc$concave_pts_worst), col = "lightblue")
# legend(0.16, 30,  legend=c("Benign", "Malign", "Mean"),
#        col=c(2,3, "lightblue"), lty=1, cex = 0.8,
#        title="Line types", text.font=4)

# Density plot of SYMMETRY
plot(density(wdbc_B$symmerty_mean), col = 2, main ="Density plot", xlab = "Symmerty mean", xlim = c(0, 35))
lines(density(wdbc_M$symmerty_mean), col = 3)
lines(density(wdbc$symmerty_mean), col = "lightblue")
plot(density(wdbc_B$symmerty_SE), col = 2, main ="Density plot", xlab = "Symmerty SE", xlim = c(0, 35))
lines(density(wdbc_M$symmerty_SE), col = 3)
lines(density(wdbc$symmerty_SE), col = "lightblue")
plot(density(wdbc_B$symmerty_worst), col = 2, main ="Density plot", xlab = "Symmerty worst", xlim = c(0, 35))
lines(density(wdbc_M$symmerty_worst), col = 3)
lines(density(wdbc$symmerty_worst), col = "lightblue")
# legend(0.22, 17,  legend=c("Benign", "Malign", "Mean"),
#        col=c(2,3, "lightblue"), lty=1, cex = 0.8,
#        title="Line types", text.font=4)

# Density plot of FRACTAL_DIM
plot(density(wdbc_B$fractal_dim_mean), col = 2, main ="Density plot", xlab = "fractal_dim mean",xlim = c(0, 0.11))
lines(density(wdbc_M$fractal_dim_mean), col = 3)
lines(density(wdbc$fractal_dim_mean), col = "lightblue")
plot(density(wdbc_B$fractal_dim_SE), col = 2, main ="Density plot", xlab = "fractal_dim SE", xlim = c(0, 0.11))
lines(density(wdbc_M$fractal_dim_SE), col = 3)
lines(density(wdbc$fractal_dim_SE), col = "lightblue")
plot(density(wdbc_B$fractal_dim_worst), col = 2, main ="Density plot", xlab = "fractal_dim worst", xlim = c(0, 0.11))
lines(density(wdbc_M$fractal_dim_worst), col = 3)
lines(density(wdbc$fractal_dim_worst), col = "lightblue")
# legend(0.074, 75,  legend=c("Benign", "Malign", "Mean"),
#        col=c(2,3, "lightblue"), lty=1, cex = 0.8,
#        title="Line types", text.font=4)
par(mfrow=c(1, 1))
##### FINE DENSITY PLOT #####

##### COVARIANCE MATRIX #####

# Benign
cov_mat_B <- cov(wdbc_mean_B) # creo una matrice delle covarianze

D_B <- diag(cov_mat_B) # salvo solo la diagonale della matrice
D_B <- diag(D_B) # faccio diventare la diagonale una matrice di tutti 0 con solo i valori nella diagonale

cor_mat_B <- solve(sqrt(D_B))%*%cov_mat_B%*%solve(sqrt(D_B)) #correlation matrix

cov2cor(cov_mat_B) # convert covariance matrix to correlation
cor_B <- cor(wdbc_mean_B)

colnames(cor_B) = c( 'radius_dim', 'texture', 'perim', 'area', 
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

## Malign
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

corrplot.mixed(cor_M, diag = 'n',
               upper = 'square',
               lower = 'number',
               addgrid.col = 'black',
               tl.col = 'black')

## Mean
cov_mat <- cov(wdbc[-c(1,2)])
D_median <- diag(cov_mat)
D_median <- diag(D_median)

cor_mat_median <- solve(sqrt(D_median))%*%cov_mat%*%solve(sqrt(D_median)) 

cov2cor(cov_mat)


###################### corrplot ####################
cor_BM<- cor(wdbc[-c(1,2)])
colnames(cor_BM) = c( 'radius_mean', 'texture_mean', 'perim_mean', 'area_mean', 
                      'smoot_mean', 'compact_mean', 'conc_mean', 'conc_pts_mean', 'sym_mean',
                      'fractal_mean','radius_se', 'texture_se', 'perim_se', 'area_se', 
                      'smoot_se', 'compact_se', 'conc_se', 'conc_pts_se', 'sym_se',
                      'fractal_se','radius_wrs', 'texture_wrs', 'perim_wrs', 'area_wrs', 
                      'smoot_wrs', 'compact_wrs', 'conc_wrs', 'conc_pts_wrs', 'sym_wrs',
                      'fractal_wrs') #cambio nomi per leggibilita 
rownames(cor_BM)=c( 'radius_mean', 'texture_mean', 'perim_mean', 'area_mean', 
                    'smoot_mean', 'compact_mean', 'conc_mean', 'conc_pts_mean', 'sym_mean',
                    'fractal_mean','radius_se', 'texture_se', 'perim_se', 'area_se', 
                    'smoot_se', 'compact_se', 'conc_se', 'conc_pts_se', 'sym_se',
                    'fractal_se','radius_wrs', 'texture_wrs', 'perim_wrs', 'area_wrs', 
                    'smoot_wrs', 'compact_wrs', 'conc_wrs', 'conc_pts_wrs', 'sym_wrs',
                    'fractal_wrs') #cambio nomi per leggibilita 

corrplot(cor_BM, method = 'square', diag = FALSE, order = 'hclust',
         addrect = 3, rect.col = 'blue', rect.lwd = 2 )

cor_BM<- cor(wdbc[-c(1,2,23:32)])
colnames(cor_BM) = c( 'radius_mean', 'texture_mean', 'perim_mean', 'area_mean', 
                      'smoot_mean', 'compact_mean', 'conc_mean', 'conc_pts_mean', 'sym_mean',
                      'fractal_mean','radius_se', 'texture_se', 'perim_se', 'area_se', 
                      'smoot_se', 'compact_se', 'conc_se', 'conc_pts_se', 'sym_se',
                      'fractal_se') #cambio nomi per leggibilita 
rownames(cor_BM)=c( 'radius_mean', 'texture_mean', 'perim_mean', 'area_mean', 
                    'smoot_mean', 'compact_mean', 'conc_mean', 'conc_pts_mean', 'sym_mean',
                    'fractal_mean','radius_se', 'texture_se', 'perim_se', 'area_se', 
                    'smoot_se', 'compact_se', 'conc_se', 'conc_pts_se', 'sym_se',
                    'fractal_se') #cambio nomi per leggibilita 

corrplot(cor_BM, method = 'number', diag = FALSE, order = 'hclust', type='lower',
         addrect = 3, rect.col = 'blue', rect.lwd = 2 )

##### FINE COVARIANCE MATRIX #####
################################################################################
                    ##### FINE PLOT #####
################################################################################

