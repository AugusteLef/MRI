#Outlier Detection
# WARNING : DATA SET USED FOR OUTLIER DETECTION MUST BE ENTIERLY FILLING IN (NO MISSING VALUES)
# HERE WE USED THE MEAN METHOD TO FILLING MISSING VALUES, REPLACE "MEAN" BY "MEDIAN" or "KNN" TO USE ANOTHER METHOD


######################################## PERCENTILE ################################################
#######################TEST###################
x_train = read.csv("x_train_mean.csv")
x_train

#EXAMPLE for first column

#Check density function in order to see whether are not the distribution is a Gaussian distribution or not
d <- density(x_train[,2])
plot(d)

#Get the lower bound 
lower_bound <- quantile(x_train[,2], 0.025)
lower_bound

#Get the upper bound
upper_bound <- quantile(x_train[,2], 0.975)
upper_bound

#Get samples index (raw) of the outlier
outlier_ind <- which(x_train[,2] < lower_bound | x_train[,2] > upper_bound)
outlier_ind 

#Get the number of outlier
nbr <- length(outlier_ind)
nbr

######################SELECTION AND REPLACEMENT##################
#############TRAININGSET################
#FOR ALL THE DATA SET USING MEAN
x_train = read.csv("x_train_mean.csv")
x_train

#Check all coloumn outlier
for(i in 2:ncol(x_train)){
  lower_bound <- quantile(x_train[,i], 0.025)
  upper_bound <- quantile(x_train[,i], 0.975)
  outlier_ind <- which(x_train[,i] < lower_bound | x_train[,i] > upper_bound)
  #replace outlier by zeros
  if (length(outlier_ind) > 0) {
    for (j in 1:length(outlier_ind)){
      x_train[outlier_ind[j], i] <- NA
    }
  }
}

#The data set with NAs instead of outlier
x_train

##############TESTSET##################
x_test = read.csv("x_test_mean.csv")
x_test

#Check all coloumn outlier
for(i in 2:ncol(x_test)){
  lower_bound <- quantile(x_test[,i], 0.025)
  upper_bound <- quantile(x_test[,i], 0.975)
  outlier_ind <- which(x_test[,i] < lower_bound | x_test[,i] > upper_bound)
  #replace outlier by zeros
  if (length(outlier_ind) > 0) {
    for (j in 1:length(outlier_ind)){
      x_test[outlier_ind[j], i] <- NA
    }
  }
}

#The data set with NAs instead of outlier
x_test


############WRITTING PROCESS################
write.csv(x_train, "x_train_mean_percentile.csv", row.names = FALSE)
write.csv(x_test, "x_test_mean_percentile.csv", row.names = FALSE)







######################################### HAMPEL ####################################################
#############TEST####################
x_train = read.csv("x_train_mean.csv")
x_train

#EXAMPLE for first column only
#Get the lower bound
lower_bound <- median(x_train[,2]) - 3 * mad(x_train[,2])
lower_bound

#Get the upper bound
upper_bound <- median(x_train[,2]) + 3 * mad(x_train[,2])
upper_bound

#Get samples index (raw) of the outlier
outlier_ind <- which(x_train[,2] < lower_bound | x_train[,2] > upper_bound)
outlier_ind

#Get the number of outlier
nbr <- length(outlier_ind)
nbr

######################SELECTION AND REPLACEMENT##################
#############TRAININGSET##########
x_train = read.csv("x_train_mean.csv")
x_train

#Check all coloumn outlier
for(i in 2:ncol(x_train)){
  lower_bound <- median(x_train[,i]) - 3 * mad(x_train[,i])
  upper_bound <- median(x_train[,i]) + 3 * mad(x_train[,i])
  outlier_ind <- which(x_train[,i] < lower_bound | x_train[,i] > upper_bound)
  #replace outlier by zeros
  if (length(outlier_ind) > 0) {
    for (j in 1:length(outlier_ind)){
      x_train[outlier_ind[j], i] <- NA
    }
  }
}

#The data set with NAs instead of outlier
x_train

###########TESTSET##########
x_test = read.csv("x_test_mean.csv")
x_test

#Check all coloumn outlier
for(i in 2:ncol(x_test)){
  lower_bound <- median(x_test[,i]) - 3 * mad(x_test[,i])
  upper_bound <- median(x_test[,i]) + 3 * mad(x_test[,i])
  outlier_ind <- which(x_test[,i] < lower_bound | x_test[,i] > upper_bound)
  #replace outlier by zeros
  if (length(outlier_ind) > 0) {
    for (j in 1:length(outlier_ind)){
      x_test[outlier_ind[j], i] <- NA
    }
  }
}

#The data set with NAs instead of outlier
x_test



######WRITTING PROCESS############
write.csv(x_train, "x_train_mean_hampel.csv", row.names = FALSE)
write.csv(x_test, "x_test_mean_hampel.csv", row.names = FALSE)


########################################### ISOLATION FOREST #######################################################
# IsolationForest Method
install.packages("solitude")
library(solitude)
x_train = read.csv("x_train_mean.csv")

n = 1000
Var1 = c(rnorm(n, 0, 0.5), rnorm(n*0.1, -2, 1))
Var2 = c(rnorm(n, 0, 0.5), rnorm(n*0.1,  2, 1))
outliers = c(rep(0, n), rep(1, (0.1*n))) + 3
data = data.frame(Var1, Var2)
iforest <- solitude::isolationForest$new(sample_size = length(data))
iforest$fit(data)
data 

############################################# DBSCan ########################################################
install.packages("ggplot2")
install.packages("data.table")
install.packages("dbscan")
library(ggplot2)
library(data.table)
library(dbscan)


x_train <- read.csv("x_train_mean.csv")
x_scale <- apply(x_train, 2, function(y) (y - mean(y)) / sd(y) ^ as.logical(sd(y)))
print(sum(is.na(x_train)))


distance_matrix <- as.matrix(dist(x_scale))
pca <- prcomp(distance_matrix)
embedding <- data.table(pca$x[, 1:2])
embedding[, ids := rownames(x_train)]
ggplot(embedding, aes(x = PC1, y = PC2)) +
  geom_point(size = 10, colour = "steelblue", alpha = 0.3) +
  geom_text(aes(label = ids), check_overlap = TRUE) +
  theme_minimal()


embedding[, DClusters := dbscan(x_scale, eps = 0.2, minPts = 2)$cluster]
ggplot(embedding, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = factor(DClusters)), size = 10, alpha = 0.3) +
  geom_text(aes(label = ids), check_overlap = TRUE) +
  theme_minimal()


################################### EXPECTATION MAXIMISATION ###############################

install.packages("ggplot2")
install.packages("data.table")
install.packages("mclust")
library(ggplot2)
library(data.table)
library(mclust)

x_train <- read.csv("x_train_mean.csv")
x_scale <- apply(x_train, 2, function(y) (y - mean(y)) / sd(y) ^ as.logical(sd(y)))
print(sum(is.na(x_train)))


distance_matrix <- as.matrix(dist(x_scale))
pca <- prcomp(distance_matrix)
embedding <- data.table(pca$x[, 1:2])
embedding[, ids := rownames(x_train)]
ggplot(embedding, aes(x = PC1, y = PC2)) +
  geom_point(size = 10, colour = "steelblue", alpha = 0.3) +
  geom_text(aes(label = ids), check_overlap = TRUE) +
  theme_minimal()



cars_em <- Mclust(scale(x_train), G = 4)
embedding[, EMClusters := cars_em$classification]
ggplot(embedding, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = factor(EMClusters)), size = 10, alpha = 0.3) +
  geom_text(aes(label = ids), check_overlap = TRUE) +
  theme_minimal()


################################### PCOutlierDetecton ######################################

install.packages("OutlierDetection")
library(OutlierDetection)

x_train <- read.csv("x_train_mean.csv")

outdetect <- PCOutlierDetection(x_train[,2])


