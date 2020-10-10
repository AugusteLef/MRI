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







