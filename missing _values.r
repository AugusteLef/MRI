# Filling the missing values

#With mean
#Training set
x_train = read.csv("X_train.csv") #read csv file
x_train
for(i in 1:ncol(x_train)){
  x_train[is.na(x_train[,i]), i] <- mean(x_train[,i], na.rm = TRUE)
}

x_train

#Testing set
x_test = read.csv("X_test.csv") #read csv file
x_test
for(i in 1:ncol(x_train)){
  x_test[is.na(x_test[,i]), i] <- mean(x_test[,i], na.rm = TRUE)
}

x_test

#write to csv
write.csv(x_train, "x_train_mean.csv", row.names = FALSE)
write.csv(x_test, "x_test_mean.csv", row.names = FALSE)

#########################################################################################


#With median
#Training set
x_train = read.csv("X_train.csv") #read csv file
x_train
for(i in 1:ncol(x_train)){
  x_train[is.na(x_train[,i]), i] <- median(x_train[,i], na.rm = TRUE)
}

x_train

#Testing set
x_test = read.csv("X_test.csv") #read csv file
x_test
for(i in 1:ncol(x_train)){
  x_test[is.na(x_test[,i]), i] <- median(x_test[,i], na.rm = TRUE)
}

x_test

#write to csv
write.csv(x_train, "x_train_median.csv", row.names = FALSE)
write.csv(x_test, "x_test_median.csv", row.names = FALSE)


#######################################################################################


#By frequency

#NOT SUITABLE FOR THESE DATA SETS AS VALUES ARE double with sometimes more than 5 decimals.
#Maybe we could round all data to the closest integer and then choose the most frequent integer


#######################################################################################

#Hot deck imputation method (Nearest neighbor hot deck imputation) ALLOWED ?

update.packages()

install.packages("VIM")
library(VIM)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("impute")

#Using impute package

#KNN-imputation on x_train
x_train = read.csv("X_train.csv") #read the file
x_train_knned <- impute::impute.knn(as.matrix(x_train))
x_train <- x_train_knned[["data"]]
x_train

#KNN-imputation on x_test
x_test = read.csv("X_test.csv") #read the file
x_test_knned <- impute::impute.knn(as.matrix(x_test))
x_test <- x_test_knned[["data"]]
x_test

#write to csv
write.csv(x_train, "x_train_KNN.csv", row.names = FALSE)
write.csv(x_test, "x_test_KNN.csv", row.names = FALSE)