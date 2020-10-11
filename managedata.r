x_train <- read.csv("X_train.csv")
y_train <- read.csv("y_train.csv")
outl <- (read.csv("outlier_isolationforest_results.csv"))

# GET INDEX of the OUTLIER
index <- c()
index_raw <- c()
for (i in 1:length(outl[,1])) {
  if (outl[i,1] == -1){
    index <- c(index, i)
    index_raw <- c(index_raw, i+1)
  }
}

index

#WRITE INDEX OUTLIER
write.csv(index, "outlier_index.csv", row.names = FALSE)

#DELETE SAMPLES OUTLIER
x_train <- x_train[-index_raw,, drop = FALSE]
x_train

y_train <- y_train[-index_raw,, drop = FALSE]
y_train

#WRITE
write.csv(x_train, "x_train_wo_outlier.csv", row.names = FALSE)
write.csv(y_train, "y_train_wo_outlier.csv", row.names = FALSE)




#APPLY KNN TO THESE NEW SET TO FILLING IN NAs
#KNN-imputation on x_train
x_train_knned <- impute::impute.knn(as.matrix(x_train))
x_train <- x_train_knned[["data"]]
x_train


#write to csv
write.csv(x_train, "x_train__wo_outlier_KNN.csv", row.names = FALSE)


