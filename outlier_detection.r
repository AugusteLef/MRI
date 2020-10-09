#Outlier Detection


######################################## PERCENTILE ################################################
x_train = read.csv("X_train")
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


######################################### HAMPEL ####################################################
x_train = read.csv("X_train")
x_train

#EXAMPLE for first column

#Get the lowerbound
lower_bound <- median(dat$hwy) - 3 * mad(dat$hwy)
lower_bound

#Get the upperbound
upper_bound <- median(dat$hwy) + 3 * mad(dat$hwy)
upper_bound
