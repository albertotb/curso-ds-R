library(rpart)
library(rBayesianOptimization)

#Computes RMSE between test instances and predictions.
compute_RMSE <- function(y_test, y_pred){
	sqrt(sum((y_test-y_pred)^2)/length(y_test))
}

#Reads dataset and returns it as a data frame.
read_dataset <- function(dataset_name){
	data.frame(read.table(dataset_name, header=TRUE, sep=","))
}

k_fold_cross_validation_of_model <- function(max_depth, min_split, cp){
	k <- 10
	dataset <- read_dataset("./dataset_dummy.txt")
	print("K fold enter")
	n_train <- nrow(dataset)
	folds_i <- sample(rep(1 : k, length.out = n_train))
	errors <- rep(0, k)
	for (i in 1 : k) {
		test_i <- which(folds_i == i)
		train_xy <- dataset[- test_i,]
		test_xy <- dataset[test_i,]
		model_rf <- rpart(Y ~., data = train_xy, 
				control=rpart.control(maxdepth=as.integer(max_depth), minsplit=as.integer(min_split), xval=3, cp=cp))
		predictions_rf <- predict(model_rf, test_xy)
		test_labels <- get_labels_from_dataset(test_xy)
		errors[i] <- compute_RMSE(test_labels, predictions_rf)		
		print(errors[i])
	}
	list(Score = - sum(errors) / length(errors), Pred=0)
}

#Returns the Y vector from D.
get_labels_from_dataset <- function(dataset) {
	dataset[,length(dataset)]
}

#Returns the X matrix from D.
get_covariates_from_dataset <- function(dataset) {
	dataset[,-length(dataset)]
}

optimize_vida <- function() {
	#Read data and split into train and test.
	BayesianOptimization(k_fold_cross_validation_of_model, bounds= list(max_depth = c(1, 30), min_split=c(1, 1000), cp=c(0.0001,1)),
			n_iter=20, init_grid_dt = data.frame(max_depth=c(2,5), min_split=c(20,40), cp=c(0.01,0.2)), 
			init_points = 0, acq="ucb", verbose=TRUE)
}

optimize_vida()
