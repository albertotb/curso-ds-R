library(xgboost)

#Computes RMSE between test instances and predictions.
compute_RMSE <- function(y_test, y_pred){
	sqrt(sum((y_test-y_pred)^2)/length(y_test))
}

#Reads dataset and returns it as a data frame.
read_dataset <- function(dataset_name){
	data.frame(read.table(dataset_name, header=TRUE, sep=","))
}

#Splits a dataset into train and test partitions.
#Percentage in [0, 1].
#Return [[1]]: Train. [[2]]: Test.
split_train_test <- function(dataset, percentage_train, seed){
	if(missing(seed)){
		seed <- 1
	}
	if(missing(percentage_train)){
		percentage_train <- 0.8
	}
	set.seed(seed)
	training_size <- floor(percentage_train * nrow(dataset))
	train_ind <- sample(seq_len(nrow(dataset)), size = training_size)
	train <- dataset[train_ind, ]
	test <- dataset[-train_ind, ]
	return(list(train, test))
}

#Returns the Y vector from D.
get_labels_from_dataset <- function(dataset) {
	dataset[,length(dataset)]
}

#Returns the X matrix from D.
get_covariates_from_dataset <- function(dataset) {
	dataset[,-length(dataset)]
}

generate_default_results_vida <- function() {
	#Read data and split into train and test.
	dataset <- read_dataset("./dataset_vida.txt")
	train_test <- split_train_test(dataset)

	#Train default xgboost.
	X_train <- as.matrix(get_covariates_from_dataset(train_test[[1]]))
	y_train <- get_labels_from_dataset(train_test[[1]])
	X_test <- as.matrix(get_covariates_from_dataset(train_test[[2]]))
	y_test <- get_labels_from_dataset(train_test[[2]])
	model_xgb <- xgboost(data = X_train, label = y_train, nrounds = 50)

	#Predict labels from test set.
	test_labels <- get_labels_from_dataset(train_test[[2]])
	predictions_xgb <- predict(model_xgb, as.matrix(get_covariates_from_dataset(train_test[[2]])))

	#Compute MSE and print it into screen.
	print(paste("Root Mean squared error default Extreme Gradient Boosting = ", compute_RMSE(test_labels, predictions_xgb)))
}

generate_default_results_coche <- function() {
        #Read data and split into train and test.
        dataset <- read_dataset("./dataset_coche.txt")
        train_test <- split_train_test(dataset)

        #Train default xgboost.
        X_train <- as.matrix(get_covariates_from_dataset(train_test[[1]]))
        y_train <- get_labels_from_dataset(train_test[[1]])
        X_test <- as.matrix(get_covariates_from_dataset(train_test[[2]]))
        y_test <- get_labels_from_dataset(train_test[[2]])
        model_xgb <- xgboost(data = X_train, label = y_train, nrounds = 50)

        #Predict labels from test set.
        test_labels <- get_labels_from_dataset(train_test[[2]])
        predictions_xgb <- predict(model_xgb, as.matrix(get_covariates_from_dataset(train_test[[2]])))

        #Compute MSE and print it into screen.
        print(paste("Root Mean squared error default Extreme Gradient Boosting = ", compute_RMSE(test_labels, predictions_xgb)))
}

generate_default_results_hogar <- function() {
        #Read data and split into train and test.
        dataset <- read_dataset("./dataset_hogar.txt")
        train_test <- split_train_test(dataset)

        #Train default xgboost.
        X_train <- as.matrix(get_covariates_from_dataset(train_test[[1]]))
        y_train <- get_labels_from_dataset(train_test[[1]])
        X_test <- as.matrix(get_covariates_from_dataset(train_test[[2]]))
        y_test <- get_labels_from_dataset(train_test[[2]])
        model_xgb <- xgboost(data = X_train, label = y_train, nrounds = 50, params=list(eta=0.5, max_depth=8, booster="gblinear"))

        #Predict labels from test set.
        test_labels <- get_labels_from_dataset(train_test[[2]])
        predictions_xgb <- predict(model_xgb, as.matrix(get_covariates_from_dataset(train_test[[2]])))

        #Compute MSE and print it into screen.
        print(paste("Root Mean squared error default Extreme Gradient Boosting = ", compute_RMSE(test_labels, predictions_xgb)))
}

generate_default_results_hogar()
#generate_default_results_coche()
#generate_default_results_vida()
