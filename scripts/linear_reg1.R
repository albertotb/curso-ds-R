library(ggplot2)

train <- sample_frac(diamonds, 0.6)
test  <- anti_join(diamonds, train)

modelo <- lm(price ~ ., data = train)

pred_train <- predict(modelo)
pred_test  <- predict(modelo, newdata = test)

y_train <- train$price
y_test  <- test$price

err_train <- mean((y_train - pred_train)^2)
err_test  <- mean((y_test - pred_test)^2)