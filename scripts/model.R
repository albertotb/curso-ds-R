model <- glm(formula, data=train, family="binomial")
ypred <- as.factor(predict(model, newdata=test, type="response") > 0.015)
ytrue <- test$Diabetes1

print(confusionMatrix(ypred, ytrue, positive="TRUE"))

model1 <- randomForest(formula, 
                       data=train, 
                       strata=train$Diabetes1, 
                       sampsize=c(500, 500),
                       #mtry=3,
                       ntree=1500,
                       nodesize=1,
                       maxnodes=NULL)
ytrue <- test$Diabetes1
ypred <- predict(model1, newdata=test)

print(confusionMatrix(ypred, ytrue, positive="TRUE"))