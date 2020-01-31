#1
data <- read.table("titanic.csv", 
                   header = TRUE, 
                   sep = ",", 
                   stringsAsFactors = F,
                   na.strings="")

#2
mean(data$Survived)*100

#3 
colMeans(is.na(data))*100

#4 
data$Cabin <- NULL

#5
data$Age[is.na(data$Age)] <- median(data$Age, na.rm=T)

#6
aggregate(formula = Survived ~ Sex, data = data,  FUN = mean)
# o
tapply(data$Survived, data$Sex, mean)

#7
aggregate(formula = Survived ~ Age, data = data,  FUN = mean)
# o
tapply(data$Survived, data$Age, mean)

#8
data$Decade <- cut(data$Age, breaks = seq(0,100,10), right=FALSE)
# o
data$Decade <- data$Age %/% 10 * 10

aggregate(formula = Survived ~ Decade, data = data,  FUN = mean)
# o
tapply(data$Survived, data$Decade, mean)

#9
aggregate(formula = Survived ~ Pclass, data = data,  FUN = mean)
# o
tapply(data$Survived, data$Pclass, mean)

#10
aggregate(formula = Survived ~ Pclass + Sex, data = data,  FUN = mean)
# o
tapply(data$Survived, list(data$Pclass, data$Sex), mean)
