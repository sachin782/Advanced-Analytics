library('e1071')
library('caret')
set.seed(101)
data <- as.data.frame(Titanic)
View(data)
n <- nrow(data)
trainIndex <- sample(1:n, size = round(0.7*n), replace=FALSE)
train <- data[trainIndex ,]
test <- data[-trainIndex ,]
model <- naiveBayes(Survived~Class+Sex+Age+Freq, data=train)
print(model)
prediction <- predict(model,test)
print(prediction)
print(confusionMatrix(prediction,test$Survived))