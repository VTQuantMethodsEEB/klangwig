library(MASS)
library(dplyr)
library(tidyr)


#make sure you have this one:
library(boot)
library(caret)

set.seed(123)
liz = read.csv("lizards.csv")

##simple bootstrap model validation

# create a function suitable for boot that will return the goodness of fit
# statistics testing models against the full original sample.

compare <- function(orig_data, i){
  # create the resampled data
  train_data <- orig_data[i, ]
  test_data <- orig_data # ie the full original sample
  # fit the model
  model_full <- glm.nb(N ~time*light, data = train_data)
  # predict the values on the original, unresampled data
  predict_full  <- predict(model_full, newdata = test_data)
  # # return a vector of 6 summary results
  results <- c(
    full_RMSE  = RMSE(predict_full, test_data$N)
  )
  return(results)
  }

#perform bootstrap
repeats <- 100
res <- boot(liz, statistic = compare, R = repeats)


# load the library
library(caret)
# load the iris dataset
data(iris)
# define training control
train_control <- trainControl(method="boot", number=100)
# train the model
model <- train(Species~., data=iris, trControl=train_control, method="nb")
# summarize results
print(model)
  

library(caret)
library(klaR)
# load the iris dataset
data(iris)
# define an 80%/20% train/test split of the dataset
split=0.80
trainIndex <- createDataPartition(iris$Petal.Width, p=split, list=FALSE)
data_train <- iris[ trainIndex,]
data_test <- iris[-trainIndex,]
# train a naive bayes model
model <- lm(Petal.Width~Sepal.Length*Species, data=data_train)
# make predictions
x_test <- data.frame(Sepal.Length = data_test$Sepal.Length,Species = data_test$Species)
y_test <- data_test$Petal.Width
predictions <- predict(model, x_test)
# summarize results
confusionMatrix(predictions, y_test)
