library("e1071")
library(GGally)
library(ggplot2)
data("iris")
head(iris,5)
ggpairs(iris, ggplot2::aes(colour = Species, alpha = 0.4))


#SVM model
svm_model <- svm(Species ~ ., data=iris,
                 kernel="linear")

plot(svm_model, data=iris,
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3, Sepal.Length=4) 
)

pred <- predict(svm_model,iris)
tab = table(Predicted=pred, Actual = iris$Species)
tab

1-sum(diag(tab)/sum(tab))


svm_model <- svm(Species ~ ., data=iris,
                 kernel="radial")

plot(svm_model, data=iris,
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3, Sepal.Length=4) 
)

pred <- predict(svm_model,iris)
tab = table(Predicted=pred, Actual = iris$Species)
tab

1-sum(diag(tab)/sum(tab))

#Tuning parameter epsilon
tmodel=tune(svm,Species~., data=iris,
            ranges=list(epsilon= seq(0,1,0.1), cost = 2^(2:7)))
plot(tmodel)

summary(tmodel)

best_model <- tmodel$best.model

plot(best_model, data=iris,
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3, Sepal.Length=4)  
)

pred_best_model = predict(best_model,iris)
tab1 = table(Predicted=pred_best_model, Actual = iris$Species)
tab1

1-sum(diag(tab1)/sum(tab1))
