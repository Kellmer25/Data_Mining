library(neuralnet)
day_data <- read.csv("day.csv")

data <- day_data[c("cnt","atemp","hum","windspeed")]
set.seed(1)

apply(data,2,function(x) sum(is.na(x)))
#Split data-----------------------------------------------
index <- sample(1:nrow(data), round(0.85*nrow(data)))
Train_data1 <- data[index,]
  
Test_data1 <- data[-index,]

#Normalize data----------------------------------------------
maxs <- apply(data,2,max)
mins <- apply(data,2,min)

scaled <- as.data.frame(scale(data,center = mins, scale = maxs-mins))

View(scaled)
Train_data <- scaled[index,]

Test_data <- scaled[-index,]

#Setup------------------------------------------------------------------
n <- names(Train_data)
f <- as.formula(paste("cnt ~", paste(n[!n %in% "cnt"], collapse = " + ")))

nn <- neuralnet(f,data=Train_data,hidden=c(7,5),linear.output = TRUE)


plot(nn)
#prediction---------------------------------------------------------------------

pr.nn <- compute(nn,Test_data[,2:4])

pr.nn_ <- pr.nn$net.result*(max(day_data$cnt)-min(day_data$cnt))+min(day_data$cnt)

test_rescaled <- Test_data$cnt*(max(day_data$cnt)-min(day_data$cnt))+min(day_data$cnt)


pr.nn$net.result

MSE.nn <- sum((test_rescaled-pr.nn_)^2)/nrow(Test_data)

MSE.nn



#linear-model
lm.fit <- glm(cnt~., data=Train_data1)
summary(lm.fit)
pr.lm <- predict(lm.fit,Test_data1)
MSE.lm <- sum((pr.lm - Test_data$cnt)^2)/nrow(Test_data)


#Predicted vs real, sammenligning mellem de to modeller
plot(test_rescaled,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)

points(test_rescaled,pr.lm,col='blue',pch=1)




#Neural networks for classification---------------------------------------------------
library(caret)

iris_data <- iris
index_iris <- sample(1:nrow(iris_data), round(0.85*nrow(iris_data)))
Train_data <- iris_data[index_iris,]

Test_data <- iris_data [-index_iris,]

n <- names(Train_data)
f <- as.formula(paste("Species ~", paste(n[!n %in% "Species"], collapse = " + ")))

nn <- neuralnet(f,data=Train_data,hidden=c(5,5),linear.output = FALSE)

plot(nn)

ypred <- neuralnet::compute(nn,Test_data[1:4])

yhat <- ypred$net.result

yhat=data.frame("yhat"=ifelse(max.col(yhat[ ,1:3])==1, "setosa",
                              ifelse(max.col(yhat[ ,1:3])==2, "versicolor", "virginica")))

cm = confusionMatrix(as.factor(Test_data[,5]), as.factor(yhat$yhat))

print(cm)
