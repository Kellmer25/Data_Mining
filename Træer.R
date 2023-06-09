### Dependencies ---------------------------------------------------------------
library(magrittr)
library(dplyr)

### Opg 3 ----------------------------------------------------------------------
df = read.csv("day.csv")

### A
library(rpart)
library(rpart.plot)

tree = rpart(cnt ~ atemp + hum + windspeed + as.factor(season) + as.factor(holiday) + as.factor(weathersit),
             data = df, 
             control = rpart.control(cp = 0.0001))

printcp(tree)

best = tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]

pruned.tree = prune(tree, cp = best)

printcp(pruned.tree)

prp(pruned_tree, faclen = 0, extra = 1, roundint = F, digits = 5)

### B // bagging
library(ipred)
library(caret)

set.seed(123)

bag = bagging(formula = cnt ~ atemp + hum + windspeed + as.factor(season) + as.factor(holiday) + as.factor(weathersit), 
              data = df, 
              nbagg = 150, 
              coob = TRUE, 
              control = rpart.control(minsplit = 2, cp = 0))

bag

#calculate variable importance
varImp(bag) %>% arrange(Overall, asc = FALSE)


### B // random forest
library(randomForest)

forest = randomForest(formula = cnt ~ atemp + hum + windspeed + as.factor(season) + as.factor(holiday) + as.factor(weathersit),
                      data = df)

plot(forest)

varImpPlot(forest) 

### C


### D


### Opg 4 ----------------------------------------------------------------------
set.seed(1)

### A
opg4.df = matrix(nrow = 300, ncol = 7) %>% as.data.frame() %>%
  magrittr::set_colnames(c("y1", "y11", "x1", "x11", "x2", "x22", "err")) %>%
  dplyr::mutate(x1 = rnorm(300),
                x11 = ifelse(x1 > 0, 1, 0),
                x2 = rnorm(300),
                x22 = ifelse(x2 > 0, 1, 0),
                err = rnorm(300) / 2,
                y1 = 1 + 2 * x1 + 3 * x2 + err,
                y11 = 1 + 2 * x11 + 3 * x22 + err)

### B

lin.model = lm(y1 ~ x1 + x2 + err, data = opg4.df)
tree.model = rpart(y1 ~ x1 + x2 + err,
                   data = opg4.df, 
                   control = rpart.control(cp = 0.0001))
summary(lin.model)

printcp(tree.model)

### C
lin.model = lm(y11 ~ x11 + x22 + err, data = opg4.df)

tree.model = rpart(y11 ~ x11 + x22 + err,
                   data = opg4.df, 
                   control = rpart.control(cp = 0.0001))
summary(lin.model)

printcp(tree.model)
