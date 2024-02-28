#BAGGING AND RANDOM FOREST
library(caret)
library(doParallel)
library(randomForest)
RNGkind(sample.kind = "Rounding")
set.seed(123)

#bagging (mtry = p)
#mtry = 17
cl <- makePSOCKcluster(10)
registerDoParallel(cl, cores = 5)
start.time <- proc.time()

bag.listings = randomForest(price~., data = train, mtry = 17, importance = T)

stop.time <- proc.time()
run.time <- stop.time - start.time
run.time
stopCluster(cl)

bag.listings

#bag train mse = 3696.236
#bag test mse = 3709.098
bag.listings.test = predict(bag.listings, newdata = test)
mean((bag.listings.test - y.test)^2)

par(mfrow = c(1, 2))

plot(bag.listings$predicted, 
     y.train,
     xlab = "Predicted Price",
     ylab = "Actual Price",
     main = "Random Forests with Bagging Train")
abline(0, 1)

plot(bag.listings.test, 
     y.test, 
     xlab = "Predicted Price",
     ylab = "Actual Price", 
     main = "Random Forests with Bagging Test",
     col = "blue")
abline(0, 1)

plot(bag.listings, main = "Random Forests with Bagging")

importance(bag.listings)
varImpPlot(bag.listings, main = "Random Forests with Bagging")

#predict with mtry = 6 - MSE = 3372.407
#yhat.rf = predict(rf.listings, newdata = test)
#plot(yhat.rf, y.test, main = "Price Prediction with Random Forests")
#abline(0, 1)
#mean((yhat.rf - y.test)^2)

#RANDOM FORESTS NO BAG
#takes less time
#try mtry = 5; p = 17, p/3 = 5.6667 
#train
library(caret)
library(doParallel)
library(randomForest)
RNGkind(sample.kind = "Rounding")
set.seed(123)

cl <- makePSOCKcluster(10)
registerDoParallel(cl, cores = 5)
start.time <- proc.time()

rf.listings = randomForest(price~., data = train, mtry = 5, importance = T)

stop.time <- proc.time()
run.time <- stop.time - start.time
run.time
stopCluster(cl)

rf.listings

#rf train mse = 3443.682
#rf test mse = 3410.058
rf.listings.test <- predict(rf.listings, newdata = test)
mean((rf.listings.test - y.test)^2)

par(mfrow = c(1, 2))

plot(rf.listings$predicted, 
     y.train,
     xlab = "Predicted Price",
     ylab = "Actual Price",
     main = "Random Forests Train")
abline(0, 1)

plot(rf.listings.test, 
     y.test,
     xlab = "Predicted Price",
     ylab = "Actual Price",
     main = "Random Forests Test",
     col = "blue")
abline(0, 1)


importance(bag.listings)
importance(rf.listings)

plot(rf.listings, main = "Random Forests")

plot(bag.listings, 
     y.train, 
     main = "Random Forests with Bagging Train")
abline(0, 1)

plot(bag.listings.test, 
     y.test, 
     xlab = "Predicted Price",
     ylab = "Actual Price", 
     main = "Random Forests with Bagging Test")
abline(0, 1)

varImpPlot(rf.listings, main = "Random Forests")

