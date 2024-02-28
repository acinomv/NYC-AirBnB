library(glmnet)
#training
RNGkind(sample.kind = "Rounding")
set.seed(123)
grid = 10^seq(10, -2, length = 100)
ridge.listings = glmnet(x.train, y.train, alpha = 0, lambda = grid)

#bestlam = 75 which is 68th value in matrix
#10 fold cv
cv.ridge <- cv.glmnet(x.train, y.train, alpha = 0, lambda = grid)
best.lam.ridge <- cv.ridge$lambda.min
best.lam.ridge

plot(cv.ridge)

coef(ridge.listings)[,68]

#train
#bestlam = 75
#mse = 4326.439
ridge.listings.train <- predict(ridge.listings, s = 75, newx = x.train)
mean((ridge.listings.train - y.train)^2)


#test
#mse = 4246.66
ridge.listings.test <- predict(ridge.listings, s = 75, newx = x.test)
mean((ridge.listings.test - y.test)^2)

#plot
par(mfrow = c(1, 2))

plot(ridge.listings.train, 
     y.train, 
     main = "Ridge Regression Test",
     xlab = "Predicted Price",
     ylab = "Actual Price")
abline(0, 1)

plot(ridge.listings.test, 
     y.test,
     main = "Ridge Regression Train",
     xlab = "Predicted Price",
     ylab = "Actual Price",
     col = "blue")
abline(0, 1)

par(mfrow = c(1, 1))
res.ridge <- glmnet(x.train, y.train, alpha = 0, lambda = grid, standardize = F)
plot(res.ridge, xvar = "lambda")
legend("bottomright", 
       lwd = 1, 
       col = 1:6, 
       legend = colnames(x.train), 
       cex = 0.7)

