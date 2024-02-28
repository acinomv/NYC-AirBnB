library(glmnet)

set.seed(123)
grid = 10^seq(10, -2, length = 100)

lasso.listings = glmnet(x.train, y.train, alpha = 1, lamda = grid)
#plot(lasso.listings, main = "Lasso")

#best lam = 0.5
cv.lasso <- cv.glmnet(x.train, y.train, alpha = 1, lambda = grid)
best.lam.lasso <- cv.lasso$lambda.min
best.lam.lasso
plot(cv.lasso)

set.seed(123)

#train
#mse = 3991.401
lasso.listings.train <- predict(lasso.listings, s = 0.5, newx = x.train)
mean((lasso.listings.train - y.train)^2)

#test
#mse = 3867.295
lasso.listings.test <- predict(lasso.listings, s = 0.5, newx = x.test)
mean((lasso.listings.test - y.test)^2)

par(mfrow = c(1, 2))

plot(lasso.listings.train, 
     y.train, 
     main = "Lasso Regression Train",
     xlab = "Predicted Price",
     ylab = "Actual Price")
abline(0, 1)

plot(lasso.listings.test, 
     y.test, 
     main = "Lasso Regression Test",
     xlab = "Predicted Price",
     ylab = "Actual Price",
     col = "blue")
abline(0, 1)

par(mfrow = c(1, 1))

#plot coef
res.lasso <- glmnet(x.train, y.train, alpha = 1, lambda = grid, standardize = F)
plot(res.lasso, xvar = "lambda")
legend("bottomright", 
       lwd = 1, 
       col = 1:6, 
       legend = colnames(x.train), 
       cex = 0.7)
