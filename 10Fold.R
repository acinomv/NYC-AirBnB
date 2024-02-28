library(leaps)
RNGkind(sample.kind = "Rounding")
set.seed(123)

predict.regsubsets = function(object, newdata, id,...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  mat[,xvars]%*%coefi
}

k = 10
RNGkind(sample.kind = "Rounding")
set.seed(123)
folds = sample(1:k, nrow(train), replace = T)
cv.errors = matrix(NA, k, 10, dimnames = list(NULL, paste(1:10)))

for(j in 1:k){
  best.fit = regsubsets(price~., data = train[folds != j,], nvmax = 10)
  for(i in 1:10){
    pred = predict(best.fit, train[folds == j,],  id = i)
    cv.errors[j, i] = mean((train$price[folds == j] - pred)^2)
  }
}
mean.cv.errors = apply(cv.errors, 2, mean)
mean.cv.errors

plot(mean.cv.errors, 
     type = "b", 
     main = "10 Fold Cross Validation - Forward",
     xlab = "Number of Variables",
     ylab = "MSE")
