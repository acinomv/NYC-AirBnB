library(tree)
#REGRESSION TREE
RNGkind(sample.kind = "Rounding")
set.seed(123)

#results:
#variables used: room type private room, bathrooms text, neighbourhood group
#cleansed manhattan, bedrooms, room type shared room, accommodates
#8 terminal nodes, 6 variables
tree.listings <- tree(price~., data = train)
summary(tree.listings)
plot(tree.listings)
text(tree.listings, pretty = 0, main = "Decision Tree")

#train mse = 4190.272
tree.train <- predict(tree.listings, newdata = train)
mean((tree.train - y.train)^2)

#test mse = 4075.223
tree.test <- predict(tree.listings, newdata = test)
mean((tree.test - y.test)^2)

#plot
par(mfrow = c(1, 2))
#train
plot(tree.train,
     y.train,
     xlab = "Predicted Price",
     ylab = "Actual Price",
     main = "Decision Tree Train")
abline(0, 1)

#test
plot(tree.test,
     y.test,
     xlab = "Predicted Price",
     ylab = "Actual Price",
     main = "Decision Tree Test",
     col = "blue")
abline(0, 1)


#10 fold cv
#results: 8 is best but try 6
#6 terminal nodes, 5 variables
#variables used: room type private room, bathrooms text, neighbourhood group
#cleansed manhattan, room shared type room, accommodates 
cv.tree.listings <- cv.tree(tree.listings, K = 10)
plot(cv.tree.listings$size, 
     cv.tree.listings$dev, 
     xlab = "Number of Terminal Nodes",
     ylab = "Deviance",
     type = 'b',
     main = "Decision Tree")

prune.listings <- prune.tree(tree.listings.price, best = 6)
plot(prune.listings)
text(prune.listings, pretty = 0)

#train mse = 4332.135
prune.train <- predict(prune.listings, newdata = train)
mean((prune.train - y.train)^2)

#test mse = 4267.416
prune.test <- predict(prune.listings, newdata = test)
mean((prune.test - y.test)^2)

summary(prune.listings)
