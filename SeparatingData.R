listings <- read.csv("listingsclean37.csv")

RNGkind(sample.kind = "Rounding")
set.seed(123)

train.values = sample(1:nrow(listings), nrow(listings) * 0.9)

train <- listings[train.values,]
test <- listings[-train.values,]

#matrices for calculations
x.train = model.matrix(price~., train)[,-1]
y.train = train$price

x.test = model.matrix(price~., test)[,-1]
y.test = test$price

write.csv(train.clean, "trainclean.csv", row.names = F)
write.csv(test.clean, "testclean.csv", row.names = F)