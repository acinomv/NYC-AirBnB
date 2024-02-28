#look for outliers in accommodates, bathrooms, bedrooms, beds, price

#accommodates
#results: min outlier: 8, max outlier: 16
hist(listings$accommodates, xlab = "accommodates", 
     main = "Histogram of accomodates")
boxplot(listings$accommodates, main = "Boxplot of accommodates")
summary(listings$accommodates)
boxplot.stats(listings$accommodates)$out
min(boxplot.stats(listings$accommodates)$out)
max(boxplot.stats(listings$accommodates)$out)

boxplot(price~accommodates, data = listings)

boxplot(price~accommodates, xlab = "accommodates",
        main = "Boxplot of Price by Accommodates")
boxplot(log_price~accommodates, xlab = "accommodates",
        main = "Boxplot of log(price) by Accommodates")

#bathrooms
#results: basically anything but 1 is an outlier, probably only get rid of 3-8
hist(listings$bathrooms_text, xlab = "bathrooms", 
     main = "Histogram of bathrooms")
boxplot(listings$bathrooms_text, main = "Boxplot of bathrooms")
summary(listings$bathrooms_text)
boxplot.stats(listings$bathrooms_text)$out
(listings$bathrooms_text)
mean(listings$bathrooms_text)
max(boxplot.stats(listings$bathrooms_text)$out)
table(listings$bathrooms_text)

attach(listings)
plot(price, bathrooms_text, xlab = "price", ylab = "bathrooms", 
     main = "Bathrooms vs Price Scatter")

#checking to see if 8 bathrooms make sense with the amount it accommodates and
#number of bedrooms
#results: only hostels returned 8 bathrooms which makes sense
listings[listings$bathrooms_text == 8,]

ggplot(listings, aes(price, bathrooms_text)) + geom_point()

boxplot(price~bathrooms_text, xlab = "bathrooms",
        main = "Boxplot of Price by Bathrooms")
boxplot(log_price~bathrooms_text, xlab = "bathrooms",
        main = "Boxplot of log(price) by Bathrooms")

#bedrooms
hist(listings$bedrooms, xlab = "bedrooms", main = "Histogram of bedrooms")
boxplot(listings$bedrooms, main = "Boxplot of bedrooms")
summary(listings$bedrooms)
boxplot.stats(listings$bedrooms)$out

table(listings$bedrooms)

attach(listings)
plot(price, bedrooms, xlab = "price", ylab = "bedrooms", main = "Bedrooms vs Price Scatter")

max(listings$bedrooms)

ggplot(listings, aes(price, bedrooms)) + geom_point()

#looking to see if 21 bedrooms makes sense
#result: only one occurrence  of 21 bedrooms, property accommodates only 2
#occupants and is an apartment with 2 bathrooms, so 21 bedrooms does not make 
#sense - get rid of this point
listings[listings$bedrooms == 21,]
listings <- listings[!(listings$bedrooms == 21),]

table(listings$bedrooms)

boxplot(price~bedrooms, xlab = "bedrooms",
        main = "Boxplot of Price by Bedrooms")
boxplot(log_price~bedrooms, xlab = "bedrooms",
        main = "Boxplot of log(price) by Bedrooms")

#beds
boxplot(price~beds, xlab = "beds",
        main = "Boxplot of Price by Beds")
boxplot(log_price~beds, xlab = "beds",
        main = "Boxplot of log(price) by Beds")


#looking at borough
table(listings$neighbourhood_group_cleansed)

ggplot(listings, aes(price, neighbourhood_group_cleansed)) + geom_point()

boxplot(price~neighbourhood_group_cleansed, xlab = "borough",
        main = "Boxplot of Price by Borough")
boxplot(log_price~neighbourhood_group_cleansed, xlab = "borough",
        main = "Boxplot of log(price) by Borough")

#boxplot for price
#remove rows where price = 0
#price outliers between 286 and 999; 2466 points in this range; 6% of total data
boxplot(listings$price, main = "Boxplot of Price")
summary(listings$price)
sum(listings$price == 0)
listings[listings$price == 0,]
listings <- listings[!(listings$price == 0),]
min(boxplot.stats(listings$price)$out)
max(boxplot.stats(listings$price)$out)
sum(listings$price >= 286)
2466 / nrow(listings)

hist(listings$price)

#result: 548 listings above  500
sum(listings$price > 500)

listings <- listings[!(listings$price > 500),]
hist(listings$price, main = "Histogram of Price", xlab = "price")

#listings$log_price<- log10(listings$price)

#hist(listings$log_price, main =  "Histogram of Log Price", xlab = "log(price)")

#room type
boxplot(price~room_type, xlab = "room_type",
        main = "Boxplot of Price by Room Type")
boxplot(log_price~room_type, xlab = "room_type",
        main = "Boxplot of log(price) by Room Type")

#look at quantiles
library(quantile)
quantile(listings$price)

#USE THIS TO VISUALIZE PRICE

#quantiles
#0%  25%  50%  75% 100% 
#10   60   99  150  999
quantile(listings$price)
#result 8987 25% of data
sum(listings$price >150)

hist(listings$price, main = "Histogram of Price", xlab = "Price")
boxplot(listings$price, main = "Boxplot of Price")
boxplot.stats(listings$price)$out
#about 1% of the data
sum(listings$price > 500)
listings <- listings[!(listings$price > 500),]


