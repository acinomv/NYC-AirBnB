listings <- read.csv("listings.csv")
keep <- c(18, 29, 33, 34, 36:38, 40:42, 56, 61, 69)
listings <- listings[keep]
write.csv(listings, "listingsclean36.csv", row.names = F)

#fill blank with na
listings <- read.csv("listingsclean36.csv", header = T, 
                     na.strings = c("", " ", "N/A", "NA"))

#change na to 0
listings$bathrooms_text[is.na(listings$bathrooms_text)] <- 0
listings$bedrooms[is.na(listings$bedrooms)] <- 0
listings$beds[is.na(listings$beds)] <- 0

#removing $ in price
listings$price <- as.numeric(gsub("\\$", "", listings$price))

#remove rows with na values in price column, where price is 0, and above 500
listings <- listings[!is.na(listings$price),]
listings <- listings[!(listings$price == 0),]


#bathrooms
table(listings$bathrooms_text)
listings$bathrooms_text <- gsub(" shared baths", "", listings$bathrooms_text)
listings$bathrooms_text <- gsub(" private baths", "", listings$bathrooms_text)
listings$bathrooms_text <- gsub(" baths", "", listings$bathrooms_text)
listings$bathrooms_text <- gsub(" bath", "", listings$bathrooms_text)
listings$bathrooms_text <- gsub(" Half-bath", "", listings$bathrooms_text)
listings$bathrooms_text <- gsub(" Private half-bath", "", listings$bathrooms_text)
listings$bathrooms_text <- gsub(" private", "", listings$bathrooms_text)
listings$bathrooms_text <- gsub(" shared", "", listings$bathrooms_text)
listings$bathrooms_text <- gsub("Half-bath", "", listings$bathrooms_text)
listings$bathrooms_text <- gsub("Private half-bath", "", listings$bathrooms_text)
listings$bathrooms_text <- gsub("Shared half-bath", "", listings$bathrooms_text)
listings$bathrooms_text <- as.numeric(as.character(listings$bathrooms_text))
listings <- listings[!is.na(listings$bathrooms_text),]

#change false (f) to 0 and true (t) to 1
listings$host_is_superhost <- gsub("f", 0, listings$host_is_superhost)
listings$host_is_superhost <- gsub("t", 1, listings$host_is_superhost)
listings$host_is_superhost <- as.numeric(as.character(listings$host_is_superhost))

listings$instant_bookable <- gsub("f", 0, listings$instant_bookable)
listings$instant_bookable <- gsub("t", 1, listings$instant_bookable)
listings$instant_bookable <- as.numeric(as.character(listings$instant_bookable))

#gives review score of 0 if no reviews
listings$review_scores_rating[is.na(listings$review_scores_rating)] <- 0

#remove other 466 na (in superhost and beds columns)
sum(is.na(listings))
listings <- na.omit(listings)

write.csv(listings, "listingscleanfinalfinal.csv", row.names = F)


#make dummy var
library(fastDummies)
listings.new <- dummy_cols(listings, 
                           select_columns = c("neighbourhood_group_cleansed",
                                              "room_type"),
                           remove_selected_columns = T, remove_first_dummy = T)
#correlation
library(corrplot)
cor.listings.new <- cor(listings.new)
corrplot(cor.listings.new, method = "color",  sig.level = 0.1, insig = "blank")

pairs(listings.new)
plot(listings.new)

#drop <- c(1, 7, 8, 14)
#listings.clean <- listings.new[-drop]

write.csv(train.clean, "trainclean.csv", row.names = F)
write.csv(test.clean, "testclean.csv", row.names = F)

#cor.clean <- cor(train.clean)
