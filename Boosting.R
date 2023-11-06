# XGBoost on Occupancy rate, price

## Data Loading

library(tidyverse)
#install.packages("readxl")
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(gbm)
# XGBOOST
library(xgboost)
library(randomForest)


property <- read_excel("property_data.xlsx")

count(property)

property <- subset(property, `OCCUPANCY RATE` <= 1)
property <- subset(property, `OCCUPANCY RATE` >= 0)

head(property)
summary(property)
count(property)


property <- property[,-c(1,2,3,4,15,16,17,18,20,21,24,25,26)]

property[,c(1,2,5,6,7)] <- lapply(property[,c(1,2,5,6,7)] , factor)

property <- na.omit(property)

# Separate train and test sets
set.seed(1)
train = sample(nrow(property), 0.7*nrow(property))
property.train <- property[train,]
property.test <- property[-train,]



------------------------------------------------------------------------------------------
# Price model
------------------------------------------------------------------------------------------
# Boosting for Price using regression trees
# Most important variables for predicting price are service fee, neighborhood and occupancy rate in that order
set.seed(1)
boost.price=gbm(PRICE~.,data=property.train,distribution="gaussian",n.trees=5000,
              interaction.depth=4)
summary(boost.price)
yhat.boost2=predict(boost.price,property.test,n.trees=5000)
mean((yhat.boost-property.test$PRICE)^2)

# Create a data frame with the predictions and the neighborhood variable
prediction_df2 <- data.frame(yhat.boost2, NEIGHBORHOOD = property.test$NEIGHBORHOOD)

# Calculate the average prediction for each neighborhood
neighborhood_averages2 <- aggregate(yhat.boost2 ~ NEIGHBORHOOD, data = prediction_df2, FUN = mean)


# Sort neighborhood_averages from highest to lowest
neighborhood_averages2 <- neighborhood_averages2[order(neighborhood_averages2$yhat.boost2, decreasing = TRUE), ]

neighborhood_averages2

# Create a subset of the top 20 neighborhoods
top_neighborhoods2 <- head(neighborhood_averages2, 20)

# Create a sorted bar chart for the top 20 neighborhoods
plot_3 <- ggplot(top_neighborhoods2, aes(x = reorder(NEIGHBORHOOD, yhat.boost2), y = yhat.boost2)) +
  geom_bar(stat = "identity",  fill = "red") +
  coord_flip() +
  theme_classic() +
  labs(title = "Top 20 Neighborhoods by predicted price", x = "Neighborhood", y = "Price")
plot_3


------------------------------------------------------------------------------------------
  # Main Model
------------------------------------------------------------------------------------------

# Boosting for Occupancy Rate vs all variables
# Most important variables for predicting occupancy rate are neighborhood, reviews_per_month and time since listed in that order
set.seed(1)
boost.occupancy=gbm(`OCCUPANCY RATE`~.,data=property.train,distribution="gaussian",n.trees=5000,
              interaction.depth=4)
summary(boost.occupancy)
yhat.boost=predict(boost.occupancy,property.test,n.trees=5000)
#property.test$`OCCUPANCY RATE` <- as.numeric(property.test$`OCCUPANCY RATE`)
mean((yhat.boost-property.test$`OCCUPANCY RATE`)^2)

# Create a data frame with the predictions and the neighborhood variable
prediction_df <- data.frame(yhat.boost, NEIGHBORHOOD = property.test$NEIGHBORHOOD)

# Calculate the average prediction for each neighborhood
neighborhood_averages <- aggregate(yhat.boost ~ NEIGHBORHOOD, data = prediction_df, FUN = mean)


# Sort neighborhood_averages from highest to lowest
neighborhood_averages <- neighborhood_averages[order(neighborhood_averages$yhat.boost, decreasing = TRUE), ]

neighborhood_averages

# Create a subset of the top 20 neighborhoods
top_neighborhoods <- head(neighborhood_averages, 20)

# Create a sorted bar chart for the top 20 neighborhoods
plot_1 <- ggplot(top_neighborhoods, aes(x = reorder(NEIGHBORHOOD, yhat.boost), y = yhat.boost)) +
  geom_bar(stat = "identity",  fill = "pink") +
  coord_flip() +
  theme_classic() +
  labs(title = "Top 20 Neighborhoods by predicted occupancy rate", x = "Neighborhood", y = "Occupancy Rate")
plot_1
------------------------------------------------------------------------------------------
# Revenue Prediction

# Calculate the average prediction for each neighborhood
neighborhood_prices <- aggregate(PRICE ~ NEIGHBORHOOD, data = property.test, FUN = mean)

merged_data <- merge(neighborhood_averages, neighborhood_prices, by = "NEIGHBORHOOD")

# Create a new column with the product of predictions and prices
merged_data$product <- merged_data$yhat.boost * merged_data$PRICE

# Sort from highest to lowest
merged_data <- merged_data[order(merged_data$product, decreasing = TRUE), ]

write_xlsx(merged_data, "/Users/shivi/Desktop/Career/Case\ Studies/Datathon\ -\ NYC\ properties\\NeighborhoodsRevenue.xlsx")

top_neighborhoods_revenue <- head(merged_data, 20)

# Create a sorted bar chart for the top 20 neighborhoods
plot_4 <- ggplot(top_neighborhoods_revenue, aes(x = reorder(NEIGHBORHOOD, product), y = product)) +
  geom_bar(stat = "identity",  fill = "blue") +
  coord_flip() +
  theme_classic() +
  labs(title = "Top 20 Neighborhoods by predicted revenue", x = "Neighborhood", y = "Revenue")
plot_4









------------------------------------------------------------------------------
------------------------------------------------------------------------------
                 # Same Results as previous
------------------------------------------------------------------------------
------------------------------------------------------------------------------

# Boosting for Occupancy Rate vs Neighborhood
set.seed(1)
boost.occupancy=gbm(`OCCUPANCY RATE`~.,data=property.train,distribution="gaussian",n.trees=5000,
                    interaction.depth=4)
summary(boost.occupancy)
yhat.boost=predict(boost.occupancy,property.test,n.trees=5000)
#property.test$`OCCUPANCY RATE` <- as.numeric(property.test$`OCCUPANCY RATE`)
mean((yhat.boost-property.test$`OCCUPANCY RATE`)^2)

# Create a data frame with the predictions and the neighborhood variable
prediction_df <- data.frame(yhat.boost, NEIGHBORHOOD = property.test$NEIGHBORHOOD)

# Calculate the average prediction for each neighborhood
neighborhood_averages <- aggregate(predictions ~ NEIGHBORHOOD, data = prediction_df, FUN = mean)

# Sort neighborhood_averages from highest to lowest
neighborhood_averages <- neighborhood_averages[order(neighborhood_averages$predictions, decreasing = TRUE), ]

neighborhood_averages

# Create a subset of the top 20 neighborhoods
top_neighborhoods <- head(neighborhood_averages, 20)

# Create a sorted bar chart for the top 20 neighborhoods
plot_2 <- ggplot(top_neighborhoods, aes(x = reorder(NEIGHBORHOOD, predictions), y = predictions)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  theme_classic() +
  labs(title = "Top 20 Neighborhoods by predicted occupancy rate", x = "Neighborhood", y = "Occupancy Rate")

