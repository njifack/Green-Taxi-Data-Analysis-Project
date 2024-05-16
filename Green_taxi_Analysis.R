
library(readr)
# Load data
green_taxi_data <- read_csv("2018_Green_Taxi_Trip_Data-1 .csv")
str(green_taxi_data)


## Load libraries
library(tidyr)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

# Load data
green_taxi_data <- read_csv("/Users/nishabled/Desktop/Ms NEU/DA5020/2018_Green_Taxi_Trip_Data-1 .csv")

# Basic data exploration

dim(green_taxi_data)  # 1,048,575 rows and 19 columns
glimpse(green_taxi_data) # You can see the different variable types
summary(green_taxi_data) # Check summary stats
head(green_taxi_data) # first 6
tail(green_taxi_data) # last 6

# Tip amount histogram
ggplot(green_taxi_data, aes(x = tip_amount)) +
 geom_histogram(binwidth = 1, fill = "blue", color = "black") +
 theme_minimal() +
 labs(title = "Tip Amount Histogram", x = "Tip Amount", y = "Frequency")

# Histogram shows a highly skewed distribution with the majority of tips being close to zero and very few larger tips
# This skewness indicates that no or low tips are much more common than higher tips.
# We think its the nature of tips as most people tip low and very few high

# Tip amount boxplot (We are using this to check outliers)
ggplot(green_taxi_data, aes(y = tip_amount)) +
  geom_boxplot(fill = "orange", color = "black") +
  theme_minimal() + 
  labs(title = "Tip Amount Boxplot", y = "Tip Amount")

#The boxplot shows the presence of outliers in the tip amount data.
#The median tip amount appears to be near zero and there are high-value outliers.
#This suggests that most rides do not result in a high tip, but a few rides do result in significantly high tips.
#This could be due to particularly long rides aka airport trip?

# Convert to datetime objects
green_taxi_data <- green_taxi_data %>%
  mutate(
    lpep_pickup_datetime = mdy_hm(lpep_pickup_datetime),
    lpep_dropoff_datetime = mdy_hm(lpep_dropoff_datetime),
    trip_duration = as.numeric(difftime(lpep_dropoff_datetime, lpep_pickup_datetime, units = "hours")),
    speed = trip_distance / trip_duration
  )

# Getting rid of NAs
green_taxi_data <- filter(green_taxi_data, !is.infinite(speed), !is.na(speed))

# Correlation matrix has new 'speed' feature and tip amount
features <- c("trip_distance", "fare_amount", "total_amount", "tip_amount", "speed")
correlation <- cor(green_taxi_data[features], use = "complete.obs")


#total_amount shows the highest correlation with tip_amount, so the total charge of a trip likely influences the tip amount.
#The speed feature has a positive correlation with trip_distance and a weaker correlation with tip_amount, we thinnk that faster trips cover more distance but may not significantly influence tipping behavior.


# Visualization of the relationship between speed and tip amount
ggplot(green_taxi_data, aes(x = speed, y = tip_amount)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Relationship Between Speed and Tip Amount", x = "Speed (miles/hour)", y = "Tip Amount")


# There is no strong linear relationship between the speed of the trip and the tip amount.
# Most data points are clustered at lower speeds, which is expected in urban taxi trips due to traffic conditions and there are some extreme values in speed that are not common (airport trip from the top of our heads?)



# Remove 'ehail_fee' column due to missing values
green_taxi_data <- green_taxi_data %>% select(-ehail_fee)

# Impute missing values in 'fare_amount' and 'total_amount' using median
green_taxi_data <- green_taxi_data %>%
  mutate(
    fare_amount = ifelse(is.na(fare_amount), median(fare_amount, na.rm = TRUE), fare_amount),
    total_amount = ifelse(is.na(total_amount), median(total_amount, na.rm = TRUE), total_amount)
  ) %>%
  drop_na()

# Remove rows with missing 'tip_amount'
green_taxi_data <- green_taxi_data %>% filter(!is.na(tip_amount))

# Cap extreme outliers in 'tip_amount' at the 99th percentile
cap <- quantile(green_taxi_data$tip_amount, 0.99)
green_taxi_data <- green_taxi_data %>% mutate(tip_amount = ifelse(tip_amount > cap, cap, tip_amount))

# Log-transform 'tip_amount' to reduce skewness, adding 1 to handle zero values
green_taxi_data$tip_amount <- log(green_taxi_data$tip_amount + 1)

# Normalize continuous variables using z-score standardization
green_taxi_data <- green_taxi_data %>%
  mutate_at(vars(trip_distance, fare_amount, total_amount), scale)

# Convert 'RatecodeID' to a factor and create dummy variables
green_taxi_data <- green_taxi_data %>%
  mutate(RatecodeID = as.factor(RatecodeID)) %>%
  mutate(dummy_vars = model.matrix(~ RatecodeID + 0, data = .))

# Shuffle the data
set.seed(123)
green_taxi_data <- green_taxi_data[sample(nrow(green_taxi_data)),]

# Split the data into training and test sets (80% training, 20% test)
split_index <- floor(0.8 * nrow(green_taxi_data))
train_data <- green_taxi_data[1:split_index,]
test_data <- green_taxi_data[(split_index + 1):nrow(green_taxi_data),]


#REMOVE 'ehail_fee' has na's
#green_taxi_data <- green_taxi_data %>% select(-ehail_fee)
green_taxi_data <- green_taxi_data[, !colnames(green_taxi_data) %in% "ehail_fee"]

#Dealing with Nas here we replaced with median
green_taxi_data <- green_taxi_data %>%
  mutate(
    fare_amount = ifelse(is.na(fare_amount), median(fare_amount, na.rm = TRUE), fare_amount),
    total_amount = ifelse(is.na(total_amount), median(total_amount, na.rm = TRUE), total_amount)
  ) %>%
 drop_na()

green_taxi_data <- green_taxi_data %>%
 filter(!is.na(tip_amount))

# We set the cap for the tip_amount at a .99 to remove extreme outliers
cap <- quantile(green_taxi_data$tip_amount, 0.99)
green_taxi_data <- green_taxi_data %>%
  mutate(tip_amount = ifelse(tip_amount > cap, cap, tip_amount))

# We log-transform the tip_amount to reduce skewness, add 1 to handle the zero tips
green_taxi_data$tip_amount <- log(green_taxi_data$tip_amount + 1)

# We normalized the data using z-score standardization
green_taxi_data <- green_taxi_data %>%
  mutate_at(vars(trip_distance, fare_amount, total_amount), scale)

# Convert RatecodeID to a factor and then to a dummy variable
green_taxi_data <- green_taxi_data %>%
  mutate(RatecodeID = as.factor(RatecodeID)) %>%
  mutate(dummy_vars = model.matrix(~ RatecodeID + 0, data = .))

# Seed
set.seed(123)

# Shuffle
green_taxi_data <- green_taxi_data[sample(nrow(green_taxi_data)),]

# Split the data into training and test sets (e.g., 80% training and 20% test) # This is usual ml (machine learning split) this is why we used it (Hope this satifiess the professor)
split_index <- floor(0.8 * nrow(green_taxi_data))
train_data <- green_taxi_data[1:split_index,]
test_data <- green_taxi_data[(split_index + 1):nrow(green_taxi_data),]



# Just checking if there are any negative values in tip_amount dont want any #problems
negative_tips <- sum(green_taxi_data$tip_amount < 0)
print(paste("Number of negative tip amounts:", negative_tips))

# Create dummy variables for RatecodeID
green_taxi_data <- green_taxi_data %>%
mutate(RatecodeID = as.factor(RatecodeID))

# Create dummy variables
dummy_vars <- model.matrix(~ RatecodeID - 1, data = green_taxi_data)

# Binding the dummy variables to the original df
green_taxi_data <- cbind(green_taxi_data, dummy_vars)

# This just check on structure
glimpse(green_taxi_data)

# Checking for NA
na_count <- sapply(green_taxi_data, function(y) sum(length(which(is.na(y)))))
print(na_count)

# Checking for infinite
inf_count <- sapply(green_taxi_data, function(y) sum(length(which(is.infinite(y)))))
print(inf_count)

negative_tips_count <- sum(green_taxi_data$tip_amount < 0, na.rm = TRUE)
print(paste("Number of negative tip amounts:", negative_tips_count))

# Remove rows where tip_amount is negative
green_taxi_data <- green_taxi_data %>%
filter(tip_amount >= 0)

# Redo the log transformation now that the negative values are handled
green_taxi_data$tip_amount <- log(green_taxi_data$tip_amount + 1)

# Check for NA values
na_count <- sapply(green_taxi_data, function(y) sum(is.na(y)))
print(na_count)

# Check for infinite values
inf_count <- sapply(green_taxi_data, function(y) sum(is.infinite(y)))
print(inf_count)

library(FNN)

# knn.predict
knn.predict <- function(data_train, data_test, k) {
  numeric_columns <- sapply(data_train, is.numeric)
  train_data <- data_train[, numeric_columns]
  test_data <- data_test[, numeric_columns]

# Train and test
  train_labels <- data_train$tip_amount
  test_labels <- data_test$tip_amount

# k-NN regression
  set.seed(123)
  predictions <- knn.reg(train = train_data, test = test_data, y = train_labels, k = k)$pred

#Mean Squared Error (MSE)
  mse <- mean((predictions - test_labels)^2)

#MSE
  return(mse)
}

# Making sure RatecodeID dummy variables are numeric, bound and factors
ratecode_columns <- grep("RatecodeID", names(green_taxi_data), value = TRUE)
green_taxi_data[ratecode_columns] <- lapply(green_taxi_data[ratecode_columns], function(x) as.numeric(as.character(x)))

split_index <- floor(0.8 * nrow(green_taxi_data))
train_data <- green_taxi_data[1:split_index,]
test_data <- green_taxi_data[(split_index + 1):nrow(green_taxi_data),]

# k = 5
mse_result <- knn.predict(data_train = train_data, data_test = test_data, k = 5)
print(paste("The MSE for k=5 is:", mse_result))


#The MSE of 0.0342768535854126 is the squared difference between the predicted tip amounts and the actual tip amounts. Low (MSE)indicates that a model's predictions are closer to the true values. This reflects better overall performance
# Just a heads up from us: obviously the tip_amount was log-transformed and capped at the 99th percentile during preprocessing, which probably does affect the interpretation of MSE values




library(ggplot2)

# K values
k_values <- seq(1, 40, by = 2)

# Vector to store MSE for all k
mse_values <- numeric(length(k_values))

# Loop for k
for (i in seq_along(k_values)) {
  k <- k_values[i]
  mse_values[i] <- knn.predict(data_train = train_data, data_test = test_data, k = k)
}

# k values and their MSEs
data_frame_k_mse <- data.frame(k = k_values, MSE = mse_values)
print(data_frame_k_mse)

# Plot MSE vs. k values
mse_plot <- ggplot(data_frame_k_mse, aes(x = k, y = MSE)) +
  geom_line(group=1, colour="blue") +
  geom_point(shape=21, colour="red", fill="red", size=3) +
  labs(title = "MSE vs. k for k-NN Regression", x = "k (No of Neighbors)", y = "MSE") +
  theme_minimal()
print(mse_plot)

## Interpretion of chart and graph
#The lowest point represents the smallest MSE value, which occurs at k = 5. This means tath with all the ks, the k-NN regression model with k=5 it makes the most accurate predictions for the tip amount
#As k increases past 5, the MSE also increases
#This means that getting more neighbors does not lead to better predictions and is introducing noise into the predictions, leading to less accuracy.
#The model with k = 5 seems to have a good balance in the bias-variance tradeoff.
#Smaller values of k are typically associated with high variance low bias, while larger values of k can lead to high bias and low variance.


# Back-transform the tip_amount from the log scale
green_taxi_data$tip_amount_original <- exp(green_taxi_data$tip_amount) - 1

# Filter out extreme outliers or non-finite values
green_taxi_data <- green_taxi_data %>%
  filter(between(tip_amount_original, 0, quantile(tip_amount_original, 0.99, na.rm = TRUE)))

# Back-transform the tip_amount from log scale for visualization
green_taxi_data$tip_amount_original <- exp(green_taxi_data$tip_amount) - 1

# Extract the hour of the day from the pickup time
green_taxi_data$pickup_hour <- hour(green_taxi_data$lpep_pickup_datetime)

# Define time of day based on the hour
green_taxi_data$time_of_day <- cut(
 green_taxi_data$pickup_hour,
breaks = c(-Inf, 6, 12, 18, 24),
labels = c("Night", "Morning", "Afternoon", "Evening"),
include.lowest = TRUE
)

# Set reasonable limits for the x and y axes based on the data
x_lim <- c(0, max(green_taxi_data$trip_distance, na.rm = TRUE))
y_lim <- c(0, max(green_taxi_data$tip_amount_original, na.rm = TRUE))

# Create a hexbin plot
ggplot(green_taxi_data, aes(x = trip_distance, y = tip_amount_original)) +
geom_hex(aes(color = ..count..)) + # Color by the count of observations in each bin
scale_color_gradient(low = "blue", high = "red") + # Use a color gradient from blue to red
facet_wrap(~time_of_day) + # Create separate plots for each time of day
labs(
title = "Relationship Between Trip Distance, Tip Amount, and Time of Day",
x = "Trip Distance (miles)",
y = "Tip Amount (USD)",
color = "Number of Trips"
) +
theme_minimal() +
theme(legend.position = "right") +
xlim(x_lim) + ylim(y_lim) # Apply the axis limits


