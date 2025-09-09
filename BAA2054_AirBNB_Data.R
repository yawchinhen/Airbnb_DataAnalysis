##DATA CLEANING---------
library(readxl)
AirBNB_Database <- read_excel("AirBNB Database.xlsx", 
                              na = "NA")
View(AirBNB_Database)

# Load necessary libraries
library(readxl)
library(dplyr)
install.packages("tidyr")
library(tidyr)

# Load the dataset
data <-(AirBNB_Database)

# Custom function to calculate the mode
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Handle missing values
# Fill missing values for numerical columns with median
data$bathrooms[is.na(data$bathrooms)] <- median(data$bathrooms, na.rm = TRUE)
data$bedrooms[is.na(data$bedrooms)] <- median(data$bedrooms, na.rm = TRUE)
data$beds[is.na(data$beds)] <- median(data$beds, na.rm = TRUE)
data$review_scores_rating[is.na(data$review_scores_rating)] <- median(data$review_scores_rating, na.rm = TRUE)

# Fill missing values for categorical columns with mode
data$neighbourhood[is.na(data$neighbourhood)] <- get_mode(data$neighbourhood)
data$zipcode[is.na(data$zipcode)] <- get_mode(data$zipcode)

# Drop rows with missing values for 'description' and 'name'
data <- data %>% drop_na(description, name)

# Fill missing values for host related columns
data$host_response_rate[is.na(data$host_response_rate)] <- median(data$host_response_rate, na.rm = TRUE)
data$host_has_profile_pic[is.na(data$host_has_profile_pic)] <- get_mode(data$host_has_profile_pic)
data$host_identity_verified[is.na(data$host_identity_verified)] <- get_mode(data$host_identity_verified)
data$host_since[is.na(data$host_since)] <- get_mode(data$host_since)

# Drop columns that are not critical
data <- data %>% select(-thumbnail_url, -first_review, -last_review)

# Ensure log_price is numeric
data$log_price <- as.numeric(data$log_price)

# Generate the Original Price from log_price
data$Original_Price <- exp(data$log_price)

# Verify there are no missing values left
sapply(data, function(x) sum(is.na(x)))

# Save the cleaned data
write.csv(data, "cleaned_airbnb_data.csv", row.names = FALSE)

##PREDICTIVE ANALYTICS--------
library(readxl)
cleaned_airbnb_data <- read_excel("cleaned_airbnb_data.xlsx", 
                                  na = "NA")
View(cleaned_airbnb_data)

##1. Host Onboarding Trend Analysis-------
# Load necessary libraries
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)

# Load the dataset
cleaned_airbnb_data <- read_excel("cleaned_airbnb_data.xlsx", na = "NA")

# Convert 'host_since' to Date type
cleaned_airbnb_data$host_since <- as.Date(cleaned_airbnb_data$host_since)

# Aggregate the number of new hosts by month
monthly_hosts <- cleaned_airbnb_data %>%
  mutate(month = floor_date(host_since, "month")) %>%
  count(month) %>%
  rename(new_hosts = n)

# Convert to time series object
ts_monthly_hosts <- ts(monthly_hosts$new_hosts, start = c(year(min(monthly_hosts$month)), month(min(monthly_hosts$month))), frequency = 12)

# Fit ARIMA model
fit <- auto.arima(ts_monthly_hosts)

# Forecast for the next 12 months
forecasted <- forecast(fit, h = 12)

# Plot the results
autoplot(forecasted) +
  ggtitle("Forecast of New Hosts Per Month") +
  xlab("Time") + ylab("Number of New Hosts") +
  theme_minimal()

##2. Host Tenure Analysis--------------
# Load necessary libraries
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)

# Load the dataset
cleaned_airbnb_data <- read_excel("cleaned_airbnb_data.xlsx", na = "NA")

# Convert 'host_since' to Date type
cleaned_airbnb_data$host_since <- as.Date(cleaned_airbnb_data$host_since)

# Calculate host tenure
current_date <- Sys.Date()
cleaned_airbnb_data <- cleaned_airbnb_data %>%
  mutate(host_tenure = as.numeric(difftime(current_date, host_since, units = "days")))

# Aggregate the number of hosts by tenure
tenure_counts <- cleaned_airbnb_data %>%
  count(host_tenure) %>%
  rename(count = n)

# Convert to time series object
ts_tenure_counts <- ts(tenure_counts$count, start = min(tenure_counts$host_tenure), frequency = 1)

# Fit Exponential Smoothing model
fit <- ets(ts_tenure_counts)

# Forecast for the next 365 periods (1 year)
forecasted <- forecast(fit, h = 365)

# Plot the results
autoplot(forecasted) +
  ggtitle("Forecast of Host Tenure Distribution") +
  xlab("Tenure (Days)") + ylab("Number of Hosts") +
  theme_minimal()

##3. Host Growth Rate Analysis-------
# Load necessary libraries
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)

# Load the dataset
cleaned_airbnb_data <- read_excel("cleaned_airbnb_data.xlsx", na = "NA")

# Convert 'host_since' to Date type
cleaned_airbnb_data$host_since <- as.Date(cleaned_airbnb_data$host_since)

# Aggregate the number of new hosts by month
monthly_hosts <- cleaned_airbnb_data %>%
  mutate(month = floor_date(host_since, "month")) %>%
  count(month) %>%
  rename(new_hosts = n)

# Calculate growth rate
monthly_hosts <- monthly_hosts %>%
  arrange(month) %>%
  mutate(growth_rate = (new_hosts - lag(new_hosts)) / lag(new_hosts))

# Remove NA values and infinite values (if any)
monthly_hosts <- monthly_hosts %>%
  filter(!is.na(growth_rate) & !is.infinite(growth_rate))

# Convert to time series object
ts_growth_rate <- ts(monthly_hosts$growth_rate, start = c(year(min(monthly_hosts$month)), month(min(monthly_hosts$month))), frequency = 12)

# Fit ARIMA model
fit <- auto.arima(ts_growth_rate)

# Forecast for the next 12 months
forecasted <- forecast(fit, h = 12)

# Plot the results
autoplot(forecasted) +
  ggtitle("Forecast of Host Growth Rate") +
  xlab("Time") + ylab("Growth Rate") +
  theme_minimal()

##PRESCRIPTIVE ANALYTICS--------
library(readxl)
cleaned_airbnb_data <- read_excel("cleaned_airbnb_data.xlsx", 
                                  na = "NA")
View(cleaned_airbnb_data)
##1. Decision Tree Analysis------
library(readxl)
cleaned_airbnb_data <- read_excel("cleaned_airbnb_data.xlsx", 
                                  na = "NA")
View(cleaned_airbnb_data)

# Install and load required packages
library(readxl)
library(rpart)
library(rpart.plot)
library(dplyr)

# Load the dataset
cleaned_airbnb_data <- read_excel("cleaned_airbnb_data.xlsx", na = "NA")
View(cleaned_airbnb_data)

# Inspect column names to ensure they match
colnames(cleaned_airbnb_data)

# Prepare the data for decision tree analysis
# Select relevant features and target variable
data <- cleaned_airbnb_data %>%
  select(accommodates, bedrooms, bathrooms, beds, Rental_Price)

# Build the decision tree model
model <- rpart(Rental_Price ~ accommodates + bedrooms + bathrooms + beds, data = data, method = "anova")

# Summary of the model
summary(model)

# Visualize the decision tree
rpart.plot(model, main = "Decision Tree for Pricing Strategy", type = 3)
