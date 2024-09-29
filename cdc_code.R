library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyverse)
data <- read_csv("C:\\Users\\rache\\OneDrive\\Documents\\msa_scripts\\sentiment_merged.csv")
# Assuming 'data' is your DataFrame containing the dataset
# Make sure to convert 'time' column to Date format if it's not already
data$time1 <- as.Date(data$time,format="%m/%d/%Y")

# Filter for reviews in Paris
#paris_data <- data %>% filter(location == "Paris")

# Create a year-month column for grouping
#paris_data$year_month <- format(paris_data$time1, "%Y-%m")
data$year_month <- format(data$time1, "%Y-%m")

# Group by year-month and calculate the average sentiment
average_sentiment_per_month <- data %>%
  group_by(location,year_month) %>%
  summarise(average_sentiment = mean(sentiment, na.rm = TRUE))
ggplot(average_sentiment_per_month, aes(x = year_month, y = average_sentiment)) +
  geom_line(group=1, color="orange") + 
  geom_point(color="orange") +
  theme_minimal() +
  labs(title = "Average Sentiment per Month for Reviews in Paris",
       x = "Month",
       y = "Average Sentiment") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(average_sentiment_per_month, aes(x = year_month, y = average_sentiment, color = location, group = location)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Sentiment per Month by City",
       x = "Month",
       y = "Average Sentiment") +
  theme_minimal() + # Adjust date breaks and labels
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
data$year <- format(data$time1, "%Y")
reviews_per_year <- data %>%
  group_by(location,year) %>%
  summarise(review_count = n())
print(reviews_per_year)

p <- data %>%
  ggplot(aes(x=data$sentiment, fill=factor(location))) +
  geom_histogram( color="#E9ECEF", alpha=0.8, position = 'identity') +
  scale_fill_manual(values=c("#69B3A2", "#404080",'red','yellow','gray','brown','magenta','purple')) +
  labs(fill="")
p

p <- ggplot(reviews_per_year, aes(x = year, y = review_count, fill = location)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Number of Reviews per Location by Year",
       x = "Year",
       y = "Number of Reviews",
       fill = "Location") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

#london
london_data <- data %>% filter(location == "london")
london_average_sentiment_per_month <- data %>%
  group_by(year_month) %>%
  summarise(average_sentiment = mean(sentiment, na.rm = TRUE),
            temperature_mean = mean(temperature_2m_mean, na.rm=TRUE))
london_average_sentiment_per_month <- london_average_sentiment_per_month %>%
  mutate(
    mean_temp = mean(temperature_mean, na.rm = TRUE),
    sd_temp = sd(temperature_mean, na.rm = TRUE),
    standardized_temperature = (temperature_mean - mean_temp) / sd_temp
  ) %>%
  select(-mean_temp, -sd_temp)

p <- ggplot(london_average_sentiment_per_month, aes(x = year_month)) +

  geom_line(y = london_average_sentiment_per_month$standardized_temperature) +
  labs(title = "Sentiment and Standardized Temperature Over Time",
       x = "Time",
       y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p
coeff <- 10

ggplot(london_average_sentiment_per_month, aes(x=year_month)) +
  
  geom_line(aes(y = average_sentiment)) +
  geom_line(aes(y = standardized_temperature)) + # Divide by 10 to get the same range than the temperature
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "First Axis",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Second Axis")
  )

ggplot(london_average_sentiment_per_month, aes(x = year_month)) +
  geom_line(aes(y = average_sentiment)) +
  labs(title = "Average Sentiment per Month by City",
       x = "Month",
       y = "Average Sentiment") +
  theme_minimal() + # Adjust date breaks and labels
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(london_average_sentiment_per_month, aes(x = year_month, y = standardized_temperature)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Sentiment per Month by City",
       x = "Month",
       y = "Average Sentiment") +
  theme_minimal() + # Adjust date breaks and labels
  theme(axis.text.x = element_text(angle = 90, hjust = 1))







