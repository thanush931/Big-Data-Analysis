# ==========================================
# ROAD TRAFFIC ACCIDENT DATA ANALYSIS USING R
# ==========================================

# Install packages if not already installed

install.packages("dplyr")
install.packages("ggplot2")
install.packages("readr")
install.packages("tidyr")
install.packages("lubridate")

# Load required libraries

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(lubridate)


# ==========================================
# STEP 1: LOAD THE DATASET
# ==========================================

accident_data <- read_csv("road_accident_data.csv")

# Display first few rows

head(accident_data)

# Check dataset structure

str(accident_data)

# Summary of dataset

summary(accident_data)


# ==========================================
# STEP 2: CHECK MISSING VALUES
# ==========================================

colSums(is.na(accident_data))


# Remove rows with missing values

accident_data <- na.omit(accident_data)


# ==========================================
# STEP 3: REMOVE DUPLICATE RECORDS
# ==========================================

accident_data <- accident_data %>%
  distinct()


# ==========================================
# STEP 4: ANALYZE ACCIDENT SEVERITY
# ==========================================

severity_count <- accident_data %>%
  count(Accident_Severity)

print(severity_count)


# Bar chart for accident severity

ggplot(severity_count,
       aes(x = Accident_Severity, y = n)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Accident Severity Distribution",
    x = "Accident Severity",
    y = "Number of Accidents"
  ) +
  theme_minimal()


# ==========================================
# STEP 5: ANALYZE WEATHER CONDITIONS
# ==========================================

weather_count <- accident_data %>%
  count(Weather_Conditions) %>%
  arrange(desc(n))

print(weather_count)


# Weather condition graph

ggplot(weather_count,
       aes(x = reorder(Weather_Conditions, n),
           y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Accidents Based on Weather Conditions",
    x = "Weather Conditions",
    y = "Number of Accidents"
  ) +
  theme_minimal()


# ==========================================
# STEP 6: ANALYZE DAY-WISE ACCIDENTS
# ==========================================

day_count <- accident_data %>%
  count(Day_of_Week)

print(day_count)


ggplot(day_count,
       aes(x = Day_of_Week, y = n)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Day-wise Accident Analysis",
    x = "Day of Week",
    y = "Number of Accidents"
  ) +
  theme_minimal()


# ==========================================
# STEP 7: ANALYZE ROAD SURFACE CONDITIONS
# ==========================================

road_count <- accident_data %>%
  count(Road_Surface_Conditions)

print(road_count)


ggplot(road_count,
       aes(x = reorder(Road_Surface_Conditions, n),
           y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Accidents Based on Road Surface Conditions",
    x = "Road Surface Condition",
    y = "Number of Accidents"
  ) +
  theme_minimal()


# ==========================================
# STEP 8: ANALYZE CASUALTIES
# ==========================================

total_casualties <- sum(accident_data$Number_of_Casualties)

print(total_casualties)

average_casualties <- mean(
  accident_data$Number_of_Casualties
)

print(average_casualties)


# ==========================================
# STEP 9: SAVE RESULTS
# ==========================================

write.csv(
  severity_count,
  "accident_severity_results.csv",
  row.names = FALSE
)

write.csv(
  weather_count,
  "weather_analysis_results.csv",
  row.names = FALSE
)

print("Big Data Analysis Completed Successfully!")
