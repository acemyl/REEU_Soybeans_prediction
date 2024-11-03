# -----------------------------------------------------------------------------
# Install and load necessary packages
# -----------------------------------------------------------------------------
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages("lubridate")
library(lubridate)
install.packages("dplyr")
library(dplyr)

# -----------------------------------------------------------------------------
# Load and Process Data
# -----------------------------------------------------------------------------

# Load Illinois Data
illinois_data <- read_csv("/Users/yumo/Desktop/REEU_Soybean_Prediction/REEU/Illinoise_Soybeans_50.csv")

# Convert date format
illinois_data$datetime <- as.Date(illinois_data$datetime, format = "%m/%d/%Y")

# Filter data between May 15 and July 15 each year
filtered_illinois <- illinois_data %>%
  mutate(year = year(datetime), month = month(datetime), day = day(datetime)) %>%
  filter((month == 5 & day >= 15) | (month == 6) | (month == 7 & day <= 15))

# Select required columns
selected_columns <- c("datetime", "tempmax", "tempmin", "temp", "windspeed", "winddir", "cloudcover", "icon")
filtered_illinois <- filtered_illinois[, selected_columns]

# -----------------------------------------------------------------------------
# Handle Missing Values
# -----------------------------------------------------------------------------

# Calculate mean values for temperature variables
mean_temp <- mean(filtered_illinois$temp, na.rm = TRUE)
max_temp <- mean(filtered_illinois$tempmax, na.rm = TRUE)
min_temp <- mean(filtered_illinois$tempmin, na.rm = TRUE)

# Replace missing values
filtered_illinois <- filtered_illinois %>%
  mutate(temp = ifelse(is.na(temp), mean_temp, temp),
         tempmax = ifelse(is.na(tempmax), max_temp, tempmax),
         tempmin = ifelse(is.na(tempmin), min_temp, tempmin))

# -----------------------------------------------------------------------------
# Data Sampling and Visualization
# -----------------------------------------------------------------------------

# Sample three dates per year for Illinois
selected_dates <- filtered_illinois %>%
  group_by(year = year(datetime)) %>%
  slice_sample(n = 3) %>%
  ungroup()

# Plot yearly maximum, minimum, and average temperatures
ggplot(selected_dates, aes(x = datetime)) +
  geom_line(aes(y = tempmax, color = "Max Temperature")) +
  geom_line(aes(y = tempmin, color = "Min Temperature")) +
  geom_line(aes(y = temp, color = "Average Temperature")) +
  labs(x = "Date", y = "Temperature", color = "Metric") +
  scale_color_manual(values = c("Max Temperature" = "red", "Min Temperature" = "blue", "Average Temperature" = "green")) +
  ggtitle("Temperature Changes in Illinois") +
  theme(legend.title = element_blank())

# -----------------------------------------------------------------------------
# Repeat Analysis for Minnesota Data
# -----------------------------------------------------------------------------

# Assuming minnesota_data is already loaded
minnesota_data$datetime <- as.Date(minnesota_data$datetime, format = "%m/%d/%Y")

# Process and filter data for Minnesota (similar steps as Illinois)
filtered_minnesota <- minnesota_data %>%
  mutate(year = year(datetime), month = month(datetime), day = day(datetime)) %>%
  filter((month == 5 & day >= 15) | (month == 6) | (month == 7 & day <= 15)) %>%
  select(selected_columns)

# Handle missing values for Minnesota
mean_temp <- mean(filtered_minnesota$temp, na.rm = TRUE)
max_temp <- mean(filtered_minnesota$tempmax, na.rm = TRUE)
min_temp <- mean(filtered_minnesota$tempmin, na.rm = TRUE)
filtered_minnesota <- filtered_minnesota %>%
  mutate(temp = ifelse(is.na(temp), mean_temp, temp),
         tempmax = ifelse(is.na(tempmax), max_temp, tempmax),
         tempmin = ifelse(is.na(tempmin), min_temp, tempmin))

# Sample and visualize data for Minnesota
selected_dates <- filtered_minnesota %>%
  group_by(year = year(datetime)) %>%
  slice_sample(n = 3) %>%
  ungroup()

# Plot yearly temperatures for Minnesota
ggplot(selected_dates, aes(x = datetime)) +
  geom_line(aes(y = tempmax, color = "Max Temperature")) +
  geom_line(aes(y = tempmin, color = "Min Temperature")) +
  geom_line(aes(y = temp, color = "Average Temperature")) +
  labs(x = "Date", y = "Temperature", color = "Metric") +
  scale_color_manual(values = c("Max Temperature" = "red", "Min Temperature" = "blue", "Average Temperature" = "green")) +
  ggtitle("Temperature Changes in Minnesota") +
  theme(legend.title = element_blank())

# -----------------------------------------------------------------------------
# Repeat Analysis for Nebraska Data
# -----------------------------------------------------------------------------

# Load Nebraska Data
nebraska_data <- read_csv("/path_to_your_data/Nebraska_Soybeans_50.csv")

# Convert date format
nebraska_data$datetime <- as.Date(nebraska_data$datetime, format = "%m/%d/%Y")

# Filter data between May 15 and July 15 each year
filtered_nebraska <- nebraska_data %>%
  mutate(year = year(datetime), month = month(datetime), day = day(datetime)) %>%
  filter((month == 5 & day >= 15) | (month == 6) | (month == 7 & day <= 15))

# Select required columns
filtered_nebraska <- filtered_nebraska[, selected_columns]

# -----------------------------------------------------------------------------
# Handle Missing Values for Nebraska
# -----------------------------------------------------------------------------

# Calculate mean values for temperature variables
mean_temp <- mean(filtered_nebraska$temp, na.rm = TRUE)
max_temp <- mean(filtered_nebraska$tempmax, na.rm = TRUE)
min_temp <- mean(filtered_nebraska$tempmin, na.rm = TRUE)

# Replace missing values
filtered_nebraska <- filtered_nebraska %>%
  mutate(temp = ifelse(is.na(temp), mean_temp, temp),
         tempmax = ifelse(is.na(tempmax), max_temp, tempmax),
         tempmin = ifelse(is.na(tempmin), min_temp, tempmin))

# -----------------------------------------------------------------------------
# Data Sampling and Visualization for Nebraska
# -----------------------------------------------------------------------------

# Sample three dates per year for Nebraska
selected_dates <- filtered_nebraska %>%
  group_by(year = year(datetime)) %>%
  slice_sample(n = 3) %>%
  ungroup()

# Plot yearly maximum, minimum, and average temperatures for Nebraska
ggplot(selected_dates, aes(x = datetime)) +
  geom_line(aes(y = tempmax, color = "Max Temperature")) +
  geom_line(aes(y = tempmin, color = "Min Temperature")) +
  geom_line(aes(y = temp, color = "Average Temperature")) +
  labs(x = "Date", y = "Temperature", color = "Metric") +
  scale_color_manual(values = c("Max Temperature" = "red", "Min Temperature" = "blue", "Average Temperature" = "green")) +
  ggtitle("Temperature Changes in Nebraska") +
  theme(legend.title = element_blank())

# -----------------------------------------------------------------------------
# Additional Analysis: Yield and Temperature Correlation for Nebraska
# -----------------------------------------------------------------------------

# Extract Nebraska yield data and merge with temperature data
nebraska_yield <- soybean_yield[soybean_yield$State == "NEBRASKA", c("Year", "Yield")]
nebraska_avg_temp <- aggregate(cbind(temp, tempmax, tempmin) ~ format(datetime, "%Y"), data = filtered_nebraska, FUN = mean)
colnames(nebraska_avg_temp)[1] <- "Year"
nebraska_yield <- merge(nebraska_yield, nebraska_avg_temp, by = "Year", all.x = TRUE)

# -----------------------------------------------------------------------------
# Visualize Yield and Temperature Correlation for Nebraska
# -----------------------------------------------------------------------------

# Plot yield vs. temperature in Nebraska
ggplot(data = nebraska_yield, aes(x = Year, y = Yield)) +
  geom_line(color = "blue") +
  geom_line(aes(y = tempmax, color = "Max Temp"), linetype = "dashed") +
  geom_line(aes(y = tempmin, color = "Min Temp"), linetype = "dotted") +
  labs(title = "Yield and Temperature in Nebraska", x = "Year", y = "Yield", color = "Temperature") +
  theme_minimal()

# -----------------------------------------------------------------------------
# Summary and Correlation Analysis for Nebraska
# -----------------------------------------------------------------------------

# Correlation matrix for Nebraska yield and temperature
correlation <- cor(nebraska_yield[, c("Yield", "temp", "tempmax", "tempmin")])

# Heatmap for correlation
heatmap_data <- reshape2::melt(correlation)
colnames(heatmap_data) <- c("Variable 1", "Variable 2", "Correlation")
ggplot(data = heatmap_data, aes(x = `Variable 1`, y = `Variable 2`)) +
  geom_tile(aes(fill = Correlation), color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Correlation Heatmap", x = "Variable 1", y = "Variable 2") +
  theme_minimal()

-----------------------------------------------------------
# Define Function for Yield and Temperature Correlation Analysis
# -----------------------------------------------------------------------------

yield_temp_correlation_analysis <- function(filtered_data, state_name, yield_data) {
  # Filter the yield data for the given state
  state_yield <- yield_data[yield_data$State == toupper(state_name), c("Year", "Yield")]
  
  # Calculate average temperature variables by year
  state_avg_temp <- aggregate(cbind(temp, tempmax, tempmin) ~ format(datetime, "%Y"), data = filtered_data, FUN = mean)
  colnames(state_avg_temp)[1] <- "Year"  # Rename Year column
  
  # Merge yield data with average temperature data
  state_yield <- merge(state_yield, state_avg_temp, by = "Year", all.x = TRUE)
  
  # -----------------------------------------------------------------------------
  # Visualize Yield and Temperature Correlation
  # -----------------------------------------------------------------------------
  
  ggplot(data = state_yield, aes(x = Year, y = Yield)) +
    geom_line(color = "blue") +
    geom_line(aes(y = tempmax, color = "Max Temp"), linetype = "dashed") +
    geom_line(aes(y = tempmin, color = "Min Temp"), linetype = "dotted") +
    labs(title = paste("Yield and Temperature in", state_name), x = "Year", y = "Yield", color = "Temperature") +
    theme_minimal()
  
  # -----------------------------------------------------------------------------
  # Summary and Correlation Analysis
  # -----------------------------------------------------------------------------
  
  # Calculate correlation matrix
  correlation <- cor(state_yield[, c("Yield", "temp", "tempmax", "tempmin")], use = "complete.obs")
  
  # Reshape correlation matrix for heatmap
  heatmap_data <- melt(correlation)
  colnames(heatmap_data) <- c("Variable 1", "Variable 2", "Correlation")
  
  # Plot correlation heatmap
  ggplot(data = heatmap_data, aes(x = `Variable 1`, y = `Variable 2`)) +
    geom_tile(aes(fill = Correlation), color = "white") +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    labs(title = paste("Correlation Heatmap for", state_name), x = "Variable 1", y = "Variable 2") +
    theme_minimal()
  
  # Return processed data if further analysis is needed
  return(state_yield)
}

# -----------------------------------------------------------------------------
# Load Yield Data and Process Data for Each State
# -----------------------------------------------------------------------------

# Load yield data
soybean_yield <- read.csv("/Users/yumo/Downloads/Soybeans_50.csv", sep = "\t")

# Assuming filtered datasets for Illinois, Minnesota, and Nebraska are already prepared:
# - filtered_illinois
# - filtered_minnesota
# - filtered_nebraska

# Process and analyze data for Illinois
illinois_yield <- yield_temp_correlation_analysis(filtered_illinois, "ILLINOIS", soybean_yield)

# Process and analyze data for Minnesota
minnesota_yield <- yield_temp_correlation_analysis(filtered_minnesota, "MINNESOTA", soybean_yield)

# Process and analyze data for Nebraska
nebraska_yield <- yield_temp_correlation_analysis(filtered_nebraska, "NEBRASKA", soybean_yield)

# -----------------------------------------------------------------------------
# Install and load necessary packages
# -----------------------------------------------------------------------------
install.packages("tidyverse")
install.packages("lubridate")
install.packages("tseries")
install.packages("forecast")
install.packages("reshape2")

library(tidyverse)
library(lubridate)
library(tseries)
library(forecast)
library(reshape2)

# -----------------------------------------------------------------------------
# Define function for time series analysis and hypothesis testing
# -----------------------------------------------------------------------------

time_series_hypothesis_test <- function(filtered_data, state_name, yield_data, high_temp_threshold = 80) {
  # Filter yield data for the given state
  state_yield <- yield_data[yield_data$State == toupper(state_name), c("Year", "Yield")]
  
  # Calculate annual average temperature and merge with yield data
  state_avg_temp <- aggregate(cbind(temp, tempmax, tempmin) ~ format(datetime, "%Y"), data = filtered_data, FUN = mean)
  colnames(state_avg_temp)[1] <- "Year"
  state_yield <- merge(state_yield, state_avg_temp, by = "Year", all.x = TRUE)
  
  # Convert Year to date format for time series
  state_yield$Year <- as.numeric(state_yield$Year)
  
  # -----------------------------------------------------------------------------
  # Time Series Analysis
  # -----------------------------------------------------------------------------
  
  # Create time series objects for temperature and yield
  yield_ts <- ts(state_yield$Yield, start = min(state_yield$Year), frequency = 1)
  tempmax_ts <- ts(state_yield$tempmax, start = min(state_yield$Year), frequency = 1)
  
  # Seasonal decomposition of temperature data
  decomposed_temp <- decompose(tempmax_ts)
  plot(decomposed_temp)
  
  # Visualize yield time series with a rolling average
  plot(yield_ts, main = paste("Yield Time Series in", state_name), ylab = "Yield", xlab = "Year")
  lines(ma(yield_ts, order = 3), col = "red")  # 3-year moving average
  
  # -----------------------------------------------------------------------------
  # Identify High-Temperature Years
  # -----------------------------------------------------------------------------
  
  # Add column indicating high temperature anomaly
  state_yield$Temperature_Anomaly <- ifelse(state_yield$tempmax > high_temp_threshold, "High Temp", "Normal Temp")
  
  # Summarize average yields for high-temp vs. normal years
  yield_summary <- state_yield %>%
    group_by(Temperature_Anomaly) %>%
    summarize(mean_yield = mean(Yield, na.rm = TRUE), sd_yield = sd(Yield, na.rm = TRUE))
  print(yield_summary)
  
  # -----------------------------------------------------------------------------
  # Hypothesis Testing
  # -----------------------------------------------------------------------------
  
  # Perform a two-sample t-test for yields between high-temp and normal-temp years
  high_temp_yield <- state_yield$Yield[state_yield$Temperature_Anomaly == "High Temp"]
  normal_temp_yield <- state_yield$Yield[state_yield$Temperature_Anomaly == "Normal Temp"]
  
  # Test if high temperature significantly impacts yield
  t_test_result <- t.test(high_temp_yield, normal_temp_yield, alternative = "less")
  print(t_test_result)
  
  # Check for significance and output
  if (t_test_result$p.value < 0.05) {
    cat(paste("In", state_name, "high temperatures have a statistically significant impact on yield (p =", round(t_test_result$p.value, 4), ").\n"))
  } else {
    cat(paste("In", state_name, "high temperatures do not have a statistically significant impact on yield (p =", round(t_test_result$p.value, 4), ").\n"))
  }
  
  # Return result
  return(list(yield_summary = yield_summary, t_test_result = t_test_result))
}

# -----------------------------------------------------------------------------
# Main Analysis for Each State
# -----------------------------------------------------------------------------

# Load yield data
soybean_yield <- read.csv("/Users/yumo/Downloads/Soybeans_50.csv", sep = "\t")

# Run the analysis for Illinois, Minnesota, and Nebraska (assuming filtered datasets are loaded)
illinois_result <- time_series_hypothesis_test(filtered_illinois, "ILLINOIS", soybean_yield)
minnesota_result <- time_series_hypothesis_test(filtered_minnesota, "MINNESOTA", soybean_yield)
nebraska_result <- time_series_hypothesis_test(filtered_nebraska, "NEBRASKA", soybean_yield)
