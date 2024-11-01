# Install and load necessary packages
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

#-----------------------------------------------------------------------------
# Load data
library(readr)
library(readr)
illinois_data <- read_csv("~/Downloads/Illinoise_corn_50.csv")

# Convert date format
illinois_data$datetime <- as.Date(illinois_data$datetime, format = "%m/%d/%Y") 
# Extract data from May 15 to July 15 for each year
filtered_illinois <- illinois_data %>%
  mutate(year = lubridate::year(datetime),
         month = lubridate::month(datetime),
         day = lubridate::day(datetime)) %>%
  filter(month == 5 & day >= 15 | month == 6 | month == 7 & day <= 15)
# Select required columnssource('~/REEU.R', chdir = TRUE)
selected_columns <- c("datetime", "tempmax", "tempmin", "temp", "windspeed", "winddir", "cloudcover", "icon")
filtered_illinois <- filtered_illinois[, selected_columns]
# View extracted data for Illinois
print(filtered_illinois)
# Calculate mean values
mean_temp <- mean(filtered_illinois$temp, na.rm = TRUE)
max_temp <- mean(filtered_illinois$tempmax, na.rm = TRUE)
min_temp <- mean(filtered_illinois$tempmin, na.rm = TRUE)
# Fill in missing values
filtered_illinois <- filtered_illinois %>%
  mutate(temp = ifelse(is.na(temp), mean_temp, temp))
filtered_illinois <- filtered_illinois %>%
  mutate(tempmax = ifelse(is.na(tempmax), max_temp, tempmax))
filtered_illinois <- filtered_illinois %>%
  mutate(tempmin = ifelse(is.na(tempmin), min_temp, tempmin))

# Select three dates per year
selected_dates <- filtered_illinois %>%
  group_by(year = lubridate::year(datetime)) %>%
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

# Select data for every third year
selected_years <- filtered_illinois %>%
  group_by(year = lubridate::year(datetime)) %>%
  filter(year %% 3 == 0) %>%
  ungroup()

# Plot average temperature bar chart, differentiated by year
ggplot(selected_years, aes(x = lubridate::month(datetime), y = temp, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Month", y = "Average Temperature", fill = "Year") +
  ggtitle("Average Temperature Changes in Illinois Every Three Years") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_fill_discrete(guide = guide_legend(title = NULL)) +
  theme(legend.title = element_blank())

# Repeat the same data loading, processing, and visualization for Minnesota and Nebraska
#-----------------------------------------------------------------------------

# Load income data--------------------------------------------------------------

# Read data

soybean_yield <- read.csv("~/Downloads/Soybeans_50.csv")
# View data
head(soybean_yield)

# Split data by state (ILLINOIS) and keep "Year" and "Yield" columns
illinois_yield <- soybean_yield[soybean_yield$State == "ILLINOIS", c("Year", "Yield")]
# Calculate average values by year
illinois_avg_temp <- aggregate(cbind(temp, tempmax, tempmin) ~ format(datetime, "%Y"), data = filtered_illinois, FUN = mean)
# Rename "Year" column
colnames(illinois_avg_temp)[1] <- "Year"
# Merge with illinois_yield data frame
illinois_yield <- merge(illinois_yield, illinois_avg_temp, by = "Year", all.x = TRUE)
head(illinois_yield)

# Repeat the same for Minnesota and Nebraska
#-----------------------------------------------------------------------------

# To analyze whether temperature changes affect income, we can check if temperatures exceed or fall below a certain threshold and add this as a new column. We can then compare temperature anomalies with income.

# Define temperature anomaly thresholds
temperature_high_threshold <- 80  # High temperature threshold
temperature_low_threshold <- 60   # Low temperature threshold

# Add temperature anomaly column
illinois_yield$Temperature_Abnormal <- ifelse(illinois_yield$temp > temperature_high_threshold, "High", ifelse(illinois_yield$temp < temperature_low_threshold, "Low", "Normal"))
minnesota_yield$Temperature_Abnormal <- ifelse(minnesota_yield$temp > temperature_high_threshold, "High", ifelse(minnesota_yield$temp < temperature_low_threshold, "Low", "Normal"))
nebraska_yield$Temperature_Abnormal <- ifelse(nebraska_yield$temp > temperature_high_threshold, "High", ifelse(nebraska_yield$temp < temperature_low_threshold, "Low", "Normal"))

# Visualize yield and temperature correlation in Illinois
ggplot(data = illinois_yield, aes(x = Year, y = Yield)) +
  geom_line(color = "blue") +
  geom_line(aes(y = tempmax, color = "Max Temp"), linetype = "dashed") +
  geom_line(aes(y = tempmin, color = "Min Temp"), linetype = "dotted") +
  labs(title = "Yield and Temperature in Illinois",
       x = "Year", y = "Yield",
       color = "Temperature") +
  theme_minimal()

# Boxplot to compare yield distribution across states
boxplot_data <- data.frame(Yield = c(illinois_yield$Yield, minnesota_yield$Yield, nebraska_yield$Yield),
                           Temperature = rep(c("Illinois", "Minnesota", "Nebraska"), each = length(illinois_yield$Year)))

ggplot(data = boxplot_data, aes(x = Temperature, y = Yield)) +
  geom_boxplot() +
  labs(title = "Yield Distribution across States",
       x = "State", y = "Yield") +
  theme_minimal()

# Correlation analysis
correlation <- cor(illinois_yield[, c("Yield", "temp", "tempmax", "tempmin")])
correlation

# Convert correlation matrix to long format
heatmap_data <- reshape2::melt(correlation)
colnames(heatmap_data) <- c("Variable 1", "Variable 2", "Correlation")

# Plot heatmap
ggplot(data = heatmap_data, aes(x = `Variable 1`, y = `Variable 2`)) +
  geom_tile(aes(fill = Correlation), color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Correlation Heatmap",
       x = "Variable 1", y = "Variable 2") +
  theme_minimal()

# Analyze the impact of temperature anomalies on income
illinois_yield_summary <- aggregate(Yield ~ Temperature_Abnormal, data = illinois_yield, FUN = mean)
minnesota_yield_summary <- aggregate(Yield ~ Temperature_Abnormal, data = minnesota_yield, FUN = mean)
nebraska_yield_summary <- aggregate(Yield ~ Temperature_Abnormal, data = nebraska_yield, FUN = mean)

illinois_yield_summary
minnesota_yield_summary
nebraska_yield_summary

