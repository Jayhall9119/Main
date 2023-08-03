# To begin my analysis, I imported two datasets, namely AQI_Data.csv and World_Pop_2022.csv, into Google Sheets.
# For the World Population data, I focused solely on the 2022 population, country, and continent information. 
# To streamline the data, I removed unnecessary columns, which I saved for future analysis.
# Once I had made these adjustments, I imported the updated datasets into RStudio.



# Import Data sets.
AQI_Data <- read.csv("C:/Users/guaji/Desktop/School/Capstone/AQI_Data.csv")
World_Pop_2022 <- read.csv("C:/Users/guaji/Desktop/School/Capstone/World_Pop_2022.csv")


# View Data sets.
View(AQI_Data)
view(World_Pop_2022)


# Load necessary packages.
library(tidyverse)

library(lubridate)

library(readr)

library(dplyr)

library(tidyr)

library(ggplot2)

library(scales)

library(janitor)

# I wanted to merge the two tables so that I can compare AQI and population for each country.
AQI_Population <- merge(World_Pop_2022, AQI_Data, by = "Country")


# View the first few rows of the dataset.
head(AQI_Population)


# Get summary statistics of the dataset.
summary(AQI_Population)


# Convert character date to Date class
AQI_Population$Date <- as.Date(AQI_Population$Date)


# Check the structure of the dataset.
str(AQI_Population)


# Check for any missing values.
sum(is.na(AQI_Population))


# Find the most recent date.
Most_Recent_Date <- max(AQI_Population$Date)
print(Most_Recent_Date)


# At the time of my analysis the most recent date is 7/27/2023. 


# Check for duplicate dates in the Date column
duplicates <- AQI_Population[duplicated(AQI_Population$Date), ]


# Use distinct() to keep only the first occurrence of each country on each date.
Clean_AQI_Data <- AQI_Population %>%
  distinct(Date, Country, .keep_all = TRUE)
print(Clean_AQI_Data)
view(Clean_AQI_Data)


# Arrange the data by "Date" in descending order
Clean_AQI_Sorted <- Clean_AQI_Data %>%
  arrange(desc(Date))
print(Clean_AQI_Sorted)
view(Clean_AQI_Sorted)


# What is the average AQI value?
mean(Clean_AQI_Sorted$AQI)
Avg_AQI <- c(mean(Clean_AQI_Sorted$AQI))
print(Avg_AQI)

# What is the highest AQI value?
max(Clean_AQI_Sorted$AQI)
Max_AQI <- max(Clean_AQI_Sorted$AQI)
print(Max_AQI)

# What is the lowest AQI value?
min(Clean_AQI_Sorted$AQI)
Min_AQI <- min(Clean_AQI_Sorted$AQI)
print(Min_AQI)


# What country has the highest population?
max(Clean_AQI_Sorted$Population_2022)
Max_Population <- max(Clean_AQI_Sorted$Population_2022)

# Find the row with the maximum population value
Country_With_Max_Population <- Clean_AQI_Sorted %>%
  filter(Population_2022 == max(Population_2022)) %>%
  select(Country)

# Extract the name of the country with the highest population
Largest_Population <- Country_With_Max_Population %>%
  distinct()

# Print the country name with the highest population
print(Largest_Population)

# AQI in China.
filter(Clean_AQI_Sorted, Country == "China")
China <- filter(Clean_AQI_Sorted, Country == "China") %>%
  arrange(desc(Date))
view(China)

# Create the line plot for China
China_line_plot <- ggplot(China, aes(x = Date, y = AQI)) +
  geom_line(color = "red") +
  labs(title = "AQI Trend in China", x = "Date", y = "AQI")
print(China_line_plot)

# Average AQI in China.
mean(China$AQI)
China_Avg <- mean(China$AQI)

# Worse AQI in China.
max(China$AQI)
China_Max <- max(China$AQI)




# What is the lowest population?
min(Clean_AQI_Sorted$Population_2022)
Min_Population <- min(Clean_AQI_Sorted$Population_2022)


# What country has the lowest population?
Country_With_Min_Population <- Clean_AQI_Sorted %>%
  filter(Population_2022 == min(Population_2022)) %>%
  select(Country)

# Extract the name of the country with the lowest population
Lowest_Population <- Country_With_Min_Population %>%
  distinct()
print(Lowest_Population)


# AQI in Gibraltar.
filter(Clean_AQI_Sorted, Country == "Gibraltar")
Gibraltar <- filter(Clean_AQI_Sorted, Country == "Gibraltar") %>%
  arrange(desc(Date))
view(Gibraltar)

# Average AQI in Gibraltar.
mean(Gibraltar$AQI)
Gibraltar_Avg <- mean(Gibraltar$AQI)
print(Gibraltar_Avg)

# Worse AQI in Gibraltar.
max(Gibraltar$AQI)
Gibraltar_Max <- max(Gibraltar$AQI)
print(Gibraltar_Max)

# Create the line plot for Gibraltar
Gibraltar_line_plot <- ggplot(Gibraltar, aes(x = Date, y = AQI)) +
  geom_line(color = "black") +
  labs(title = "AQI Trend in Gibraltar", x = "Date", y = "AQI")
print(Gibraltar_line_plot)




# AQI in the USA.
filter(Clean_AQI_Sorted, Country == "United States of America")
USA <- filter(Clean_AQI_Sorted, Country == "United States of America")
view(USA)

# Average AQI in the USA.
mean(USA$AQI)
USA_Avg <- mean(USA$AQI)
print(USA_Avg)

# Worse AQI in the USA.
max(USA$AQI)
USA_Max <- max(USA$AQI)
print(USA_Max)

# Create a line plot for the USA.
USA_line_plot <- ggplot(USA, aes(x = Date, y = AQI)) +
  geom_line(color = "blue") +
  labs(title = "AQI Trend in USA", x = "Date", y = "AQI")
print(USA_line_plot)




# I wanted a pivot table that showed the average AQI and population of each country.
pivot_table <- Clean_AQI_Sorted %>%
  group_by(Country) %>%
  summarize(Average_AQI = mean(AQI),
            Population_2022 = first(Population_2022))

pivot_table <- pivot_table %>%
  arrange(desc(Population_2022))
view(pivot_table)



# Using the above pivot table I wanted to create a visualization showcasing the relation between AQI and population.
# Create the scatter plot
scatter_plot <- ggplot(pivot_table, aes(x = Average_AQI, y = Population_2022, color = Country)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +  # Use LOESS method for smoothing
  labs(title = "Population vs. Average AQI by Country", x = "Average AQI", y = "Population") +
  scale_y_continuous(labels = scales::comma)  # Use comma format for y-axis labels
print(scatter_plot)
# The scatter chart was not visually appealing and hard to read.

# Next I tried a bar chart.

# Create a bar chart with countries on the x-axis and average AQI on the y-axis
bar_chart <- ggplot(pivot_table, aes(x = reorder(Country, -Average_AQI), y = Average_AQI)) +
  geom_col() +
  labs(title = "Average AQI by Country", x = "Country", y = "Average AQI") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
print(bar_chart)

# The bar chart was nice to show each country but did not show the population.

# My next choice was a bubble chart.

# Create a bubble chart with colored and sized dots for each country
bubble_chart <- ggplot(pivot_table, aes(x = Population_2022, y = Average_AQI, color = Country, size = Population_2022)) +
  geom_point() +
  labs(title = "Population vs. Average AQI by Country", x = "Population", y = "Average AQI") +
  scale_x_continuous(labels = scales::comma)  # Use comma format for x-axis labels
print(bubble_chart)

# I was getting closer to what I wanted but still did not like how crowded it looked.

# Unhappy with the above examples I decided to export the pivot table to Tableau.
cat("Tableau Visualization: ", "<a href='https://public.tableau.com/app/profile/jayce.hallberg.roblero/viz/Populationvs_AverageAQIbyCountry/Sheet1#1'>Tableau_Visual</a>")


# We can see that even though China has the largest population it does not have the highest AQI. 
# And even though Gibraltar has the lowest population it does not have the lowest average AQI.


# I would love to explore this data further to include weather patters, CO2 levels and create a map showing geographical locations of each country.
