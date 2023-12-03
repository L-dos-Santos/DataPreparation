#install.packages("lubridate")

library(tidyverse)
#library(lubridate)
library(ggplot2)
library(gridExtra)


#reading the dataset
vaccination <- read.csv("country_vaccinations.csv", stringsAsFactors = TRUE )
head(vaccination)

# *------ DATA CLEANING -----*

#deleting columns
vaccination[, c('iso_code', 
                   'daily_vaccinations_raw', 
                   'vaccines', 
                   'source_name', 
                   'source_website')] <- list(NULL)
head(vaccination)

# Check data types
print(sapply(vaccination, class))

#checking missing values
which(is.na(vaccination$country))
which(is.na(vaccination$date))
which(is.na(vaccination$total_vaccinations))
which(is.na(vaccination$people_vaccinated))
which(is.na(vaccination$people_fully_vaccinated))
which(is.na(vaccination$daily_vaccinations))

#replacing the missing values for 0
vaccination[is.na(vaccination)] <- 0

# *------ DATA TRANSFORMATION -----*


#analising the daily vaccinations
hist(vaccination$daily_vaccinations)

#transforming character to Date
vaccination$date <- as.Date(vaccination$date, format = "%d-%m-%Y")


#min-max normalization
normalizeMinMax <- function (x) {
  res <- (x - min(x)) / (max(x) - min(x))
  return(res)
}

#creating a data frame with the numeric values
#[,3:8] means that we are selecting from the 3th column to the 8th
vaccination_minmax <- as.data.frame(sapply(vaccination[,3:8], normalizeMinMax))
summary(vaccination_minmax)

# Standardize the numeric columns
z_score <- function(x) {
  res <- (x - mean(x)) / sd(x)
  return(res)
}

# Creating a z score standardization data frame
vaccination_z_score <- as.data.frame(sapply(vaccination[,3:8], z_score))
summary(vaccination_z_score)
sapply(vaccination_z_score,sd)

#using log10 transformation to normalise
#the distribution
vaccination_log <- log10(
  vaccination$daily_vaccinations
)
summary(vaccination_log)

p1 <- ggplot(vaccination, aes(x = daily_vaccinations)) +
  geom_histogram(fill = "blue", color = "black", bins = 30) +
  labs(title = "Boxplot of Sepal Length (Original Data)")

p2 <- ggplot(vaccination_minmax, aes(x = daily_vaccinations)) +
  geom_histogram(fill = "lightgreen", color = "black", bins = 30) +
  labs(title = "Boxplot of Sepal Length (Min-Max Scaled)")

p3 <- ggplot(vaccination_z_score, aes(x = daily_vaccinations)) +
  geom_histogram(fill = "red", color = "black", bins = 30) +
  labs(title = "Boxplot of Sepal Length (Standardized)")

p4 <- ggplot(vaccination, aes(x = daily_vaccinations)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Histogram of Log-Transformed Daily Vaccinations",
       x = "Log(Daily Vaccinations)",
       y = "Frequency")

  
grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)



#creating avg variables for total of vaccines
#and avg for people vaccinated per country
by_country <- vaccination %>% group_by(country) %>% summarise(
  count = n(),
  avg_total_vaccinations = mean(total_vaccinations),
  avg_people_vaccinated = mean(people_vaccinated)
  ) 
  
by_country

#filter to find Argentina
argentina <- filter(vaccination, country == "Argentina")

# Display the filtered data
print(argentina)

#plot
ggplot(data = by_country, mapping = aes(x = avg_total_vaccinations,
                                      y = avg_people_vaccinated))+
  geom_point(aes(size = count),
             alpha = 1/3) +
  geom_smooth(se = FALSE)

plot(vaccination$total_vaccinations, 
     breaks = 30,
     xlim = c(0, 5000),
     col = "purple",
     border = "black",
     ylim = c(0, 400),
     xlab = "Total of Vaccinations",
     ylab = "Counts",
     main = "Vaccination in Argentina")
box(which = "plot",
    lty = "solid",
    col = "black")

select(vaccination, country, date, daily_vaccinations_per_million)

plot(vaccination$daily_vaccinations_per_million,
     vaccination$people_vaccinated_per_hundred,
     xlim = c(0, 5000),
     ylim = c(0, 600),
     xlab = "Daily Vaccinations Per Million",
     ylab = "Date",
     main = "",
     type = "p",
     pch = 16,
     col = "lightpink")
points(Argentina$daily_vaccinations_per_million,
       Argentina$people_vaccinated_per_hundred,
       type = "p",
       col = "black")

top_countries <- vaccination %>%
  arrange(desc(total_vaccinations)) %>%
  distinct(country, .keep_all = TRUE) %>%
  head(5)

# Print the result
print(top_countries)

#plot with top 5 countries
ggplot(top_countries, aes(x = reorder(country, -total_vaccinations), y = total_vaccinations)) +
  geom_bar(stat = "identity", fill = "orange", color = "black") +
  labs(title = "Top 5 Countries with Highest Total Vaccinations",
       x = "Country",
       y = "Total Vaccinations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


vaccination %>%
  select(country, date, total_vaccinations,
         people_vaccinated,
         daily_vaccinations) %>%
  drop_na(total_vaccinations) %>%
  view()

vaccination %>%
  select(country, date, ends_with("vaccinated")) %>%
  names()



#checking for duplicated values
duplicated(vaccination$country)



#checking the type of data 
typeof(vaccination$total_vaccinations)
typeof(vaccination$date)
typeof(vaccination$daily_vaccinations)

#checking the number of rows
NROW(vaccination)


as.Date(vaccination$date)
typeof(vaccination$date)
summary(vaccination)



ggplot(vaccination, aes(x = Argentina, y = daily_vaccinations)) +
  geom_line(color = "blue") +
  labs(title = "Overall Trend in Daily Vaccinations",
       x = "Argentina",
       y = "Daily Vaccinations")+
  theme_minimal() + 
  theme(text = element_text(size = 14))


vaccination$date

