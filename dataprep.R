library(tidyverse)
library(ggplot2)
library(gridExtra)
#library(cowplot)
library(robustbase)




install.packages("robustbase")
#install.packages("cowplot")


#reading the dataset
vaccination <- read.csv("country_vaccinations.csv", stringsAsFactors = FALSE)


# *------ DATA CLEANING -----*

#deleting columns
vaccination[, c('iso_code', 
                   'daily_vaccinations_raw',
                   'vaccines',
                   'source_name', 
                   'source_website')] <- list(NULL)
head(vaccination)

#Check data types
print(sapply(vaccination, class))

#checking missing values
which(is.na(vaccination$country))
which(is.na(vaccination$date))
which(is.na(vaccination$total_vaccinations))
which(is.na(vaccination$people_vaccinated))
which(is.na(vaccination$people_fully_vaccinated))
which(is.na(vaccination$daily_vaccinations))

#checking for duplicated values
duplicated(vaccination)

#replacing the missing values for 0
vaccination[is.na(vaccination)] <- 0

#*----- CALCULATING MEAN ------*
mean(vaccination$total_vaccinations)
mean(vaccination$people_vaccinated)
mean(vaccination$people_fully_vaccinated)
mean(vaccination$daily_vaccinations)
mean(vaccination$total_vaccinations_per_hundred)
mean(vaccination$people_vaccinated_per_hundred)
mean(vaccination$people_fully_vaccinated_per_hundred)
mean(vaccination$daily_vaccinations_per_million)

# *------ DATA TRANSFORMATION -----*

#transforming character to Date
vaccination$date <- as.Date(vaccination$date)


#[,3:8] means that we are selecting from the 3th column to the 8th
#vaccination_minmax <- as.data.frame(sapply(vaccination[,3:8], normalizeMinMax))
#summary(vaccination_minmax)

#min-max normalization
normalizeMinMax <- function (x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

daily_vac_norm<-normalizeMinMax(vaccination$daily_vaccinations)
#total_vac_norm<-normalizeMinMax(vaccination$total_vaccinations)

# Standardize the numeric columns
z_score <- function(x) {
  res <- (x - mean(x)) / sd(x)
  return(res)
}

vaccination_z_score <- as.data.frame(sapply(vaccination[,3:8], z_score))
summary(vaccination_z_score)
sapply(vaccination_z_score,sd)

#using log10 transformation to normalize
#the distribution
vaccination_log <- log10(
  vaccination$daily_vaccinations + 1e-10)
summary(vaccination_log)


# Normalize the data using the Robust Scalar
daily_vac_robust <- 
  (vaccination$daily_vaccinations - median(
    vaccination$daily_vaccinations)) / mad(
      vaccination$daily_vaccinations)




p1 <- ggplot(vaccination, aes(x = daily_vaccinations)) +
  geom_histogram(fill = "blue2", color = "black") +
  labs(title = "Histogram of Original Daily Vaccinations")

p2 <- ggplot(vaccination, aes(x = daily_vac_norm)) +
  geom_histogram(fill = "green2", color = "black") +
  labs(title = "Histogram of Min-Max Scaled Daily Vaccinations")

p3 <- ggplot(vaccination_z_score, aes(x = daily_vaccinations)) +
  geom_histogram(fill = "red2", color = "black") +
  labs(title = "Histogram of Standardized Daily Vaccinations")

p4 <- ggplot(vaccination, aes(x = log10(daily_vaccinations + 1e-10))) +
  geom_histogram(fill = "yellow2", color = "black") +
  labs(title = "Histogram of Log-Transformed Daily Vaccinations",
       x = "Log(Daily Vaccinations)")
p5 <- ggplot(vaccination, aes(x = daily_vac_robust)) +
  geom_histogram(fill = "pink", color = "black") +
  labs(title = "Histogram of Robust Scalar",
       x = "Robust Scalar(Daily Vaccinations)")
  
grid.arrange(p1, p2, p3, p4, p5, nrow = 3, ncol = 3)


#creating avg variables for total of vaccines
#and avg for people vaccinated per country
by_country <- vaccination %>% group_by(country) %>% summarise(
  avg_total_vaccinations = mean(total_vaccinations),
  avg_people_vaccinated = mean(people_vaccinated)
  ) 
  
by_country

#filtering the countries where the avarage of total vaccinations and people vaccinated
#are <5000
filtered_country <- filter(by_country, 
                           avg_total_vaccinations <5000 & avg_people_vaccinated < 5000)
print(filtered_country)

#box plot to identify outliers
boxplot(filtered_country$avg_total_vaccinations, 
        filtered_country$avg_people_vaccinated,
        main = "Box Plot with Outliers", 
        names = c("Avarage of Total Vaccinations", 
                  "Avarage of People Vaccinated"))

#selecting the top 5 countries
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

#dummy encoding to country variable
encoded_data <- model.matrix(
  ~ country - 1,
  data = vaccination
)
final_data <- cbind(vaccination, encoded_data)
print(final_data [, 1:15])
