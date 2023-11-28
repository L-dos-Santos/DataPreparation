install.packages("lubridate")
library(lubridate)
library(ggplot2)
library(dplyr)

#reading the dataset
vaccination <- read.csv("country_vaccinations.csv", stringsAsFactors = )
head(vaccination)

# DATA CLEANING

#deleting columns
vaccination[, c('iso_code', 
                   'daily_vaccinations_raw', 
                   'vaccines', 
                   'source_name', 
                   'source_website')] <- list(NULL)
head(vaccination)

#checking missing values
which(is.na(vaccination$country))
which(is.na(vaccination$iso_code))
which(is.na(vaccination$date))
which(is.na(vaccination$total_vaccinations))
which(is.na(vaccination$people_vaccinated))
which(is.na(vaccination$people_fully_vaccinated))
which(is.na(vaccination$daily_vaccinations))

#deleting the missing values
df_vaccination <- na.omit(vaccination)
head(df_vaccination)

#checking for duplicated values
duplicated(df_vaccination)

#checking the type of data 
typeof(df_vaccination$total_vaccinations)
typeof(df_vaccination$date)
typeof(df_vaccination$daily_vaccinations)

#checking the number of rows
NROW(df_vaccination)

#converting from double to integer
as.integer(df_vaccination$daily_vaccinations)
typeof(df_vaccination$daily_vaccinations)
summary(df_vaccination)

# Assuming df_vaccination is your data frame and date is the column to convert
df_vaccination$date <- ymd(df_vaccination$date)

# Check the data type
typeof(df_vaccination$date)

ggplot(df_vaccination, aes(x = date, y = daily_vaccinations)) +
  geom_line(color = "blue") +
  labs(title = "Overall Trend in Daily Vaccinations",
       x = "Date",
       y = "Daily Vaccinations")+
  theme_minimal() + 
  theme(text = element_text(size = 14))
