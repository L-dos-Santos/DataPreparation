#install.packages("lubridate")

library(tidyverse)
#library(lubridate)
library(ggplot2)
library(gridExtra)


#reading the dataset
vaccination <- read.csv("~/GitHub/AI_CA2/DataPreparation/country_vaccinations.csv", stringsAsFactors = )
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

#creating a data frame only with numeric variables
df_numeric <- subset(vaccination, select = 
                       c(total_vaccinations,
                         people_vaccinated, 
                         people_fully_vaccinated,
                         daily_vaccinations))
df_numeric

#checking variable with non finite values
any(!is.finite((df_numeric$total_vaccinations)))
any(!is.finite((df_numeric$people_vaccinated)))
any(!is.finite((df_numeric$people_fully_vaccinated)))
any(!is.finite((df_numeric$daily_vaccinations)))


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
normalizeStandardize <- function(x) {
  res <- (x - mean(x)) / sd(x)
  return(res)
}

# Creating a standardized data frame
vaccination_standardized <- as.data.frame(sapply(df_numeric, normalizeStandardize))


p1 <- ggplot(vaccination, aes(x = people_vaccinated_per_hundred)) +
  geom_boxplot(fill = "blue", color = "black", bins = 30) +
  labs(title = "Boxplot of Sepal Length (Original Data)")

p2 <- ggplot(vaccination_minmax, aes(x = people_vaccinated_per_hundred)) +
  geom_boxplot(fill = "green", color = "black", bins = 30) +
  labs(title = "Boxplot of Sepal Length (Min-Max Scaled)")

p3 <- ggplot(vaccination_standardized, aes(x = "", y = daily_vaccinations)) +
  geom_boxplot(fill = "red", color = "black", bins = 30)
  labs(title = "Boxplot of Sepal Length (Standardized)")
  
grid.arrange(p1, p2, p3, nrow = 2, ncol = 2)

#using log10 transformation to normalise
#the distribution

vaccination$daily_vaccinations <- log10(
  vaccination$daily_vaccinations
)

#histogram after normalisation
barplot(vaccination$daily_vaccinations,
        col = "black",        
        border = "skyblue",       
        main = "Daily Vaccinations",  
        xlab = "Categories",    
        ylab = "Counts",        
        ylim = c(0, 400),     
)


#creating avg variables for total of vaccines
#and avg for people vaccinated per country
by_country <- vaccination %>% group_by(country) %>% summarise(
  count = n(),
  avg_total_vaccinations = mean(total_vaccinations),
  avg_people_vaccinated = mean(people_vaccinated)
  ) 
  
by_country

#plot
ggplot(data = by_country, mapping = aes(x = avg_total_vaccinations,
                                      y = avg_people_vaccinated))+
  geom_point(aes(size = count),
             alpha = 1/3) +
  geom_smooth(se = FALSE)

barplot(vaccination$total_vaccinations, 
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
     col = "blue")
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

vaccination %>%
  select(country, date, total_vaccinations,
         people_vaccinated,
         daily_vaccinations) %>%
  drop_na(total_vaccinations) %>%
  view()

df_vaccination %>%
  select(country, date, ends_with("vaccinated")) %>%
  names()



#checking for duplicated values
duplicated(df_vaccination$country)



#checking the type of data 
typeof(df_vaccination$total_vaccinations)
typeof(vaccination$date)
typeof(df_vaccination$daily_vaccinations)

#checking the number of rows
NROW(df_vaccination)


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


df_vaccination$date

