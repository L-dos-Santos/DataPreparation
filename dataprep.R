#install.packages("lubridate")

library(tidyverse)
#library(lubridate)
library(ggplot2)


#reading the dataset
vaccination <- read.csv("country_vaccinations.csv", stringsAsFactors = )
head(vaccination)
#glimpse(vaccination)


# *------ DATA CLEANING -----*

#deleting columns
vaccination[, c('iso_code', 
                   'daily_vaccinations_raw', 
                   'vaccines', 
                   'source_name', 
                   'source_website')] <- list(NULL)
head(vaccination)

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

# Check data types
print(sapply(vaccination, class))

#min-max normalisation
normalizeMinMax <- function (x) {
return((x - min(x)) / (max(x) - min(x)))
}

vaccination_minmax <- vaccination
vaccination_minmax[, 1:4] <- apply(vaccination[, 1:4],
                                   2, normalizeMinMax)
  #*Error in x - min(x) : non-numeric argument to binary operator*

  
#using log10 transformation to normalise
#the distribution
vaccination$daily_vaccinations <- log10(
  vaccination$daily_vaccinations
)

#histogram after normalisation
hist(vaccination$daily_vaccinations)

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

hist(vaccination$total_vaccinations, 
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
points(argentina$daily_vaccinations_per_million,
       argentina$people_vaccinated_per_hundred,
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
typeof(df_vaccination$date)
typeof(df_vaccination$daily_vaccinations)

#checking the number of rows
NROW(df_vaccination)

#converting from double to integer
as.integer(df_vaccination$daily_vaccinations)
typeof(df_vaccination$daily_vaccinations)
summary(df_vaccination)

class(df_vaccination$date)
df_vaccination$date <- as.Date(df_vaccination$date)

# Check the data type
typeof(df_vaccination$date)

ggplot(vaccination, aes(x = argentina, y = daily_vaccinations)) +
  geom_line(color = "blue") +
  labs(title = "Overall Trend in Daily Vaccinations",
       x = "Argentina",
       y = "Daily Vaccinations")+
  theme_minimal() + 
  theme(text = element_text(size = 14))


df_vaccination$date
