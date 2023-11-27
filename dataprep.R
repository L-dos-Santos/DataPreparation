#reading the dataset
vaccination <- read.csv("country_vaccinations.csv", stringsAsFactors = )
head(vaccination)

#checking missing values
which(is.na(vaccination$country))
which(is.na(vaccination$iso_code))
which(is.na(vaccination$date))
which(is.na(vaccination$total_vaccinations))
which(is.na(vaccination$people_vaccinated))
which(is.na(vaccination$people_fully_vaccinated))


#deleting the missing values
df_vaccination <- na.omit(vaccination)
head(df_vaccination)


