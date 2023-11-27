#reading the dataset
vaccination <- read.csv("country_vaccinations.csv", stringsAsFactors = )
head(vaccination)

#deleting the missing values
df_vaccination <- na.omit(vaccination)
head(df_vaccination)
