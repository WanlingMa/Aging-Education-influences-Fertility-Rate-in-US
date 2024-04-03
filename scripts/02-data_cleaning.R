#### Preamble ####
# Purpose: Cleaned raw dataset that are used to fit the model used in the paper analysis
# Author: Wanling Ma
# Date: 3 April 2024
# Contact: wanling.ma@mail.utoronto.ca
# License: MIT
# Pre-requisites: Download the dataset according to the 01-download_data.R

#### Workspace setup ####
library(tidyverse)

#### Clean data ####
## ==== Clean Age Distribution Dataset ====
# Read the CSV file into a dataframe
data <- read.csv("data/raw_data/age-distribution.csv")

# Filter the data for the United States and years 1990 to 2020
filtered_data <- data %>%
  filter(Country.name == "United States" & Year >= 1990 & Year <= 2020) %>%
  mutate(X65..years = ifelse(is.na(`X65..years`), 0, `X65..years`),
         X25.64.years = ifelse(is.na(`X25.64.years`), 0, `X25.64.years`),
         X15.24.years = ifelse(is.na(`X15.24.years`), 0, `X15.24.years`),
         X5.14.years = ifelse(is.na(`X5.14.years`), 0, `X5.14.years`),
         Under.5s = ifelse(is.na(Under.5s), 0, Under.5s)) %>%
  mutate(Young.Dependents = `X5.14.years` + `X15.24.years` + Under.5s,
         Working.Age = `X25.64.years`,
         Elderly.Dependents = `X65..years`) %>%
  select(Country.name, Year, Young.Dependents, Working.Age, Elderly.Dependents) %>%
  pivot_longer(cols = c("Young.Dependents", "Working.Age", "Elderly.Dependents"), 
               names_to = "AgeGroup", values_to = "Population")

# Calculate the total population and percentage for the new categories
filtered_data <- filtered_data %>%
  group_by(Year) %>%
  mutate(Total.Population = sum(Population)) %>%
  ungroup() %>%
  mutate(Percentage = (Population / Total.Population) * 100)

age_distribution_data <- filtered_data %>%
  group_by(Year) %>%
  summarize(Young.Dependents.Percentage = sum(Percentage[AgeGroup == "Young.Dependents"]),
            Working.Age.Percentage = sum(Percentage[AgeGroup == "Working.Age"]),
            Elderly.Dependents.Percentage = sum(Percentage[AgeGroup == "Elderly.Dependents"])) %>%
  ungroup()

## ==== Clean Enrollment Rate Dataset ====
# Read the new dataset into a dataframe
education_data <- read.csv("data/raw_data/gross-enrollment-ratio-in-tertiary-education.csv")

# Filter the dataset for the United States and the years 1990 to 2020
us_education_data <- education_data %>%
  filter(Entity == "United States" & Year >= 1990 & Year <= 2020) %>%
  select(Year, EnrollmentRate = `School.enrollment..tertiary....gross.`)

# Interpolate data to fill 1990 to 2020 for further usage
us_education_data <- us_education_data[order(us_education_data$Year),]
complete_years <- data.frame(Year = 1990:2020)
full_dataset <- merge(complete_years, us_education_data, by = c("Year"), all.x = TRUE)
full_dataset$EnrollmentRate <- na.approx(full_dataset$EnrollmentRate, na.rm = FALSE)
us_education_data <- full_dataset


## ==== Clean Fertility Rate Dataset ====
# Read the dataset into a dataframe
fertility_data <- read.csv("data/raw_data/fertility-rate.csv")

# Filter for the United States and select relevant columns using the modified column names
us_fertility_data <- fertility_data %>%
  filter(`Country.name` == "United States" & Year >= 1990 & Year <= 2020) %>%
  select(Year, `Fertility.rate..children.per.woman`)


## ==== Merge 3 Dataset ====
# Merging the datasets on 'Year'
combined_data <- merge(x = age_distribution_data, y = us_education_data, by = "Year")
combined_data <- merge(x = combined_data, y = us_fertility_data, by = "Year", all.x = TRUE)

combined_data <- combined_data %>%
  rename(FertilityRate = `Fertility.rate..children.per.woman`,
         EnrollmentRate = EnrollmentRate)


#### Save data ####
write_csv(combined_data, "data/analysis_data/cleaned_data.csv")

