#### Preamble ####
# Purpose: Test the cleaned dataset to see if the data has any abnormality
# Author: Wanling Ma
# Date: 3 April 2024
# Contact: wanling.ma@mail.utoronto.ca
# License: MIT
# Pre-requisites: 02-data_cleaning.R file is executed successfully


#### Workspace setup ####
library(tidyverse)
library(testthat)

#### Test data ####
analysis_data <- read_csv("data/analysis_data/cleaned_data.csv")

# Test 1: Percentages Sum to 100%
test_that("Sum of percentages equals 100 for each row", {
  expect_true(all((analysis_data$Young.Dependents.Percentage +
                  analysis_data$Working.Age.Percentage + 
                  analysis_data$Elderly.Dependents.Percentage - 100) <= 0.0001),
              info = "Percentages do not sum to 100 for all rows.")
})

# Test 2: No Negative Values
test_that("No negative values in dataset", {
  expect_true(all(analysis_data$Young.Dependents.Percentage >= 0) &&
              all(analysis_data$Working.Age.Percentage >= 0) &&
              all(analysis_data$Elderly.Dependents.Percentage >= 0) &&
              all(analysis_data$EnrollmentRate >= 0) &&
              all(analysis_data$FertilityRate >= 0),
              info = "There are negative values in the dataset.")
})

# Test 3: Enrollment and Fertility Rates within realistic ranges
test_that("Enrollment and Fertility Rates are within expected ranges", {
  expect_true(all(analysis_data$EnrollmentRate >= 65 & analysis_data$EnrollmentRate <= 100) &&
              all(analysis_data$FertilityRate >= 0.8 & analysis_data$FertilityRate <= 2.5),
              info = "Enrollment or Fertility Rates are outside of realistic ranges.")
})

# Test 4: Correct Year Sequence
test_that("Year sequence is correct", {
  expect_true(all(diff(analysis_data$Year) == 1),
              info = "There are issues with the Year sequence (gaps or duplications).")
})
