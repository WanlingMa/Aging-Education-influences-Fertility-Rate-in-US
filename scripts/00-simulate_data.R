#### Preamble ####
# Purpose: Simulates the dataset that are used to fit the model used in the paper analysis
# Author: Wanling Ma
# Date: 3 April 2024
# Contact: wanling.ma@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(tidyverse)


#### Simulate data ####

set.seed(1003841257) # Set a seed for reproducibility

# Generate data
n <- 31 # Number of rows

Year <- 1990:(1990 + n - 1)
Young_Dependents_Percentage <- runif(n, min=30, max=35)
Working_Age_Percentage <- runif(n, min=50, max=53)
Elderly_Dependents_Percentage <- 100 - Young_Dependents_Percentage - Working_Age_Percentage
EnrollmentRate <- runif(n, min=70, max=90)
FertilityRate <- runif(n, min=1.6, max=2.1)

# Combine into a data frame
simulated_data <- data.frame(Year, Young_Dependents_Percentage, Working_Age_Percentage, 
                             Elderly_Dependents_Percentage, EnrollmentRate, FertilityRate)

# Print the simulated data
print(simulated_data)



