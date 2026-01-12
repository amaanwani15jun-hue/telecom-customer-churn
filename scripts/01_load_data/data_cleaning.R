# Load required libraries for data manipulation and cleaning
library(tidyverse)
library(janitor)
library(here)

# Clean column names to snake_case and remove all rows with missing values
cleaned_data <- raw_data |> 
  clean_names() |> 
  drop_na()

# Save the cleaned data to CSV file in the processed folder
write.csv(cleaned_data, 
          here("data/processed", "clean_data.csv"), 
          row.names = FALSE)

# Reload cleaned data and convert binary categorical variables to numeric (1/0)
cleaned_data <- read.csv(here("data/processed", "clean_data.csv")) |> 
  mutate(
   
    gender = ifelse(gender == "Male", 1, 0),
    partner = ifelse(partner == "Yes", 1, 0),
    dependents = ifelse(dependents == "Yes", 1, 0),
    phone_service = ifelse(phone_service == "Yes", 1, 0),
    online_security = ifelse(online_security == "Yes", 1, 0),
    online_backup = ifelse(online_backup == "Yes", 1, 0),
    device_protection = ifelse(device_protection == "Yes", 1, 0),
    tech_support = ifelse(tech_support == "Yes", 1, 0),
    streaming_tv = ifelse(streaming_tv == "Yes", 1, 0),
    streaming_movies = ifelse(streaming_movies == "Yes", 1, 0),
    paperless_billing = ifelse(paperless_billing == "Yes", 1, 0),
    churn = ifelse(churn == "Yes", 1, 0)
  )

# Preview first 6 rows to check data structure
head(cleaned_data)

# Preview last 6 rows to ensure data consistency
tail(cleaned_data)

# Check dataset dimensions (rows and columns)
dim(cleaned_data)

# Examine data structure including variable types
str(cleaned_data)

# List all column names
names(cleaned_data)

# Check for any remaining missing values
colSums(is.na(cleaned_data))

# Count duplicate rows
sum(duplicated(cleaned_data))

# Count unique rows in the dataset
n_distinct(cleaned_data)

# Generate summary statistics for all variables
summary(cleaned_data)

# Create comprehensive data profile with skimr
skim(cleaned_data)

# Count unique payment methods
length(unique(cleaned_data$payment_method))

