# Load dataset and libraries
library(tidyverse)
library(skimr)
raw_data <- read.csv("data/raw/telecom_churn_data.csv")

# 1. First look at data structure and sample values
head(raw_data)

# 2. Check end of dataset for consistency
tail(raw_data)

# 3. Understand dataset size: rows = observations, columns = features
dim(raw_data)

# 4. Detailed structure: column names, data types, initial values
str(raw_data)

# 5. List all available features/variables
names(raw_data)

# 6. Identify missing values - TotalCharges has 11 NAs to handle
colSums(is.na(raw_data))

# 7. Check for exact duplicate rows (should be 0 in clean data)
sum(duplicated(raw_data))

# 8. Count unique rows to compare with total rows
n_distinct(raw_data)

# 9. Basic statistical summary of all variables
summary(raw_data)

# 10. Comprehensive data profile with visual distribution indicators
skim(raw_data)

# 11. Explore specific categorical feature: PaymentMethod
length(unique(raw_data$PaymentMethod))