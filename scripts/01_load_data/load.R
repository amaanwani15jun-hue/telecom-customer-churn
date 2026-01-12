library(tidyverse)
library(here)

raw_data <- read.csv(here("data/raw" , "customer_churn.csv"))
view(raw_data)
