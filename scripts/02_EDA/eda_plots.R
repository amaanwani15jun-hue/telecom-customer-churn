# Set consistent theme for ALL plots
library(tidyverse)
library(ggplot2)
library(viridis)
library(patchwork)# For arranging multiple plots
library(here)
cleaned_data <- read.csv(here("data/processed" , "clean_data.csv"))
# Custom theme for publication quality
theme_custom <- theme_classic() +
  theme(
    text = element_text(family = "sans", color = "black"  ,  size = 11),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray50", hjust = 0.5),
    axis.title = element_text(size = 11, face = "plain"),
    axis.text = element_text(size = 9 , color = "grey30"),
    legend.position = "bottom" , 
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold" ,size = 11),
    legend.background = element_rect(fill = "white" , color = NA) , 
    panel.grid.major = element_line(color = "grey92" , linewidth = 0.3),
    panel.grid.minor = element_blank()
  )


# 1. CHURN DISTRIBUTION - Global churn rate
plot_1 <- cleaned_data %>%
  mutate(churn_label = ifelse(churn == 1, "Yes", "No")) %>%
  ggplot(aes(x = churn_label , fill = churn_label)) +
  geom_bar( color = "black") +
  geom_text(stat = 'count', aes(label = after_stat(count) ), 
            vjust = -0.7, size = 3.5  , 
            , fontface = "bold") +
  labs(
    title = "Customer Churn Distribution Shows Significant Class Imbalance",
    subtitle = "Only 26.5% of customers churned - Will require careful modeling approach",
    x = "Churn Status",
    y = "Number of Customers",
    fill = NULL, 
    caption = "Source: Telecom Churn Dataset | n = 7,043 customers"
  ) +
  scale_fill_viridis_d() +
  theme_custom
ggsave("CHURN_DISTRIBUTION.png" , 
        plot = plot_1,
        width = 16, height = 9, units = "in")

# plot2
 mean_value <-  round(mean(cleaned_data$tenure) , 1)
median_value <- round(median(cleaned_data$tenure) , 1)
label_1 <-  paste0("Mean: " , mean_value ," months")
label_2 <-  paste0("Median: " , median_value ," months")
plot_2 <- ggplot(data = cleaned_data ,  aes(x = tenure)) +
  geom_histogram(color = "black" , fill = "#FFB000" , bins = 15 ) +
  geom_text(stat = "bin" , aes(label = after_stat(count)) , 
            vjust = -0.7 , size = 2.7 , 
            fontface = "bold" , 
            angle = 17 , 
            bins = 15) +
   labs(
    title = "Customer Tenure Distribution Shows Most Customers are Recent",
    subtitle = "Distribution shows many new customers ",
    y = "Number of Customers",
    x = "Tenure (Months)",
    fill = NULL, 
    caption = "Data shows customer lifecycle duration in the telecom business"
  ) +
   geom_vline(xintercept = mean_value, color = "red", linetype = "dashed") +
   geom_vline(xintercept = median_value, color = "blue") +
   annotate("text", x = mean_value, y = Inf, label = label_1, 
            vjust = 2, hjust = -0.1, color = "red") +
   annotate("text", x = median_value, y = Inf, label = label_2 , 
            vjust = 2, hjust = 1.1, color = "blue") +
   theme_custom
ggsave("tenure_distribution.png" , 
       plot = plot_2 , 
       width = 16, height = 9, units = "in"
       )


# plot3
mean_monthly <-  round(mean(cleaned_data$monthly_charges) , 1)
median_monthly <- round(median(cleaned_data$monthly_charges) , 1)
label_1 <-  paste0("Mean: " , mean_monthly ," $")
label_2 <-  paste0("Median: " , median_monthly ," $")
plot_3 <- ggplot(data = cleaned_data ,  aes(x = monthly_charges)) +
  geom_histogram(color = "black"  , bins = 20 , 
                 aes(fill = internet_service)) +
  geom_text(stat = "bin" , aes(label = after_stat(count)) , 
            vjust = -0.7 , size = 2.7 , 
            fontface = "bold" , 
            angle = 17 , 
            bins = 20) +
  labs(
    title = "Monthly Charges not bimodal ",
    subtitle = "Customers cluster around 
    below(~$28) and between ($ 75--90) price points, indicating different service tiers",
    y = "Number of Customers",
    x = "Monthly Charges ($)",
    fill = "Service Type",
    caption = "Data reveals pricing strategy segmentation and customer spending behavior"
  ) +
  geom_vline(xintercept = mean_monthly, color = "red", linetype = "dashed") +
  geom_vline(xintercept = median_monthly, color = "blue") +
  annotate("text", x = mean_monthly -15, y = Inf, label = label_1, 
           vjust = 2, hjust = -0.1, color = "red") +
  annotate("text", x = median_monthly +15, y = Inf, label = label_2 , 
           vjust = 2, hjust = 1.1, color = "blue") +
  scale_fill_viridis_d() +
  theme_custom


ggsave("monthly_charges_distribution.png" , 
       plot = plot_3 , 
       width = 16, height = 9, units = "in")
# Check if bimodal
bimodal_check <- cleaned_data %>%
  summarise(
    low_tier = sum(monthly_charges < 50) / n() * 100,
    high_tier = sum(monthly_charges >= 50) / n() * 100,
    is_bimodal = abs(mean_monthly - median_monthly) > 10  
  )
bimodal_check
# plot_4 

plot_4 <- ggplot(data = cleaned_data ,  aes(x = contract)) +
  geom_bar(color = "black"  , 
                 aes(fill = contract ) , show.legend = FALSE) +
  geom_text(stat = "count" , 
            aes(label =paste(round(after_stat(count)/sum(after_stat(count)) *100 , 1) , "%")) , 
            vjust = -0.7 , size = 3.5 , 
            fontface = "bold"  ) +
  labs(
    title = "Contract Type Distribution Reveals Customer Commitment Levels",
    subtitle = "Majority of customers (55%) prefer month-to-month flexibility",
    y = "Number of Customers",
    x = "Type of Contract",
    fill = NULL,
    caption = "Understanding contract preferences helps in retention strategy planning"
  ) +
  scale_fill_manual(values = c("Month-to-month" = "#EF476F", 
                               "One year" = "#FFD166",         
                               "Two year" = "#06D6A0")) +
  theme_custom




ggsave("contract_distribution.png" , 
       plot = plot_4 , 
       width = 16, height = 9, units = "in")


# plot_5 
plot_data <-  cleaned_data |> 
  count(contract , churn) |> 
  group_by(contract) |> 
  mutate(prop = n/sum(n)) |> ungroup()
plot_5 <- ggplot(plot_data , aes(x= contract , y = prop,  fill = factor(churn)) )+
  geom_col(color = "black" , position = "fill") +
  geom_text(aes(label = percent(prop, accuracy = 1)), 
               position = position_fill(vjust = 0.5), 
               color = "white",                       
               fontface = "bold") +
  labs(
    title = "Month-to-Month Contracts Have Significantly Higher Churn Risk",
    subtitle = "Churn rate is 43% for month-to-month vs only 3% for two-year contracts",
    y = "Proportion of Customers",
    x = "Contract Type",
    fill = "Churn",
    caption = "Long-term contracts reduce churn by 4× - crucial for retention strategy"
  ) +
  scale_y_continuous(labels = percent) +
  scale_fill_discrete(labels = c("No", "Yes")) +
  theme_custom
 

ggsave("bivar_contract_vs_churn_01.png" , 
       plot = plot_5, 
       width = 16, height = 9, units = "in")
# plot_6
plot_data2 <-  cleaned_data |> 
  count(payment_method , churn) |> 
  group_by(payment_method) |> 
  mutate(prop = n/sum(n)) |> ungroup()
plot_6 <- ggplot(plot_data2 , aes(x= payment_method , y = prop,  fill = factor(churn)) )+
  geom_col(color = "black" , position = "fill") +
  geom_text(aes(label = percent(prop, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            color = "white",                       
            fontface = "bold") +
  labs(
    title = "Electronic Check Users Show Highest Churn Vulnerability",
    subtitle = "Converting check users to automatic
    payments could prevent 1 in 3 potential churn cases",
    y = "Proportion of Customers",
    x = "Payment Method",
    fill = "Churn",
    caption = "Operational insight: Friction in
    payment process directly impacts customer retention"
  ) +
  scale_y_continuous(labels = percent) +
  scale_fill_discrete(labels = c("No", "Yes")) +
  theme_custom


ggsave("bivar_payment_vs_churn_02.png" , 
       plot = plot_6, 
       width = 16, height = 9, units = "in")


# plot7 

plot_7a <- ggplot(cleaned_data , aes(x = factor(churn) , y =tenure , fill=factor(churn))) +
  geom_violin(alpha= 0.4) +
  geom_boxplot(width = 0.2, color = "black", outlier.shape = NA) +
  labs(
    title = "Customers Typically Churn Within the First Year",
    subtitle = "Median tenure for churned users is significantly lower than retained users",
    x = "Churn Status",
    y = "Tenure (Months)",
    fill = "Churned?" , 
    caption = "Tenure is a huge indicator of churn"
  ) +
  scale_x_discrete(labels = c("0" = "Stayed", "1" = "Churned"))+
  scale_fill_manual(values = c("0" = "#648FFF", "1" = "#FFB000"), labels = c("No", "Yes")) +
  theme_custom
ggsave("bivar_tenure_vs_churn_03_a.png" , 
       plot = plot_7a , 
       width = 16 , height = 9 , 
       units = "in")



# plot_7b
plot_7b <- ggplot(cleaned_data , aes(x = tenure ,  fill=factor(churn))) +
 
  geom_histogram(color = "black") +
  labs(
    title = "Customers Typically Churn Within the First Year",
    subtitle = "typically after 24 months churn tends to be very low",
    x = "Churn Status",
    y = "freq",
    fill = "Churned?" , 
    caption = "Tenure is a huge indicator of churn"
  ) +
 
  scale_fill_manual(values = c("0" = "#648FFF", "1" = "#FFB000"), labels = c("No", "Yes")) +
  theme_custom
ggsave("bivar_tenure_vs_churn_03_b.png" , 
       plot = plot_7b , 
       width = 16 , height = 9 , 
       units = "in")


# plot_8a
plot_8a <- ggplot(cleaned_data , aes(x = factor(churn) , y =monthly_charges , fill=factor(churn))) +
  geom_violin(alpha= 0.4) +
  geom_boxplot(width = 0.2, color = "black", outlier.shape = NA) +
  labs(
    title = "Higher Monthly Bills Increase Churn Risk, Especially for New Customers",
    subtitle = "Median monthly charges for churned users is  higher
    than retained users",
    x = "Churn Status",
    y = "Monthy charges in $",
    fill = "Churned?" , 
    caption = "Monthly charge is a huge indicator of churn"
  ) +
  scale_x_discrete(labels = c("0" = "Stayed", "1" = "Churned"))+
  scale_fill_manual(values = c("0" = "#648FFF", "1" = "#FFB000"), labels = c("No", "Yes")) +
  theme_custom
ggsave("bivar_monthly_vs_churn_04_a.png" , 
       plot = plot_8a , 
       width = 16 , height = 9 , 
       units = "in")


# plot_8b
plot_8b <- ggplot(cleaned_data , aes(x = monthly_charges ,  fill=factor(churn))) +
  
  geom_histogram(color = "black") +
  labs(
    title = "Customers Typically Churn Within the First Year",
    subtitle = "People begin to churn more when mothly charge > 65$",
    x = "Churn Status",
    y = "Freq",
    fill = "Churned?" , 
    caption = "Monthly Charge is a huge indicator of churn"
  ) +
  
  scale_fill_manual(values = c("0" = "#648FFF", "1" = "#FFB000"), labels = c("No", "Yes")) +
  theme_custom
ggsave("bivar_monthly_vs_churn_04_b.png" , 
       plot = plot_8b , 
       width = 16 , height = 9 , 
       units = "in")
