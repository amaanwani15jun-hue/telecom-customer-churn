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

