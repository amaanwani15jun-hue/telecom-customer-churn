# Set consistent theme for ALL plots
library(ggplot2)
library(viridis)
library(patchwork)  # For arranging multiple plots

# Custom theme for publication quality
theme_custom <- theme_classic() +
  theme(
    text = element_text(family = "sans", color = "grey20"  , size = 11),
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
  geom_text(stat = 'count', aes(label = after_stat(count)), 
            vjust = -0.7, size = 3.5) +
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

