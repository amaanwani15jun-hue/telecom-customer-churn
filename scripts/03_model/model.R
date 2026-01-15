# scripts/03_logistic_regression.R
# ================================

# Load libraries
library(tidyverse)
library(caret)       # For modeling tools
library(ROCR)        # For ROC curves
library(pROC)        # For AUC
library(ggrepel)     # For nice labels
library(here)
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


# Load your cleaned data
plotting_data <- read.csv(here("data/processed" , "clean_data.csv"))



# ============================================
# STEP 1: PREPARE DATA FOR MODELING
# ============================================

# Remove customer ID (identifier, not a feature)
model_data <- plotting_data %>%
  select(-customer_id)  # Adjust to your column name

# Convert target to factor (required by caret)
model_data <- model_data %>%
  mutate(churn = factor(churn, levels = c(0, 1), labels = c("No", "Yes")))
model_data <- model_data %>%
  mutate(across(where(is.character), as.factor))
# Check structure
str(model_data)

# ============================================
# STEP 2: TRAIN/TEST SPLIT (80/20)
# ============================================

set.seed(123)  # For reproducibility

# Create indices for split
train_index <- createDataPartition(model_data$churn, 
                                   p = 0.8, 
                                   list = FALSE, 
                                   times = 1)

# Split data
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Check split proportions
cat("Training set:", nrow(train_data), "rows (", 
    round(nrow(train_data)/nrow(model_data)*100, 1), "%)\n")
cat("Test set:", nrow(test_data), "rows (", 
    round(nrow(test_data)/nrow(model_data)*100, 1), "%)\n")

# Check churn distribution in each set
cat("\nChurn distribution - Training:\n")
print(prop.table(table(train_data$churn)))

cat("\nChurn distribution - Test:\n")
print(prop.table(table(test_data$churn)))

# ============================================
# STEP 3: HANDLE CLASS IMBALANCE (OPTIONAL)
# ============================================

# Since churn is ~26%, we have imbalance. Options:

# OPTION A: Use class weights (simpler)
model_weights <- ifelse(train_data$churn == "Yes",
                        1/table(train_data$churn)[2],  # Weight for minority class
                        1/table(train_data$churn)[1])  # Weight for majority class

# OPTION B: Oversample minority class (SMOTE)
# library(DMwR)
# train_balanced <- SMOTE(churn ~ ., data = train_data, perc.over = 200)

# For now, let's use weights
train_data$model_weights <- model_weights

# ============================================
# STEP 4: PREPROCESSING (SCALE NUMERIC FEATURES)
# ============================================

# Identify numeric columns (excluding target)
numeric_cols <- train_data %>%
  select(where(is.numeric), -contains("churn"), -contains("weight")) %>%
  names()

# Create preprocessing recipe
preproc_values <- preProcess(train_data[, numeric_cols], 
                             method = c("center", "scale"))

# Apply to both train and test
train_scaled <- predict(preproc_values, train_data)
test_scaled <- predict(preproc_values, test_data)

# ============================================
# STEP 5: TRAIN LOGISTIC REGRESSION MODEL
# ============================================

# Simple formula (adjust based on your features)
# Use all columns except weights
features <- setdiff(names(train_scaled), c("churn", "model_weights"))
formula <- as.formula(paste("churn ~", paste(features, collapse = " + ")))

# Train model with 10-fold cross-validation
set.seed(123)
ctrl <- trainControl(
  method = "cv",           # Cross-validation
  number = 10,             # 10 folds
  classProbs = TRUE,       # For probability predictions
  summaryFunction = twoClassSummary,  # For AUC, Sensitivity, Specificity
  savePredictions = "final"
)

# Fit logistic regression
logit_model <- train(
  formula,
  data = train_scaled,
  method = "glm",          # Generalized Linear Model (logistic)
  family = "binomial",
  trControl = ctrl,
  weights = model_weights,  # Address class imbalance
  metric = "ROC"           # Optimize for AUC
)

# View model summary
print(logit_model)
summary(logit_model$finalModel)

# ============================================
# STEP 6: MAKE PREDICTIONS ON TEST SET
# ============================================

# Predict probabilities
test_predictions <- predict(logit_model, 
                            newdata = test_scaled, 
                            type = "prob")

# Add predictions to test data
test_results <- test_scaled %>%
  mutate(
    pred_prob = test_predictions$Yes,  # Probability of churn
    pred_class = ifelse(pred_prob > 0.5, "Yes", "No"),
    pred_class = factor(pred_class, levels = c("No", "Yes"))
  )

# ============================================
# STEP 7: EVALUATE MODEL PERFORMANCE
# ============================================

# Confusion Matrix
conf_matrix <- confusionMatrix(test_results$pred_class, 
                               test_results$churn,
                               positive = "Yes")
print(conf_matrix)

# Calculate additional metrics
accuracy <- conf_matrix$overall["Accuracy"]
precision <- conf_matrix$byClass["Precision"]
recall <- conf_matrix$byClass["Recall"]  # Sensitivity
f1 <- conf_matrix$byClass["F1"]
auc <- roc(test_results$churn, test_results$pred_prob)$auc[1]

cat("\n📊 MODEL PERFORMANCE SUMMARY:\n")
cat("Accuracy:", round(accuracy, 3), "\n")
cat("Precision:", round(precision, 3), "\n")
cat("Recall (Sensitivity):", round(recall, 3), "\n")
cat("F1-Score:", round(f1, 3), "\n")
cat("AUC-ROC:", round(auc, 3), "\n")

# ============================================
# STEP 8: CREATE VISUALIZATIONS
# ============================================

# 1. ROC Curve
roc_curve <- roc(test_results$churn, test_results$pred_prob)
roc_data <- data.frame(
  FPR = 1 - roc_curve$specificities,
  TPR = roc_curve$sensitivities
)

roc_plot <- ggplot(roc_data, aes(x = FPR, y = TPR)) +
  geom_line(color = "#2E86AB", size = 1.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.5) +
  annotate("text", x = 0.7, y = 0.3, 
           label = paste("AUC =", round(auc, 3)), 
           size = 5, fontface = "bold") +
  labs(title = "ROC Curve - Logistic Regression",
       subtitle = paste("Model performance on test set (n =", nrow(test_data), "customers)"),
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  theme_custom +
  coord_equal()

# 2. Feature Importance (Coefficients)
coef_summary <- summary(logit_model$finalModel)$coefficients
coef_data <- data.frame(
  Feature = rownames(coef_summary),
  Coefficient = coef_summary[, "Estimate"],
  P_Value = coef_summary[, "Pr(>|z|)"],
  Significance = ifelse(coef_summary[, "Pr(>|z|)"] < 0.05, "Significant", "Not Significant")
) %>%
  filter(Feature != "(Intercept)") %>%
  arrange(desc(abs(Coefficient)))

feature_plot <- ggplot(coef_data %>% head(10), 
                       aes(x = reorder(Feature, Coefficient), y = Coefficient, 
                           fill = Significance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Features Driving Churn Prediction",
       subtitle = "Based on logistic regression coefficients",
       x = "Feature",
       y = "Coefficient Magnitude",
       fill = "Statistical\nSignificance") +
  scale_fill_manual(values = c("Significant" = "#A23B72", 
                               "Not Significant" = "gray70")) +
  theme_custom

# Save plots
ggsave(plot = roc_plot, "logistic_regression_roc_curve.png" )
ggsave(plot= feature_plot, "logistic_regression_feature_importance.png")

# ============================================
# STEP 9: SAVE MODEL & RESULTS
# ============================================

# Save model
saveRDS(logit_model, "model_output/logistic_regression_model.rds")

# Save predictions
write.csv(test_results, "model_output/test_predictions.csv", row.names = FALSE)

# Save performance metrics
metrics_df <- data.frame(
  Model = "Logistic Regression",
  Accuracy = accuracy,
  Precision = precision,
  Recall = recall,
  F1_Score = f1,
  AUC = auc,
  Timestamp = Sys.time()
)

write.csv(metrics_df, "model_output/logistic_regression_metrics.csv", row.names = FALSE)

# ============================================
# STEP 10: BUSINESS INSIGHTS
# ============================================

cat("\n🎯 BUSINESS RECOMMENDATIONS:\n")
cat("1. Top 3 factors increasing churn risk:\n")
top_risks <- coef_data %>% 
  filter(Coefficient > 0) %>% 
  arrange(desc(Coefficient)) %>% 
  head(3)
for(i in 1:nrow(top_risks)) {
  cat("   •", top_risks$Feature[i], "(risk multiplier:", 
      round(exp(top_risks$Coefficient[i]), 2), "×)\n")
}

cat("\n2. Model can identify", round(recall * 100, 1), 
    "% of actual churners\n")
cat("3. For every 100 customers flagged as 'high risk',", 
    round(precision * 100, 1), "will actually churn\n")

cat("\n✅ Logistic regression complete! Next: Try Random Forest for comparison.\n")