# 1. Load Libraries and Data
library(tidyverse)

# Load the data
data <- read.csv("house_prices.csv")
head(data)

# 2. Data Preparation and Cleaning
data <- data %>%
  filter(!is.na(GrLivArea), !is.na(FullBath), !is.na(BedroomAbvGr), !is.na(SalePrice))

# 3. Data Exploration
summary(data)
# Plot relationships
ggplot(data, aes(x = GrLivArea, y = SalePrice)) + geom_point() + geom_smooth(method = "lm")

# 4. Train-Test Split
set.seed(123)
train_indices <- sample(seq_len(nrow(data)), size = 0.8 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# 5. Build Linear Regression Model
model <- lm(SalePrice ~ GrLivArea + FullBath + BedroomAbvGr, data = train_data)
summary(model)

# 6. Make Predictions and Evaluate
predictions <- predict(model, test_data)
results <- data.frame(Actual = test_data$SalePrice, Predicted = predictions)

# Calculate Metrics
mae <- mean(abs(results$Actual - results$Predicted))
rmse <- sqrt(mean((results$Actual - results$Predicted)^2))
cat("MAE:", mae, "\nRMSE:", rmse, "\n")

# 7. Plot Predictions vs Actual
ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Actual vs. Predicted Sale Prices", x = "Actual Prices", y = "Predicted Prices") +
  theme_minimal()
