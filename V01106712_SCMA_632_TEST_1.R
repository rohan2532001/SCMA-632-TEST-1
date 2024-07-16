# Install missing packages if necessary
install.packages("olsrr")
install.packages("lmtest")

# Load necessary libraries
library(tidyverse)
library(caret)
library(car)
library(ggplot2)
library(olsrr)
library(lmtest)

# Load the dataset
data <- read.csv("C:/Users/HP/Downloads/cancer_reg.csv", encoding = "latin1")

# Data Cleaning and Preprocessing
data <- data %>% drop_na() # Handle missing values

# Convert categorical variables to factors
data$binnedInc <- as.factor(data$binnedInc)

# Build the multivariate OLS regression model
model <- lm(TARGET_deathRate ~ ., data = data)

# Summary of the model
summary(model)

# Calculate RMSE
predicted <- predict(model, data)
rmse <- sqrt(mean((data$TARGET_deathRate - predicted)^2))
print(paste("RMSE:", rmse))

# Model Diagnostics
# Linearity
plot(model, which = 1)

# Serial independence of errors
durbinWatsonTest(model)

# Heteroskedasticity
bptest(model)

# Normality of residuals
residuals <- resid(model)
if (length(unique(residuals)) > 1) {
  qqnorm(residuals)
  qqline(residuals)
  shapiro.test(residuals)
} else {
  "Residuals contain only identical values."
}
# Multicollinearity
vif_model <- try(vif(model), silent = TRUE)
if (class(vif_model) == "try-error") {
  "There are aliased coefficients in the model indicating multicollinearity issues."
} else {
  vif_model
}

# Save the HTML report
rmarkdown::render("path/to/your_script.Rmd", output_format = "html_document")

