library(readr)
library(tidyverse)

datapermsa <- read_csv("datapermsa.csv")
Personal_Income_by_County <- read_csv("Personal Income by County.csv")
educated <- read_csv("educated.csv")
educated <- educated %>%
  select(Name, educated)

merged_data <- merge(datapermsa, Personal_Income_by_County, by = "Name", all.y = TRUE)

merged_data <- merged_data %>%
  filter(!is.na(Name))


mergeder_data <- merge(merged_data, educated, by = "Name", all.x = TRUE)

breaks <- quantile(mergeder_data$educated, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)

mergeder_data$Education_Level <- cut(mergeder_data$educated, breaks = breaks, labels = c("Low Education", "Medium Education", "High Education"), include.lowest = TRUE)


write.csv(merged_data, "new_datapermsa.csv", row.names = FALSE)
colnames(merged_data)
ggplot(merged_data, aes(x = `Population`, y = `Median Housing Price per sqft`)) +
  geom_point()

# write.csv(mergeder_data, "newer_datapermsa.csv", row.names = FALSE)


#--DONT RUN ABOVE HERE-------
df <- read_csv("newer_datapermsa.csv")
df <- df %>%
  mutate(log10_population = log10(Population))

ggplot(df, aes(x=log10_population)) + 
  geom_histogram()

ggplot(df, aes(x=log10_income)) + 
  geom_histogram() +
  labs(
    title="Log 10 Income Histogram")
    


ggplot(df, aes(x=log10(Income))) + 
  geom_histogram()

ggplot(df, aes(x=Price)) + 
  geom_histogram()
df <- df %>%
  mutate(log10_price = log10(Price))

df <- df %>%
  mutate(log10_income = log10(Income))


ggplot(df, aes(x=log10_price)) + 
  geom_histogram()



ggplot(df, aes(y = log10_income, x = log10_price)) +
  geom_point() +
  labs(title = "Income vs. Price", 
       x = "Median Personal Income (log10)",
       y = "Median Housing Price per Square Foot (log10)")

cor(df$log10_price, df$log10_income)

ggplot(df, aes(y = log10_income, x = log10_price, color = Education_Level)) +
  geom_point() +
  labs(title = "Income vs. Price with Education Level",
         x= "Median Housing Price per Square Foot (log10)",
         y= "Median Personal Income (log10)",
          color = "Education Level")

ggplot(df, aes(y = Education_Level, x = log10_price, fill = Education_Level)) +
  geom_boxplot() +
  labs(title = "Housing Price vs. Education Level",
       x = "Education Level",
       y = "Median Housing Price per Square Foot (log10)",
       fill = "Education Level") +
  theme_minimal()


ggplot(df, aes(x = Region, y = log10_price, fill = Region)) +
  geom_boxplot() +
  labs(title = "Housing Price vs. Education Level",
       x = "Education Level",
       y = "Median Housing Price per Square Foot (log10)",
       fill = "Education Level") +
  theme_minimal()



ggplot(df, aes(y = log10_income, x = log10_price, color = Region)) +
  geom_point() +
  labs(title = "Income vs. Price with Region",
       x= "Median Housing Price per Square Foot (log10)",
       y= "Median Personal Income (log10)",
       color = "Region")


all_data_model <- lm(log10_price ~ log10_population + log10_income + Education_Level + Region, data = df)
summary(all_data_model)

residuals <- residuals(all_data_model)
predicted_values <- fitted(all_data_model)

anova(all_data_model)

numeric_variables <- df[, sapply(df, is.numeric)]

# Calculate the correlation matrix
cor(numeric_variables)


library(moderndive)
library(skimr)
library(ISLR)



initial_model <- lm(log10_price ~ log10_population + log10_income + Education_Level + Region, data = df)

# Stepwise model selection
stepwise_model <- step(
  initial_model,
  scope = list(lower = ~ 1, upper = ~ log10_population + log10_income + Education_Level + Region),
  direction = "both",
  trace = 0
)

# Display the summary of the selected model
summary(initial_model)
summary(stepwise_model)


hist(residuals(stepwise_model), main = "Histogram of Residuals", col = "lightblue", border = "black")
qqnorm(residuals(stepwise_model), main = "Normal Q-Q Plot")

plot(stepwise_model, which = 1, main = "Residuals vs Fitted Values")
plot(stepwise_model, which = 3, main = "Scale-Location Plot")
plot(stepwise_model, which = 5, main = "Residuals vs Leverage")


plot(cooks.distance(stepwise_model), pch = "o", main = "Cook's Distance Plot", cex = 1, col = "blue")
abline(h = 4/(length(resid(stepwise_model)) - length(coefficients(stepwise_model))), col = "red")

library(MASS)

# Robust linear regression
robust_model <- rlm(log10_price ~ log10_population + log10_income + Education_Level + Region, data = df)

# Check the QQ plot for the robust model
qqnorm(residuals(robust_model), main = "Normal Q-Q Plot (Robust Model)")
qqline(residuals(robust_model))
boxplot(residuals(stepwise_model), main = "Boxplot of Residuals")




library(car)
vif(all_data_model)


# Create a residuals vs. fitted plot with ggplot2
ggplot() +
  geom_point(aes(x = predicted_values, y = residuals)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted",
       x = "Fitted Values (log10_price)",
       y = "Residuals")


library(rpart)
tree_model <- rpart(log10_price ~ log10_population + log10_income + Education_Level + Region, data = df)
print(tree_model)
plot(tree_model)
text(tree_model, cex = 0.8)
summary(tree_model)

plot(predict(tree_model), df$log10_price, main = "Predicted vs. Actual")
abline(0, 1, col = "red", lty = 2)

pruned_tree_model <- prune(tree_model, cp = 0.01)  # You may need to adjust the complexity parameter (cp)
summary(pruned_tree_model)



library(randomForest)
rf_model <- randomForest(log10_price ~ log10_population + log10_income + Education_Level + Region, data = df)

# Evaluate the model
summary(rf_model)
