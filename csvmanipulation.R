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

write.csv(mergeder_data, "newer_datapermsa.csv", row.names = FALSE)


#---------
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

# Create a residuals vs. fitted plot with ggplot2
ggplot() +
  geom_point(aes(x = predicted_values, y = residuals)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted",
       x = "Fitted Values (log10_price)",
       y = "Residuals")