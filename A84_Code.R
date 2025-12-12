#  Load Required Libraries
#  Install packages 
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")

# Load libraries
library(ggplot2)  # For visualisation
library(dplyr)    # For data manipulation

# Loading the Dataset
data <- read.csv("Fuel_Consumption_Ratings.csv", stringsAsFactors = FALSE)

# Rename Columns for Readability 
colnames(data) <- c(
  "Model_Year",
  "Make",
  "Model",
  "Vehicle_Class",
  "Engine_Size_L",
  "Cylinders",
  "Transmission",
  "Fuel_Type",
  "Fuel_Consumption_City_L100km",
  "Fuel_Consumption_Hwy_L100km",
  "Fuel_Consumption_Comb_L100km",
  "Fuel_Consumption_Comb_mpg",
  "CO2_Emissions_gkm",
  "CO2_Rating",
  "Smog_Rating"
)
# Data Inspection
cat(" Dataset Overview\n")
cat("Dimensions:", dim(data)[1], "rows x", dim(data)[2], "columns\n\n")

cat("Column Names (Renamed):\n")
print(colnames(data))

cat("\nData Structure\n")
str(data)

#Checking missing values
cat("\nMissing Values\n")
missing <- colSums(is.na(data))
print(missing[missing > 0])
if(sum(missing) == 0) cat("No missing values found!\n")

#Duplicates Check
cat("\n Duplicate Rows \n")
dup_count <- sum(duplicated(data))
cat("Number of duplicate rows:", dup_count, "\n")

# Summary Statistics
cat("\n Summary Statistics (Before Cleaning)\n")
summary(data)

# Checking Outliers
cat("\n Outlier Check (numeric columns) ===\n")
numeric_cols <- sapply(data, is.numeric)
numeric_data <- data[, numeric_cols]

for(col in names(numeric_data)) {
  outliers <- boxplot.stats(numeric_data[[col]])$out
  if(length(outliers) > 0) {
    cat(col, ":", length(outliers), "potential outliers\n")
  }
}

#  Categorical Variables 
cat("\n Categorical Variables \n")
categorical_cols <- sapply(data, function(x) is.character(x) | is.factor(x))
cat_data <- data[, categorical_cols]

for(col in names(cat_data)[1:min(5, length(names(cat_data)))]) {
  cat("\n", col, "- Unique values:", length(unique(cat_data[[col]])), "\n")
  print(head(table(cat_data[[col]]), 5))
}

#  Data Cleaning 
cat("\n\n Data Cleaning \n")

# Remove rows with missing values
cat("\nOriginal dataset:", nrow(data), "rows\n")
data_clean <- na.omit(data)
cat("After removing missing values:", nrow(data_clean), "rows\n")
cat("Removed:", nrow(data) - nrow(data_clean), "rows with missing values\n")

# Remove duplicate rows
before_dup <- nrow(data_clean)
data_clean <- data_clean[!duplicated(data_clean), ]
cat("After removing duplicates:", nrow(data_clean), "rows\n")
cat("Removed:", before_dup - nrow(data_clean), "duplicate rows\n")

# Convert Model_Year to numeric
# First, check if Model_Year needs cleaning
cat("\nChecking Model_Year column\n")
cat("Unique values in Model_Year:\n")
print(table(data_clean$Model_Year))

# Convert to numeric (handles if it's character)
data_clean$Model_Year <- as.numeric(as.character(data_clean$Model_Year))

# Remove any rows where Model_Year conversion failed (resulted in NA)
if(sum(is.na(data_clean$Model_Year)) > 0) {
  cat("Removing", sum(is.na(data_clean$Model_Year)), "rows with invalid Model_Year\n")
  data_clean <- data_clean[!is.na(data_clean$Model_Year), ]
}

# Filter to keep only 2019 data 
data_clean <- data_clean %>% filter(Model_Year == 2019)
cat("\nFiltered to 2019 vehicles only:", nrow(data_clean), "rows\n")

# Verify Cleaning
cat("\n Cleaned Dataset Summary \n")
cat("Final dimensions:", nrow(data_clean), "rows x", ncol(data_clean), "columns\n\n")

# Check no missing values remain
cat("Missing values after cleaning:\n")
missing_after <- colSums(is.na(data_clean))
if(sum(missing_after) == 0) {
  cat(" No missing values!\n")
} else {
  print(missing_after[missing_after > 0])
}

# Key Variables Summary
cat("\nKey Variables for Analysis\n")
summary(data_clean[, c("Engine_Size_L", "CO2_Emissions_gkm", 
                       "Fuel_Consumption_City_L100km", "Vehicle_Class")])

cat("\n Variable Descriptions\n")
cat("Engine_Size_L: Engine size in litres\n")
cat("CO2_Emissions_gkm: CO2 emissions in grams per kilometre\n")
cat("Fuel_Consumption_City_L100km: City fuel consumption in L/100km\n")
cat("Fuel_Consumption_Hwy_L100km: Highway fuel consumption in L/100km\n")
cat("Fuel_Consumption_Comb_L100km: Combined fuel consumption in L/100km\n")
cat("CO2_Rating: CO2 emissions rating (1=worst, 10=best)\n")
cat("Smog_Rating: Smog rating (1=worst, 10=best)\n")

# Save Cleaned Dataset
write.csv(data_clean, "Fuel_Consumption_Clean.csv", row.names = FALSE)
cat("\nCleaned dataset saved as 'Fuel_Consumption_Clean.csv'\n")
# Quick Preview
cat("\n First 10 rows of cleaned data \n")
print(head(data_clean, 10))

# Load cleaned data
data_clean <- read.csv("Fuel_Consumption_Clean.csv")

cat(" Data Visualisations\n\n")

# Scatter Plot for Research Question

cat("Main scatter plot (Engine Size vs CO2 Emissions)\n")

plot_main <- ggplot(data_clean, aes(x = Engine_Size_L, y = CO2_Emissions_gkm)) +
  geom_point(aes(color = Vehicle_Class), alpha = 0.6, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1.2, 
              fill = "lightblue", alpha = 0.3) +
  labs(
    title = "Relationship between Engine Size and CO2 Emissions in 2019 Vehicles",
    x = "Engine Size (Litres)",
    y = "CO2 Emissions (grams per kilometre)",
    color = "Vehicle Class",
    caption = paste0("Source: 2019 Fuel Consumption Ratings, n = ", nrow(data_clean))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 11),
    axis.title.y = element_text(face = "bold", size = 11),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0, size = 9, color = "gray40")
  ) +
  scale_x_continuous(breaks = seq(0, 8, 1)) +
  scale_y_continuous(breaks = seq(0, 550, 50))

# Save the main plot
ggsave("Engine_CO2_Scatter.png", plot_main, 
       width = 10, height = 6, dpi = 300, bg = "white")
cat("Saved:Engine_CO2_Scatter.png\n\n")
print(plot_main)   

#  Histogram for CO2 Emissions Distribution 
cat("CO2 Emissions distribution histogram\n")

plot_co2_hist <- ggplot(data_clean, aes(x = CO2_Emissions_gkm)) +
  geom_histogram(binwidth = 20, fill = "steelblue", color = "black", 
                 alpha = 0.7) +
  geom_vline(aes(xintercept = mean(CO2_Emissions_gkm)), 
             color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", x = mean(data_clean$CO2_Emissions_gkm) + 30, 
           y = Inf, vjust = 2,
           label = paste0("Mean = ", round(mean(data_clean$CO2_Emissions_gkm), 1), " g/km"),
           color = "red", fontface = "bold") +
  labs(
    title = "Distribution of CO2 Emissions in 2019 Vehicles",
    x = "CO2 Emissions (grams per kilometre)",
    y = "Frequency (Number of Vehicles)",
    caption = paste0("n = ", nrow(data_clean), 
                     ", Mean = ", round(mean(data_clean$CO2_Emissions_gkm), 1),
                     " g/km, SD = ", round(sd(data_clean$CO2_Emissions_gkm), 1))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 11),
    axis.title.y = element_text(face = "bold", size = 11),
    plot.caption = element_text(hjust = 0.5, size = 9, color = "gray40")
  )

ggsave("CO2_Distribution.png", plot_co2_hist, 
       width = 8, height = 6, dpi = 300, bg = "white")
cat("Saved: CO2_Distribution.png\n\n")
print(plot_co2_hist)   

# Histogram for Engine Size Distribution
cat("Engine Size distribution histogram\n")

plot_engine_hist <- ggplot(data_clean, aes(x = Engine_Size_L)) +
  geom_histogram(binwidth = 0.5, fill = "darkgreen", color = "black", 
                 alpha = 0.7) +
  geom_vline(aes(xintercept = mean(Engine_Size_L)), 
             color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", x = mean(data_clean$Engine_Size_L) + 0.5, 
           y = Inf, vjust = 2,
           label = paste0("Mean = ", round(mean(data_clean$Engine_Size_L), 2), " L"),
           color = "red", fontface = "bold") +
  labs(
    title = "Distribution of Engine Size in 2019 Vehicles",
    x = "Engine Size (Litres)",
    y = "Frequency (Number of Vehicles)",
    caption = paste0("n = ", nrow(data_clean), 
                     ", Mean = ", round(mean(data_clean$Engine_Size_L), 2),
                     " L, SD = ", round(sd(data_clean$Engine_Size_L), 2))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 11),
    axis.title.y = element_text(face = "bold", size = 11),
    plot.caption = element_text(hjust = 0.5, size = 9, color = "gray40")
  ) +
  scale_x_continuous(breaks = seq(0, 8, 1))

ggsave("Engine_Distribution.png", plot_engine_hist, 
       width = 8, height = 6, dpi = 300, bg = "white")
cat(" Saved:Engine_Distribution.png\n\n")
print(plot_engine_hist)   

# Correlation Comparison Table 
cat("Correlation matrix for key variables\n")

# Select relevant variables
cor_vars <- data_clean %>% 
  select(Engine_Size_L, Cylinders, 
         Fuel_Consumption_Comb_L100km, CO2_Emissions_gkm)

# Calculate correlation matrix
cor_matrix <- cor(cor_vars)

# Print correlation matrix
cat("\nCorrelation Matrix :\n")
print(round(cor_matrix, 3))

# Save as CSV 
write.csv(round(cor_matrix, 3), "Correlation_Matrix.csv")
cat("\nSaved: Correlation_Matrix.csv\n\n")
# Descriptive Statistics Table
cat("Descriptive statistics\n")

# Create summary table
desc_stats <- data_clean %>%
  summarise(
    Variable = "Multiple",
    Engine_Size_Mean = round(mean(Engine_Size_L), 2),
    Engine_Size_SD = round(sd(Engine_Size_L), 2),
    CO2_Mean = round(mean(CO2_Emissions_gkm), 1),
    CO2_SD = round(sd(CO2_Emissions_gkm), 1),
    N = n()
  )

print(desc_stats)
write.csv(desc_stats, "Descriptive_Statistics.csv", row.names = FALSE)
cat("Saved: Descriptive_Statistics.csv\n\n")


# Regression Analysis

# Load cleaned data
data_clean <- read.csv("Fuel_Consumption_Clean.csv")

cat("Regression Analysis\n")
cat("Research Question: Can engine size predict CO2 emissions?\n\n")

# Linear Regression
cat(" Simple Linear Regression \n")
cat("Predictor: Engine_Size_L\n")
cat("Outcome: CO2_Emissions_gkm\n\n")

# Build regression model
model <- lm(CO2_Emissions_gkm ~ Engine_Size_L, data = data_clean)

# Display results
cat("Model Summary:\n")
summary(model)

# Extract key statistics
r_squared <- summary(model)$r.squared
adj_r_squared <- summary(model)$adj.r.squared
f_stat <- summary(model)$fstatistic[1]
p_value <- pf(summary(model)$fstatistic[1], 
              summary(model)$fstatistic[2], 
              summary(model)$fstatistic[3], 
              lower.tail = FALSE)

cat("\n Key Statistics \n")
cat("R-squared:", round(r_squared, 4), "\n")
cat("Adjusted R-squared:", round(adj_r_squared, 4), "\n")
cat("F-statistic:", round(f_stat, 2), "\n")
cat("p-value:", format(p_value, scientific = TRUE), "\n")

# Interpretation
cat("\n Interpretaion\n")
if(p_value < 0.05) {
  cat("Reject Null Hypothesis (p < 0.05)\n")
  cat("Engine size SIGNIFICANTLY predicts CO2 emissions.\n")
  cat("R² =", round(r_squared, 4), "meaning engine size explains", 
      round(r_squared * 100, 1), "% of variance in CO2 emissions.\n")
} else {
  cat("Fail to Reject Null Hypothesis (p >= 0.05)\n")
  cat("Engine size does NOT significantly predict CO2 emissions.\n")
}

# Model Co-efficient
cat("\n Regression Equation \n")
intercept <- coef(model)[1]
slope <- coef(model)[2]
cat("CO2 Emissions = ", round(intercept, 2), " + ", 
    round(slope, 2), " × Engine Size\n", sep = "")
cat("\nInterpretation: For every 1L increase in engine size,\n")
cat("CO2 emissions increase by", round(slope, 2), "g/km on average.\n")

# Scatter Plot with Regression
cat("\n Creating Visualization \n")

# Main scatter plot with regression line
plot1 <- ggplot(data_clean, aes(x = Engine_Size_L, y = CO2_Emissions_gkm)) +
  geom_point(aes(color = Vehicle_Class), alpha = 0.6, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1.2) +
  labs(
    title = "Relationship between Engine Size and CO2 Emissions",
    subtitle = paste0("R² = ", round(r_squared, 3), 
                      ", p < 0.001, n = ", nrow(data_clean)),
    x = "Engine Size (Litres)",
    y = "CO2 Emissions (g/km)",
    color = "Vehicle Class"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    legend.position = "right"
  )

# Save the plot
ggsave("Regression_Engine_CO2.png", plot1, 
       width = 10, height = 6, dpi = 300)
cat("Saved: Regression_Engine_CO2.png\n")
print(plot1)   

#  Histogram - CO2 Emissions Distirbution 
plot2 <- ggplot(data_clean, aes(x = CO2_Emissions_gkm)) +
  geom_histogram(binwidth = 20, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(CO2_Emissions_gkm)), 
             color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Distribution of CO2 Emissions in 2019 Vehicles",
    subtitle = paste0("Mean = ", round(mean(data_clean$CO2_Emissions_gkm), 1), 
                      " g/km, SD = ", round(sd(data_clean$CO2_Emissions_gkm), 1)),
    x = "CO2 Emissions (g/km)",
    y = "Frequency (Number of Vehicles)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave("CO2_Distribution.png", plot2, 
       width = 8, height = 6, dpi = 300)
cat("Saved: CO2_Distribution.png\n")
print(plot2)   

# Histogram Engine Size Distribution
plot3 <- ggplot(data_clean, aes(x = Engine_Size_L)) +
  geom_histogram(binwidth = 0.5, fill = "darkgreen", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(Engine_Size_L)), 
             color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Distribution of Engine Size in 2019 Vehicles",
    subtitle = paste0("Mean = ", round(mean(data_clean$Engine_Size_L), 2), 
                      " L, SD = ", round(sd(data_clean$Engine_Size_L), 2)),
    x = "Engine Size (Litres)",
    y = "Frequency (Number of Vehicles)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave("Engine_Distribution.png", plot3, 
       width = 8, height = 6, dpi = 300)
cat("Saved: Engine_Distribution.png\n")
print(plot3)   

# Residual Plot 
plot4 <- ggplot(data_clean, aes(x = fitted(model), y = residuals(model))) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Residual Plot - Model Diagnostics",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal()

ggsave("Residuals.png", plot4, width = 8, height = 6, dpi = 300)
cat("Saved:Residuals.png\n")
print(plot4)   

# Correlation Table
cat("\nCorrelation Matrix \n")
cor_vars <- data_clean %>% 
  select(Engine_Size_L, Cylinders, Fuel_Consumption_Comb_L100km, CO2_Emissions_gkm)
cor_matrix <- cor(cor_vars)
print(round(cor_matrix, 3))