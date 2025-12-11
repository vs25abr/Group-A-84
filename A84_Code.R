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
