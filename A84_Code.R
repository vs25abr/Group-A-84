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
