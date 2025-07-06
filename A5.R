#-------------------------------#
# Step 1: Load and inspect data ----
#-------------------------------#

# Set working directory to the location of your dataset
setwd('D:\\Teaching\\SCMA632 2025 C51\\Assignments\\Data')

# Read the NSSO68 dataset
df <- read.csv('NSSO68.csv')

# Inspect column names
names(df)

# Check the 'foodtotal_v' column
head(df$foodtotal_v)


#----------------------------------------#
# Step 2: Filter the data for Karnataka ----
#----------------------------------------#

# Check how many Karnataka rows are there
sum(df$state_1 == 'KA')

# Filter rows where state_1 is 'KA'
ka <- df[df$state_1 == 'KA', ]
dim(ka)

# Histogram of food consumption in Karnataka
hist(ka$foodtotal_v, 
     main = "Distribution of Food Consumption in Karnataka",
     xlab = "Food Consumption (foodtotal_v)", 
     col = "lightblue", 
     border = "white")


#----------------------------------------------#
# Step 3: Group-wise summary at District level ----
#----------------------------------------------#

# Convert District column to factor (if not already)
ka$District <- as.factor(ka$District)

# Load dplyr for grouping
library(dplyr)

# Add district-wise average food consumption column
ka <- ka %>%
  group_by(District) %>%
  mutate(DWCons = mean(foodtotal_v, na.rm = TRUE)) %>%
  ungroup()


#--------------------------------------------------------#
# Step 4: Create mapping of District Codes to Names ----
#--------------------------------------------------------#

district_map <- data.frame(
  DistrictCode = sprintf("%02d", 1:29),  # Format as 01, 02, ..., 29
  DistrictName = c("Belgaum", "Bagalkot", "Bijapur", "Gulbarga", "Bidar", "Raichur", "Koppal",
                   "Gadag", "Dharwad", "Uttara Kannada", "Haveri", "Bellary", "Chitradurga",
                   "Davanagere", "Shimoga", "Udupi", "Chikmagalur", "Tumkur", "Kolar", 
                   "Bangalore", "Bangalore Rural", "Mandya", "Hassan", "Dakshina Kannada",
                   "Kodagu", "Mysore", "Chamarajanagar", "Ramanagar", "Chikkaballapura"),
  stringsAsFactors = FALSE
)


#---------------------------------------------------------#
# Step 5: Merge mapping into main data using District code ----
#---------------------------------------------------------#

# Create a DistrictCode column from District number
ka <- ka %>%
  mutate(DistrictCode = sprintf("%02d", as.numeric(District)))  # Converts 1 to '01', etc.

# Merge to get District names
ka <- ka %>%
  left_join(district_map, by = "DistrictCode")


#------------------------------------------------#
# Step 6: Summarize and Plot Bar Chart ----
#------------------------------------------------#

# Create summary table: average food consumption by district
district_avg <- ka %>%
  group_by(DistrictName) %>%
  summarise(avg_food = mean(foodtotal_v, na.rm = TRUE)) %>%
  arrange(desc(avg_food))  # Sort by consumption

# Barplot: average food consumption by district
barplot(height = district_avg$avg_food,
        names.arg = district_avg$DistrictName,
        las = 2,                 # Rotate x-axis labels vertically
        col = "skyblue",        
        main = "Average Food Consumption by District (Karnataka)",
        ylab = "Avgerage Food Consumption (Rs.)",
        cex.names = 0.7)        # Adjust label size if too crowded


# Choropleth Maps
# Plot data on the map itself

# a variable of our choice
# geojson file or the shapefile 

library(ggplot2) 
library(sf) # mapping
library(dplyr) 

#Sys.setenv("SHAPE_RESTORE_SHX" = "YES")

data_map <- st_read("KARNATAKA_DISTRICTS.geojson") 
View(data_map)


# Step 1: Ensure district name column matches in both datasets
data_map <- data_map %>%
  rename(DistrictName = dtname)  # Rename if needed

# Step 2: Left join spatial data with data values
data_map_data <- data_map %>%
  left_join(district_avg, by = "DistrictName")  # Keeps all districts

# Step 3: Replace NA with 0 for missing data
data_map_data$avg_food[is.na(data_map_data$avg_food)] <- 0

# Step 4: Plot using ggplot2
ggplot(data_map_data) + 
  geom_sf(aes(fill = avg_food, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Average Food Consumption by District") +
  theme_minimal() +
  geom_sf_text(aes(label = DistrictName), size = 3, color = "black")
