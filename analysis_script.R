packages <- c("sf", "ggplot2", "dplyr", "viridis")

for (package in packages){
  if (!require(package, character.only = TRUE)){
    install.packages(package)
  }
  library(package, character.only = TRUE)
}
# ------------------- Preprocessing of the data --------------------------------

# Import the ACLED database
countries_data = read.csv('Datas/Africa_1997-2025_Feb21.csv')

# Define the country of interest
user_country <- readline("Choose the country you want: ")

# Subset for the country selected
user_data = subset(countries_data, country == user_country)

# ------------------ Load the shape file for the country ---------------------
  ## Load the world shape file
world_shape = st_read("Layers/world-shapefile/ne_110m_admin_0_countries.shp")
  ## Load the user shape file
user_shape = st_read("Layers/Madagascar/Madagascar.shp")  
  ## Join the country data to the world shape file
joint_user_data_shape = st_as_sf(left_join(user_data, user_shape, by=c("admin1" = "adm2nm")))


# Categorize fatalities by admin_1
conflicts_grouped <- mada_shape %>%
                     st_drop_geometry() %>%
                     group_by(admin1)%>%
                    summarise(total_fatalities = sum(fatalities),
                              .groups = "drop")
  
conflicts_grouped <- st_as_sf(left_join(conflicts_grouped, mada_shape, by="admin1"))



# Plot the shape file with ggplot
ggplot(data = joint_user_data_shape) +  # Create the base ggplot object
  geom_sf(aes(fill = fatalities), color = "white", size = 0.2) +  # Plot the shapefile with color fill
  scale_fill_gradient(  # Apply a custom gradient color scale
    low = "lightblue", 
    high = "darkred", 
    na.value = "gray", 
    limits = c(min(joint_user_data_shape$fatalities, na.rm = TRUE), 
               max(joint_user_data_shape$fatalities, na.rm = TRUE))
  ) +
  theme_minimal() +  # Apply a minimal theme
  labs(title = "Fatalities by Admin1", fill = "Fatalities")  # Add title and legend label