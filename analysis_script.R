packages <- c("sf", "ggplot2", "dplyr", "viridis", "RColorBrewer")

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
user_country <- "Madagascar" #readline("Choose the country you want: ")

# Subset for the country selected
user_data = subset(countries_data, c(country == user_country & year > 2020))

# ------------------ Load the shape file for the country ---------------------
  ## Load the world shape file
world_shape = st_read("Layers/world-shapefile/ne_110m_admin_0_countries.shp")
  ## Load the user shape file
user_shape = st_read("Layers/Madagascar/Madagascar.shp")  
  ## Join the country data to the world shape file
joint_user_data_shape = left_join(user_data, user_shape, by=c("admin1" = "adm2nm"))


# Categorize fatalities by admin_1 (region)
conflicts_grouped <- joint_user_data_shape %>%
                     group_by(admin1)%>%
                    summarise(fatalities = sum(fatalities),
                              geometry = last(geometry),
                              .groups = "drop")
  
conflicts_grouped <- st_as_sf(conflicts_grouped)

# Plot the shape with region colored
ggplot() +  
  geom_sf(data = world_shape) +
  geom_sf(data = conflicts_grouped, aes(fill = admin1), color = "black", size = 0.8) +
  coord_sf(xlim = c(30, 55), ylim = c(-26, -11)) +
  theme_minimal() +  
  labs(title = paste("Regions in", user_country), fill = "Region")

# --------lot the conflict points on the map with color by disorder type -------
ggplot() +
  geom_sf(data = world_shape) +
  geom_sf(data = user_shape) +
  geom_point(data = joint_user_data_shape, aes(x = longitude, y = latitude, color = disorder_type), size = 1) +
  scale_color_brewer(palette = "Set1") +  
  coord_sf(xlim = c(30, 55), ylim = c(-26, -11)) +
  theme_bw() +
  labs(title = paste("Location of conflicts per type in", user_country, "[After 2015]"), color = "Disorder type")

# ----------------- Plot the shape with scalled color by fatalities ------------
ggplot() +  
  geom_sf(data = world_shape) +
  geom_sf(data = conflicts_grouped, aes(fill = fatalities), color = "white", size = 0.2) +
  scale_fill_gradient(  
    low = "lightblue", 
    high = "darkred", 
    na.value = "gray", 
    limits = c(min(conflicts_grouped$fatalities, na.rm = TRUE), 
               max(conflicts_grouped$fatalities, na.rm = TRUE))
  ) +
  coord_sf(xlim = c(20, 51), ylim = c(-36, -11)) +  # Focus on Madagascar
  theme_minimal() +  
  labs(title = paste("Number of fatalities per region in", user_country, "[After 2015]"), fill = "Fatalities")