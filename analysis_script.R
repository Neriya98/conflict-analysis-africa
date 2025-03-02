packages <- c("sf", "ggplot2", "dplyr")

for (package in packages){
  if (!require(package, character.only = TRUE)){
    install.packages(package)
  }
  library(package, character.only = TRUE)
}

# Import the database
countries_data = read.csv('Datas/Africa_1997-2025_Feb21.csv')
# Read the first five rows
View(countries_data)

# Define the country of interest
user_country <- readline("Choose the country you want: ")
# Subset for the country selected
mada_data = subset(countries_data, country == user_country)

# Load the shape file for the country
  ## Load the world shape file
world_shape = st_read("Layers/ne_110m_admin_0_countries.shp")
  ## Join the country data to the world shape file
mada_shape = left_join(world_shape, mada_data, by=c("NAME" = "country"))
  ## Subset for the country typed by the user
mada_shape <- subset(mada_shape, NAME == user_country)

# Group the conflict data by year
total_fatalities = sum(mada_shape$fatalities)
mada_shape_grouped = mada_shape %>% group_by(year) %>%
                                    summarise(total_conflicts = sum(fatalities),
                                              conflicts_proportion = 100 * total_conflicts/total_fatalities,
                                              .groups = "drop"
                                              )
# Plot the shape file with ggplot
ggplot()+
  geom_sf(data = mada_shape_grouped, aes(fill=total_conflicts))+
  theme_minimal()+
  labs(title = "Conflicts in Mada over the years")
