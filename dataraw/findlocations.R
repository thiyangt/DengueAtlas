# Install packages if needed
install.packages(c("tidygeocoder", "dplyr", "writexl"))

library(tidygeocoder)
library(dplyr)
library(writexl)

# area names
DengueAtlas1 <- read_excel("dataraw/DengueAtlas.xlsx", sheet=1)
areas <- DengueAtlas1$HighRiskMOHAreas

# Remove duplicates
areas_unique <- unique(areas)

# Add country name to improve matching
search_names <- paste(areas_unique, "Sri Lanka")

# Geocode
coords <- tibble(area = areas_unique,
                 search = search_names) %>%
  geocode(address = search,
          method = "osm",
          lat = latitude,
          long = longitude) %>%
  select(area, longitude, latitude)

# View
print(coords)

# Export to Excel
write_csv(coords, file = here("dataraw","sri_lanka_areas_coordinates.csv"))
