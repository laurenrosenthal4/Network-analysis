install.packages("rgdal")
library(rgdal)

find_census_vectors()
find_census_vectors("population", "CA21", type = "all", query_type = "keyword"
  )

list_census_regions("CA16") %>%
  filter(level == "CSD", name == "Montréal") %>%
  as_census_region_list() -> r # pulling the census geometry. 

list_census_vectors("CA21") %>%
  filter(str_detect(label, "Population, 2021")) %>%
  pull(vector) -> population # Pulling the vector with the number of total dwellings


population_mtl_DA <- get_census(dataset = "CA21",
                             regions = r,
                             vector = population,
                             level = "DA",
                             geo_format = "sf")

population_DA_filtered <- population_mtl_DA %>%
  filter(Population > 0)
population_DA_filtered

xy <- st_centroid(population_DA_filtered)
xy <- xy[c(6,16)]
as.data.frame(xy)
class(xy)

DA_centroids <- xy %>%
  mutate(lat = unlist(map(xy$geometry,1)),
         long = unlist(map(xy$geometry,2)))

DA_centroids <- st_drop_geometry(DA_centroids)
DA_centroids <- DA_centroids %>%
  rename(lon = longitude) %>%
  rename(lat = latitude)

##### DA scale - all parks #####

# Walking only
mode <- c("WALK")
max_walk_dist <- 1500
max_trip_duration <- 120
departure_datetime <- as.POSIXct("17-07-2019 14:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")
time_window <- 20
ttm_ct_parks <- travel_time_matrix(r5r_core = r5r_core,
                                   origins = DA_centroids, # We used the city centroids as origin points. You can only have 3 columns: id, long, lat
                                   destinations = parks,
                                   mode =  mode,
                                   departure_datetime = departure_datetime,
                                   max_walk_dist = max_walk_dist,
                                   max_trip_duration = max_trip_duration,
                                   time_window = time_window,
                                   verbose = FALSE)
ttm_DA_walk_allparks <- ttm_ct_parks


glimpse(DA_centroids)

tmap_mode("view")
tm_shape(population_mtl_DA)+
  tm_fill("Population")+
  tm_borders(col = "black")+
  tm_shape(xy)+
  tm_dots()

