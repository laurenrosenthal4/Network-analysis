library(r5r)
library(tidyverse)
library(cancensus)
library(tmap)
library(sf)

options(cancensus.cache_path = paste(getwd(),"/Test",sep="")) # This will use our working directory as the folder where it will cache the data from cancensus. 
options(cancensus.api_key = "CensusMapper_49272a61659f167722512e80b3dd8e3c")

rm(list = ls())

options(java.parameters = "-Xmx8G")
setup_r5("Parks/", verbose = FALSE)
r5r_core <- .Last.value
r5r_core

parks <- read_csv("Parks/parks.csv")
points <- read_csv("Parks/ct_centroids.csv")
points

##### CT scale - all parks #####

# Walking only
mode <- c("WALK")
max_walk_dist <- 1500
max_trip_duration <- 120
departure_datetime <- as.POSIXct("17-07-2019 14:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")
time_window <- 20
ttm_ct_parks <- travel_time_matrix(r5r_core = r5r_core,
                                   origins = points, # We used the city centroids as origin points. You can only have 3 columns: id, long, lat
                                   destinations = parks,
                                   mode =  mode,
                                   departure_datetime = departure_datetime,
                                   max_walk_dist = max_walk_dist,
                                   max_trip_duration = max_trip_duration,
                                   time_window = time_window,
                                   verbose = FALSE)
ttm_ct_walk_allparks <- ttm_ct_parks
ttm_ct_walk_allparks %>%
  summarise(mean_travel_time = mean(travel_time))

# Biking only
mode <- c("BICYCLE")
max_walk_dist <- 1500
max_trip_duration <- 120
departure_datetime <- as.POSIXct("17-07-2019 14:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")
time_window <- 20
ttm_ct_bike_allparks <- travel_time_matrix(r5r_core = r5r_core,
                                           origins = points, # We used the city centroids as origin points. You can only have 3 columns: id, long, lat
                                           destinations = parks,
                                           mode =  mode,
                                           departure_datetime = departure_datetime,
                                           max_walk_dist = max_walk_dist,
                                           max_trip_duration = max_trip_duration,
                                           time_window = time_window,
                                           verbose = FALSE)

# Transit only - morning

mode <- c("TRANSIT")
max_walk_dist <- 1500
max_trip_duration <- 120
departure_datetime <- as.POSIXct("17-07-2019 9:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")
time_window <- 20
ttm_ct_transit_allparks_morning <- travel_time_matrix(r5r_core = r5r_core,
                                                      origins = points, # We used the city centroids as origin points. You can only have 3 columns: id, long, lat
                                                      destinations = parks,
                                                      mode =  mode,
                                                      departure_datetime = departure_datetime,
                                                      max_walk_dist = max_walk_dist,
                                                      max_trip_duration = max_trip_duration,
                                                      time_window = time_window,
                                                      verbose = FALSE)

# Transit only - afternoon

mode <- c("TRANSIT")
max_walk_dist <- 1500
max_trip_duration <- 120
departure_datetime <- as.POSIXct("17-07-2019 14:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")
time_window <- 20
ttm_ct_transit_allparks_afternoon <- travel_time_matrix(r5r_core = r5r_core,
                                                        origins = points, # We used the city centroids as origin points. You can only have 3 columns: id, long, lat
                                                        destinations = parks,
                                                        mode =  mode,
                                                        departure_datetime = departure_datetime,
                                                        max_walk_dist = max_walk_dist,
                                                        max_trip_duration = max_trip_duration,
                                                        time_window = time_window,
                                                        verbose = FALSE)

ttm_ct_transit_allparks_afternoon


# Transit only - evening

mode <- c("TRANSIT")
max_walk_dist <- 1500
max_trip_duration <- 120
departure_datetime <- as.POSIXct("17-07-2019 20:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")
time_window <- 20
ttm_ct_transit_allparks_evening <- travel_time_matrix(r5r_core = r5r_core,
                                                      origins = points, # We used the city centroids as origin points. You can only have 3 columns: id, long, lat
                                                      destinations = parks,
                                                      mode =  mode,
                                                      departure_datetime = departure_datetime,
                                                      max_walk_dist = max_walk_dist,
                                                      max_trip_duration = max_trip_duration,
                                                      time_window = time_window,
                                                      verbose = FALSE)

# All modes - morning

mode <- c("WALK", "BICYCLE", "TRANSIT")
max_walk_dist <- 1500
max_trip_duration <- 120
departure_datetime <- as.POSIXct("17-07-2019 9:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")
time_window <- 20
ttm_ct_allmodes_allparks_morning <- travel_time_matrix(r5r_core = r5r_core,
                                                         origins = points, # We used the city centroids as origin points. You can only have 3 columns: id, long, lat
                                                         destinations = parks,
                                                         mode =  mode,
                                                         departure_datetime = departure_datetime,
                                                         max_walk_dist = max_walk_dist,
                                                         max_trip_duration = max_trip_duration,
                                                         time_window = time_window,
                                                         verbose = FALSE)

# All modes - afternoon

mode <- c("WALK", "BICYCLE", "TRANSIT")
max_walk_dist <- 1500
max_trip_duration <- 120
departure_datetime <- as.POSIXct("17-07-2019 14:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")
time_window <- 20
ttm_ct_allmodes_allparks_afternoon <- travel_time_matrix(r5r_core = r5r_core,
                                                       origins = points, # We used the city centroids as origin points. You can only have 3 columns: id, long, lat
                                                       destinations = parks,
                                                       mode =  mode,
                                                       departure_datetime = departure_datetime,
                                                       max_walk_dist = max_walk_dist,
                                                       max_trip_duration = max_trip_duration,
                                                       time_window = time_window,
                                                       verbose = FALSE)

# All modes - evening

mode <- c("WALK", "BICYCLE", "TRANSIT")
max_walk_dist <- 1500
max_trip_duration <- 120
departure_datetime <- as.POSIXct("17-07-2019 20:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")
time_window <- 20
ttm_ct_allmodes_allparks_evening <- travel_time_matrix(r5r_core = r5r_core,
                                                       origins = points, # We used the city centroids as origin points. You can only have 3 columns: id, long, lat
                                                       destinations = parks,
                                                       mode =  mode,
                                                       departure_datetime = departure_datetime,
                                                       max_walk_dist = max_walk_dist,
                                                       max_trip_duration = max_trip_duration,
                                                       time_window = time_window,
                                                       verbose = FALSE)


##### Mapping CT scale - all parks #####

# Pulling census geometries



list_census_vectors("CA16") %>%
  filter(str_detect(label, "Land area in square kilometers")) %>%
  pull(vector) -> landarea_v

list_census_regions("CA16") %>%
  filter(level == "CSD", name == "Montréal") %>%
  as_census_region_list() -> r

landarea_mtl_CT <- get_census(dataset = "CA16",
                              regions = r,
                              vector = landarea_v,
                              level = "CT",
                              geo_format = "sf")

censusgeometries <- landarea_mtl_CT[c(3,11)]
censusgeometries

# Adding the travel times to the census geometries table

# Walking
mintravel_ct_walk_allparks <- ttm_ct_walk_allparks %>%
  group_by(fromId) %>%
  summarize(min_travel_time = min(travel_time))

# Biking
mintravel_ct_bike_allparks <- ttm_ct_bike_allparks %>%
  group_by(fromId) %>%
  summarize(min_travel_time_bike = min(travel_time))

# Transit - morning

mintravel_ct_transit_allparks_morning <- ttm_ct_transit_allparks_morning %>%
  group_by(fromId) %>%
  summarize(min_travel_time_transit_morning = min(travel_time))

mintravel_ct_bike_allparks

# Transit - afternoon
mintravel_ct_transit_allparks_afternoon <- ttm_ct_transit_allparks_afternoon %>%
  group_by(fromId) %>%
  summarize(min_travel_time_transit_afternoon = min(travel_time))

# Transit - evening
mintravel_ct_transit_allparks_evening <- ttm_ct_transit_allparks_evening %>%
  group_by(fromId) %>%
  summarize(min_travel_time_transit_evening = min(travel_time))

# All modes - morning
mintravel_ct_allmodes_allparks_morning <- ttm_ct_allmodes_allparks_morning %>%
  group_by(fromId) %>%
  summarize(min_travel_time_allmodes_morning = min(travel_time))

# All modes - afternoon
mintravel_ct_allmodes_allparks_afternoon <- ttm_ct_allmodes_allparks_afternoon %>%
  group_by(fromId) %>%
  summarize(min_travel_time_allmodes_afternoon = min(travel_time))

# All modes - evening

censusgeometries_new <- censusgeometries %>%
  mutate(GeoUID = str_remove(GeoUID, '\\.?0+$'))

join <- merge(mintravel_ct_walk_allparks, censusgeometries_new, by.x = "fromId", by.y = "GeoUID", all.x = FALSE, all.y = TRUE)
join <- merge(mintravel_ct_bike_allparks, join, by.x = "fromId", by.y = "fromId", all.x = FALSE, all.y = TRUE)
join <- merge(mintravel_ct_transit_allparks_morning, join, by.x = "fromId", by.y = "fromId", all.x = FALSE, all.y = TRUE)
join <- merge(mintravel_ct_transit_allparks_afternoon, join, by.x = "fromId", by.y = "fromId", all.x = F, all.y = T)
join <- merge(mintravel_ct_transit_allparks_evening, join, by.x = "fromId", by.y = "fromId", all.x = F, all.y = T)
join <- merge(mintravel_ct_allmodes_allparks_morning, join, by.x = "fromId", by.y = "fromId", all.x = F, all.y = T)
join <- merge(mintravel_ct_allmodes_allparks_afternoon, join, by.c = "fromId", by.y = "fromId", all.x = F, all.y = T)

# Walking
tm_shape(join)+
  tm_fill("min_travel_time")+
  tm_layout(title = "Travel time to the nearest park on foot")

# Biking
tm_shape(join)+
  tm_fill("min_travel_time_bike")+
  tm_layout(title = "Travel time to the nearest park by bike")

# Transit - morning
tm_shape(join)+
  tm_fill("min_travel_time_transit_morning")+
  tm_layout(title = "Travel time to the nearest park by transit (morning")

# Transit - afternoon
tm_shape(join)+
  tm_fill("min_travel_time_transit_afternoon")+
  tm_layout(title = "Travel time to the nearest park by transit (afternoon)")

# Transit - evening
tm_shape(join)+
  tm_fill("min_travel_time_transit_evening")+
  tm_layout(title = "Travel time to the nearest park by transit (evening)")

# All modes - morning
tm_shape(join)+
  tm_fill("min_travel_time_allmodes_morning")+
  tm_layout(title = "Travel time to the nearest park by any mode")

# All modes - afternoon
tm_shape(join)+
  tm_fill("min_travel_time_allmodes_afternoon")+
  tm_layout(title = "Travel time to the nearest park by any mode (afternoon)")

class(join)
st_geometry(join) <- join$geometry


##### DA scale - all parks #####

# Walking

ttm_DA_walk_allparks
mintravel_da_walk_allparks <- ttm_DA_walk_allparks %>%
  group_by(fromId) %>%
  summarize(min_travel_time_walk = min(travel_time))

mintravel_da_walk_allparks
mintravel_da_walk_allparks <- merge(mintravel_da_walk_allparks, da_geometries, by.x = "fromId", by.y = "GeoUID", all.x = F, all.y = T)
mintravel_da_walk_allparks <- st_as_sf(mintravel_da_walk_allparks)
mintravel_da_walk_allparks
class(mintravel_da_allmodes_allparks)
class(mintravel_da_walk_allparks)

tm_shape(mintravel_da_walk_allparks)+
  tm_fill("min_travel_time_walk", title = "travel time (mins)")+
  tm_layout(title = "Minimum travel time to the nearest park on foot")+
  tm_borders(col = "black", lwd = 0.3)

# Transit - afternoon

mode <- c("TRANSIT")
max_walk_dist <- 1500
max_trip_duration <- 120
departure_datetime <- as.POSIXct("17-07-2019 14:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")
time_window <- 20
ttm_DA_parks_transit_afternoon <- travel_time_matrix(r5r_core = r5r_core,
                                   origins = DA_centroids, # We used the city centroids as origin points. You can only have 3 columns: id, long, lat
                                   destinations = parks,
                                   mode =  mode,
                                   departure_datetime = departure_datetime,
                                   max_walk_dist = max_walk_dist,
                                   max_trip_duration = max_trip_duration,
                                   time_window = time_window,
                                   verbose = FALSE)

# All modes - afternoon

mode <- c("TRANSIT","WALK","BICYCLE")
ttm_DA_parks_allmodes_afternoon <- travel_time_matrix(r5r_core = r5r_core,
                                                     origins = DA_centroids, # We used the city centroids as origin points. You can only have 3 columns: id, long, lat
                                                     destinations = parks,
                                                     mode =  mode,
                                                     departure_datetime = departure_datetime,
                                                     max_walk_dist = max_walk_dist,
                                                     max_trip_duration = max_trip_duration,
                                                     time_window = time_window,
                                                     verbose = FALSE)

mintravel_da_allmodes_allparks <- ttm_DA_parks_allmodes_afternoon %>%
  group_by(fromId) %>%
  summarize(min_travel_time_allmodes_afternoon = min(travel_time))

mintravel_da_allmodes_allparks
da_geometries <- population_mtl_DA[c(6, 15, 16)]
mintravel_da_allmodes_allparks <- merge(mintravel_da_allmodes_allparks, da_geometries, by.x = "fromId", by.y = "GeoUID", all.x = F, all.y = T)
mintravel_da_allmodes_allparks

tm_shape(mintravel_da_allmodes_allparks)+
  tm_fill("min_travel_time_allmodes_afternoon", title = "Travel time (mins)")+
  tm_layout(title = "Minimum time to the nearest park (all modes)")+
  tm_borders(col = "black", lwd = 0.3)

class(mintravel_da_allmodes_allparks)
mintravel_da_allmodes_allparks <- st_as_sf(mintravel_da_allmodes_allparks)


##### Compare with tenant ratio #####

# Bring in data

list_census_vectors("CA16") %>%
  filter(str_detect(label, "Renter")) %>%
  pull(vector) -> tenant_v # pulling the vector with the number of total tenant dwellings

list_census_vectors("CA16") %>%
  filter(str_detect(label, "Total - Private households by tenure")) %>%
  pull(vector) -> households_v # Pulling the vector with the number of total dwellings

list_census_regions("CA16") %>%
  filter(level == "CSD", name == "Montréal") %>%
  as_census_region_list() -> r # pulling the census geometry. 

tenants_mtl_CT <- get_census(dataset = "CA16",
                             regions = r,
                             vector = tenant_v,
                             level = "CT",
                             geo_format = "sf")

households_mtl_CT <- get_census(dataset = "CA16",
                                regions = r,
                                vector = households_v,
                                level = "CT",
                                geo_format = "sf") # The data is at the census tract scale. 

# Tenant proportion

tenants_mtl_CT <- tenants_mtl_CT %>%
  mutate(tenantratio = `v_CA16_4838: Renter`/Dwellings)

tenants_mtl_CT
tenants_mtl_CT <- tenants_mtl_CT[c(3,15,16)]

# Add tenant information to travel time table

tenants_mtl_CT <- tenants_mtl_CT %>%
  mutate(GeoUID = str_remove(GeoUID, '\\.?0+$'))

tenants_mtl_CT <- st_drop_geometry(tenants_mtl_CT)
tenantratio <- merge(tenants_mtl_CT, join, by.x = "GeoUID", by.y = "fromId", all.x = F, all.y = T)
glimpse(tenantratio)

ggplot(tenantratio, aes(x = tenantratio, y = min_travel_time_allmodes_afternoon))+
  geom_point()

ggplot(tenantratio, aes(x = tenantratio, y = min_travel_time))+
  geom_point()+
  geom_smooth()

usethis::use_github()
install.packages("usethis")
