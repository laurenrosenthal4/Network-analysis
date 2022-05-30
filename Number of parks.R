##### CT scale - all parks #####

# How many parks are accessible within x amount of time?

# 10 mins walk
parkcount_ct_allparks_walk_10 <- ttm_ct_walk_allparks %>%
  group_by(fromId) %>%
  summarise(number_parks_10 = sum(travel_time <= 10, na.rm = TRUE))

# 20 mins walk
parkcount_ct_allparks_walk_20 <- ttm_ct_walk_allparks %>%
  group_by(fromId) %>%
  summarise(number_parks_20 = sum(travel_time <= 20, na.rm = TRUE))

# 30 mins walk
parkcount_ct_allparks_walk_30 <- ttm_ct_walk_allparks %>%
  group_by(fromId) %>%
  summarise(number_parks_30 = sum(travel_time <= 30, na.rm = TRUE))

parkcount_walk <- merge(parkcount_ct_allparks_walk_10, censusgeometries_new, by.x = "fromId", by.y = "GeoUID", all.x = F, all.y = T)
parkcount_walk <- merge(parkcount_ct_allparks_walk_20, parkcount_walk, all.x = F, all.y = T)
parkcount_walk <- merge(parkcount_ct_allparks_walk_30, parkcount_walk, all.x = F, all.y = T)
st_geometry(parkcount_walk) <- parkcount_walk$geometry

# Parks accessible within a 10 min walk
tm_shape(parkcount_walk)+
  tm_fill("number_parks_10")+
  tm_layout(title = "Number of parks accessible within a 10 min walk")

# Parks accessible within a 20 min walk
tm_shape(parkcount_walk)+
  tm_fill("number_parks_20")+
  tm_layout(title = "Number of parks accessible within a 20 min walk")

# Parks accessible within a 30 min walk
tm_shape(parkcount_walk)+
  tm_fill("number_parks_30")+
  tm_layout(title = "Number of parks accessible within a 30 min walk")

# 10 mins, 20 mins, 30 mins by any mode
parkcount_ct_allparks_walk_10 <- ttm_ct_walk_allparks %>%
  group_by(fromId) %>%
  summarise(number_parks_10 = sum(travel_time <= 10, na.rm = TRUE))


##### DA scale - all parks #####

# How many parks can you access within an x minute walk?

# 10 mins walk
parkcount_DA_allparks_walk_10 <- ttm_DA_walk_allparks %>%
  group_by(fromId) %>%
  summarise(number_parks_10 = sum(travel_time <= 10, na.rm = TRUE))

parkcount_DA_allparks_walk_10

# 20 mins walk
parkcount_DA_allparks_walk_20 <- ttm_DA_walk_allparks %>%
  group_by(fromId) %>%
  summarise(number_parks_20 = sum(travel_time <= 20, na.rm = TRUE))

# 30 mins walk
parkcount_DA_allparks_walk_30 <- ttm_DA_walk_allparks %>%
  group_by(fromId) %>%
  summarise(number_parks_30 = sum(travel_time <= 30, na.rm = TRUE))

population_DA_filtered <- population_DA_filtered[c(6,15,16)]
parkcount_walk_DA <- merge(population_DA_filtered, parkcount_DA_allparks_walk_10, by.x = "GeoUID", by.y = "fromId", all.x = T, all.y = F)
parkcount_walk_DA <- merge(parkcount_walk_DA, parkcount_DA_allparks_walk_20, by.x = "GeoUID", by.y = "fromId", all.x = T, all.y = F)
parkcount_walk_DA <- merge(parkcount_walk_DA, parkcount_DA_allparks_walk_30, by.x = "GeoUID", by.y = "fromId", all.x = T, all.y = F)

tm_shape(parkcount_walk_DA)+
  tm_fill("number_parks_10", title = "Number of parks", style = "fixed", breaks = c(0, 1, 3, 10, 20, 50, 75))+
  tm_layout(title = "No. parks accessible within 10 mins on foot")+
  tm_borders(col = "black", lwd = 0.3)

tm_shape(parkcount_walk_DA)+
  tm_fill("number_parks_20", title = "Number of parks", style = "fixed", breaks = c(0, 1, 3, 10, 20, 50, 75))+
  tm_layout(title = "No. parks accessible within 20 mins on foot")+
  tm_borders(col = "black", lwd = 0.3)

max(parkcount_walk_DA$number_parks_30)

tm_shape(parkcount_walk_DA)+
  tm_fill("number_parks_30", title = "Number of parks", style = "fixed", breaks = c(0, 1, 3, 10, 20, 50, 130))+
  tm_layout(title = "No. parks accessible within 30 mins on foot")+
  tm_borders(col = "black", lwd = 0.3)

# How many parks are accessible within x minutes by any mode of transit?

# Parks accessible within a 10 minute trip

parkcount_da_allparks_allmodes_10 <- ttm_DA_parks_allmodes_afternoon %>%
  group_by(fromId) %>%
  summarise(parkcount_10 = sum(travel_time < 10, na.rm = T))

# Parks accessible within a 20 minute trip

parkcount_da_allparks_allmodes_20 <- ttm_DA_parks_allmodes_afternoon %>%
  group_by(fromId) %>%
  summarise(parkcount_20 = sum(travel_time < 20, na.rm = T))

# Parks accessible within a 30 minute trip

parkcount_da_allparks_allmodes_30 <- ttm_DA_parks_allmodes_afternoon %>%
  group_by(fromId) %>%
  summarise(parkcount_30 = sum(travel_time < 30, na.rm = T))

parkcount_da_allparks_allmodes_20

parkcount_allmodes_DA <- merge(parkcount_da_allparks_allmodes_10, da_geometries, by.x = "fromId", by.y = "GeoUID", all.x = F, all.y = T)
parkcount_allmodes_DA <- merge(parkcount_allmodes_DA, parkcount_da_allparks_allmodes_20, by.x = "fromId", by.y = "fromId", all.x = T, all.y = T)
parkcount_allmodes_DA <- merge(parkcount_allmodes_DA, parkcount_da_allparks_allmodes_30, by.x = "fromId", by.y = "fromId", alll.x = T, all.y = T)

parkcount_allmodes_DA <- st_as_sf(parkcount_allmodes_DA)

tm_shape(parkcount_allmodes_DA)+
  tm_fill("parkcount_10", title = "Number of parks")+
  tm_layout(title = "Parks accessible within 10 minutes (any mode)")+
  tm_borders(col = "black", lwd = 0.3)

tm_shape(parkcount_allmodes_DA)+
  tm_fill("parkcount_20", title = "Number of parks")+
  tm_layout(title = "Parks accessible within 20 minutes (any mode)")+
  tm_borders(col = "black", lwd = 0.3)

tm_shape(parkcount_allmodes_DA)+
  tm_fill("parkcount_30", title = "Number of parks")+
  tm_layout(title = "Parks accessible within 30 minutes (any mode)")+
  tm_borders(col = "black", lwd = 0.3)

##### Compare with population - DA scale #####

hist(parkcount_walk_DA$number_parks_10)
ggplot(parkcount_walk_DA, aes(x = number_parks_10, y = `v_CA21_1: Population, 2021`))+
  geom_col()

cor(parkcount_walk_DA$number_parks_10, parkcount_walk_DA$`v_CA21_1: Population, 2021`)

##### Compare with median income - DA scale #####

find_census_vectors("median income", data = "CA16", type = "all", query_type = "keyword")

list_census_vectors("CA16") %>%
  filter(str_detect(label, "Median total income in 2015 among recipients")) %>%
  pull(vector) -> median_income

median_income_mtl_DA <- get_census(dataset = "CA16",
                             regions = r,
                             vector = median_income,
                             level = "DA",
                             geo_format = "sf")

median_income_mtl_DA <- median_income_mtl_DA[c(5, 15)]
st_drop_geometry(median_income_mtl_DA)
as.data.frame(median_income_mtl_DA)


