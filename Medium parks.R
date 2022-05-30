url <- "https://data.montreal.ca/dataset/2e9e4d2f-173a-4c3d-a5e3-565d79baa27d/resource/35796624-15df-4503-a569-797665f8768e/download/espace_vert.json"
temp <- tempfile()
download.file(url, temp)

allparks <- read_sf(temp)
allparks <- allparks %>%
  filter(TYPO1 == "Parc d'arrondissement" | TYPO2 == "Grand parc")
allparks
allparks <- allparks[c(1,4, 12)]
allparks <- allparks %>%
  mutate(parkarea = st_area(allparks))

allparks$parkarea <- units::drop_units(allparks$parkarea)

mediumparks <- allparks %>%
  filter(parkarea >= 100000)

mediumparks_centroids %>%
  
  

