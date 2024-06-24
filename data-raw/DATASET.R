## code to prepare `DATASET` dataset goes here

## Mountains_df
Mountains_df <- data.frame(
  Summit = c("Fannaråki", "Store Skagastølstind", "Galdhøppigen", "Knutshøe"),
  Lon = c(7.6833, 7.7999, 8.2522, 9.2639),
  Lat = c(61.5831, 61.4573, 61.6333, 61.5125)
)
usethis::use_data(Mountains_df)

## Jotunheimen_poly
Jotunheimen_poly <- sf::st_read("data/Shape/Shape-polygon.shp")
usethis::use_data(Jotunheimen_poly)

## CDS_ras
API_Key <- "Nope"
API_User <- "Nope"
numberOfCores <- 1
Extent_ext <- terra::ext(c(4, 13, 58, 63.5))

CDS_rast <- CDownloadS(
  Variable = "2m_temperature",
  DateStart = "1995-01-01 00:00",
  DateStop = "1995-01-05 23:00",
  TZone = "CET",
  TResolution = "day",
  Extent = Extent_ext, # ext(Jotunheimen_poly)
  Dir = file.path(getwd(), "inst", "extdata"),
  FileName = "CentralNorway",
  API_User = API_User,
  API_Key = API_Key)
usethis::use_data(CDS_rast)
