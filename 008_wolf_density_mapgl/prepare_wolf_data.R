# ----------------------------------------------------------------------- #
#
# - Title: Interactive density map of wolf in the Iberian Peninsula - Part I
# - Author: Adrián Cidre
# - Website: https://adrian-cidre.com
#
# ----------------------------------------------------------------------- #

# 1. Load packages --------------------------------------------------------

## Load pacman
library(pacman)

## Load rest of the packages
p_load(geodata, giscoR, mapgl, sf, terra, tidyverse)

# 2. Load data ------------------------------------------------------------

## 2.1. Study area ----------------------

## Get Portugal
portugal_sf <- gisco_get_countries(
  resolution = "01",
  country    = "Portugal"
) |> 
  st_cast("POLYGON")

## Get main land
portugal_sf <- portugal_sf |> 
  mutate(
    area = st_area(portugal_sf)
  ) |> 
  slice_max(area)

## Get Spain
spain_sf <- gisco_get_countries(
  resolution = "01",
  country    = "Spain"
) |> 
  st_cast("POLYGON")

## Get main land
spain_sf <- spain_sf |> 
  mutate(
    area = st_area(spain_sf)
  ) |> 
  slice_max(area)

## Make union
iberia_sf <- st_union(
  spain_sf,
  portugal_sf
) |> 
  st_transform(25830)

## 2.2. Wolf data -------------------

## Download wolf data
## - Years: 2014-2023
wolf_lst <- map(
  .x = c("ES", "PT"),
  .f = \(x) sp_occurrence(
    genus   = "Canis",
    species = "lupus",
    args    = c(
      paste0("country=", x),
      "year=2014,2023"
    )
  )
)

## 2.3. Elevation -------------------

## Download global DEM, crop it to Iberia, and project it
dem_sr <- elevation_global(res = .5, path = tempdir()) |> 
  crop(
    st_transform(iberia_sf, 4326),
    mask = TRUE
  ) |> 
  project("EPSG:25830")

# 3. Prepare data --------------------------------------------------------

## 3.1. Prepare wolf data ---------------

## Bind data, convert to sf, transform and select points within Iberia
wolf_sf <- list_rbind(wolf_lst) |> 
  st_as_sf(
    coords = c("lon", "lat"),
    crs    = 4326
  ) |> 
  st_transform(25830) |> 
  st_intersection(iberia_sf) |> 
  select(country) 





