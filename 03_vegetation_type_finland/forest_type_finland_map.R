# FOREST TYPE IN FINLAND -------
# 
# Data source: Copernicus Global Land Operations
#

# 1. Load packages --------------------------------------------------------

library(pacman)

p_load(sf, terra, tidyverse, glue, giscoR, geodata, tidyterra, ggnewscale)

# 2. Load data ------------------------------------------------------------

## CRS
my_crs <- "ESRI:104129"

## Download rasters
download_forest_type <- function(url) {
  download.file(
    url = url,
    destfile = glue("03_vegetation_type_finland/{basename(url)}"),
    mode     = "wb"
  )
}
url  <- "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/E020N80/E020N80_PROBAV_LC100_global_v3.0.1_2019-nrt_Forest-Type-layer_EPSG-4326.tif"
url2 <- "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/E020N60/E020N60_PROBAV_LC100_global_v3.0.1_2019-nrt_Forest-Type-layer_EPSG-4326.tif"

download_forest_type(url)
download_forest_type(url2)

## Load raster
ft_base1_sr <- rast(glue("03_vegetation_type_finland/{basename(url)}")) 
ft_base2_sr <- rast(glue("03_vegetation_type_finland/{basename(url2)}"))

## Get Finland
finland_sf <- gisco_get_countries(country = "Finland")

# 3. Prepare data ---------------------------------------------------------

## Merge rasters
ft_base_sr <- merge(ft_base1_sr, ft_base2_sr)

## Crop to boundaries, convert to factor, and reproject
ft_finland_sr  <- crop(
  x    = ft_base_sr,
  y    = finland_sf,
  snap = "in",
  mask = TRUE
) %>% 
  as.factor() %>% 
  project(my_crs)

## Relabel
levels(ft_finland_sr)[[1]][,2] <- c("Unknown", "Evergreen needle leaf",
                                    "Deciduous broad leaf", "Mixed forest")

# 4. DEM and shade --------------------------------------------------------

## Download DEM
dem_sr <- elevation_30s(country = "Fi", path = "03_vegetation_type_finland/")

## Get slope and aspect
slope_sr <- terrain(dem_sr, v = "slope", unit = "radians")
aspect_sr <- terrain(dem_sr, v = "aspect", unit = "radians")

## Shade
shade_sr <- shade(slope_sr, aspect_sr) %>% project(my_crs)

# 5. Visualize ------------------------------------------------------------

ggplot() +
  ## hillshade layer
  # geom_spatraster(data = shade_sr, show.legend = FALSE) +
  # scale_fill_gradientn(colors = grey(0:100/100), na.value = NA) +
  # new_scale_fill() +
  ## new layer
  geom_spatraster(data = ft_finland_sr, show.legend = TRUE) +
  ## scales
  scale_fill_manual(
    values       = c("darkblue", "#006400", "#FFA500", "#BA300E"),
    na.value     = NA,
    na.translate = FALSE
  ) +
  theme_void()















