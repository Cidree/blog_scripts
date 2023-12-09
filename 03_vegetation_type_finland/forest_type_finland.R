# FOREST TYPE IN FINLAND -------
# 
# Data source: Copernicus Global Land Operations
#

# 1. Load packages --------------------------------------------------------

library(pacman)

p_load(sf, terra, tidyverse, glue, giscoR, png, geodata, rayshader)

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

## 3.1. Get extension --------------

## Merge rasters
ft_base_sr <- merge(ft_base1_sr, ft_base2_sr)

## Crop to boundaries
ft_finland_sr  <- crop(
  x    = ft_base_sr,
  y    = finland_sf,
  snap = "in",
  mask = TRUE
) %>% project(my_crs)

## 3.2. Convert to image -----------

## Colours for different classes in RGB matrix
cols <- c("lightblue", "#006400", "#FFA500", "#BA300E")
from <- c(0,1,4,5)
to <- t(col2rgb(
  cols
))

## Remove NA values
ft_finland_sr <-
  na.omit(ft_finland_sr)

forest_type_sr <- subst(
  x = ft_finland_sr,
  from,
  to,
  names = cols
)

plotRGB(forest_type_sr)

writeRaster(forest_type_sr, "03_vegetation_type_finland/finland-forest.png",
            overwrite = TRUE, NAflag = 255)

## 
img <- readPNG("03_vegetation_type_finland/finland-forest.png")

# 4. Elevation data -------------------------------------------------------

## Download elevation map
dem_finland_sr <- elevation_30s(
  country = "FI",
  path    = "03_vegetation_type_finland/"
) %>% project(my_crs)

## Convert to rayshader matrix
dem_matrix <- raster_to_matrix(dem_finland_sr)

# 5. Render scene ---------------------------------------------------------

## Parameters
height <- nrow(dem_finland_sr)
width  <- ncol(dem_finland_sr)

## Create scene
dem_matrix %>% 
  height_shade(
    texture = colorRampPalette("snow")(512)
  ) %>% 
  add_overlay(
    img,
    alphalayer = .9,
    alphacolor = "snow"
  ) %>% 
  add_shadow(
    lamb_shade(
      dem_matrix,
      zscale      = 10,
      sunaltitude = 90,
      sunangle    = 315
    ), max_darken = .25
  ) %>% 
  add_shadow(
    texture_shade(
      dem_matrix,
      detail     = .95,
      brightness = 90,
      contrast   = 80
    ), max_darken = .1
  ) %>% 
  plot_3d(
    dem_matrix,
    zscale          = 5,
    solid           = FALSE,
    shadow          = TRUE,
    shadow_darkness = 1,
    background      = "white",
    windowsize      = c(
      width / 5, height / 5
    ),
    zoom            = .5,
    phi             = 85,
    theta           = 0
  )




















