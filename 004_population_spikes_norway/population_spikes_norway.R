# -------------------------------------------------------------------- #
#
# - Title: 3D Population Spikes Map - Norway
# - Author: Adrián Cidre
# - Website: https://adrian-cidre.com
#
# -------------------------------------------------------------------- #

# 1. Load packages --------------------------------------------------------

library(pacman)

p_load(
  ## Core
  tidyverse,
  
  ## Donwload data
  httr, R.utils,
  
  ## Spatial data
  sf, stars, terra,
  
  ## Visualization
  rayshader, colorspace, MetBrewer
)

# 2. Load data ------------------------------------------------------------

## Create a download/unzip helper
get_data_httr <- function(url, file_name) {
  ## Get url
  httr::GET(
    url,
    write_disk(file_name)
  )
  ## Unzip the gz file
  R.utils::gunzip(file_name, remove = TRUE)
  ## Get the file path
  gsub(".gz", "", file_name)
}

## Url
url <- "https://geodata-eu-central-1-kontur-public.s3.amazonaws.com/kontur_datasets/kontur_population_NO_20231101.gpkg.gz"
## File name
file_name <- "norway-population.gpkg.gz"

## Get file path
file_path <- get_data_httr(url, file_name)

## Read file
population_sf <- 
  read_sf(file_path) %>% 
  st_transform("EPSG:3035")

## Explore
population_sf

# 3. Prepare data ---------------------------------------------------------

## Create function to get width and height
get_raster_size <- function(bbox) {
  ## Get height and width in CRS units
  height <- as.vector(bbox[4] - bbox[2])
  width  <- as.vector(bbox[3] - bbox[1])
  ## Get the ratio between height and width
  if (height > width) {
    height_ratio <- 1
    width_ratio <- width / height
  } else {
    width_ratio <- 1
    height_ratio <- height / width
  }
  
  return(list(
    width  = width_ratio, 
    height = height_ratio)
  )
}

## Get height and width for my data
hw_ratio <- get_raster_size(st_bbox(population_sf))

## Size of greater side
size <- 6000

## Convert to raster with specified dimensions
population_stars <- population_sf %>% 
  select(population) %>% 
  st_rasterize(
    nx = floor(size * hw_ratio$width),
    ny = floor(size * hw_ratio$height)
  )

## Convert to {rayshader} raster matrix
population_matrix <- 
  population_stars %>% 
  rast() %>% 
  raster_to_matrix()

# 4. Visualize ------------------------------------------------------------

## 4.1. Color palette -------------------

## Define palette
pal <- met.brewer("OKeeffe2", n = 10, "continuous")
## Define texture
population_texture <- colorRampPalette(
  colors = pal
)(256)
## Visualize it
swatchplot(population_texture)

## 4.2. Base 3D ------------------------

## Define palette
population_matrix %>% 
  height_shade(texture = population_texture) %>% 
  plot_3d(
    heightmap       = population_matrix,
    solid           = FALSE, 
    soliddepth      = 0,      
    zscale          = 50,
    shadowdepth     = 0,      
    shadow_darkness = .95,    
    windowsize      = c(600, 600),
    zoom            = .6,   
    phi             = 50,    
    theta           = 30,   
    background      = "white"
  )

## Render camera
render_camera(
  zoom  = 0.35,
  theta = 10,
  phi   = 15
)

## 4.3. High quality ------------------

## Render previous map in high quality
rayshader::render_highquality(
  filename       = "004_population_spikes_norway/norway_spikes_map.png",
  preview        = TRUE,
  light          = TRUE,
  lightdirection = c(240, 320),
  lightaltitude  = c(20, 80), 
  lightintensity = c(600, 100),
  lightcolor     = c(lighten(pal[7], 0.75), "white"),
  interactive    = FALSE,
  width          = dim(population_stars)[1], 
  height         = dim(population_stars)[2]
)











