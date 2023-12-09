# 1. Load packages --------------------------------------------------------

library(pacman)

p_load(sf, terra, tidyverse, glue, fs, giscoR, geodata, rayshader, classInt)

# 2. Load data ------------------------------------------------------------

## CRS
my_crs <- "ESRI:104129"

## Function to get and delete CH
get_canopy_height <- function(url) {
  ## file
  myfile <- glue("05_finland_tree_height/ch_raster/{basename(gsub('.*ETH_','', url))}")
  ## download file
  download.file(
    url = url,
    destfile = myfile,
    mode     = "wb"
  )
  ## read file
  ch_sr <- rast(myfile)
  ## return
  return(ch_sr)
}

## urls
urls <- c("https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N69E018_Map.tif",
          "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N66E018_Map.tif",
          "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N69E021_Map.tif",
          "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N66E021_Map.tif",
          "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N69E024_Map.tif",
          "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N69E027_Map.tif",
          "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N66E027_Map.tif",
          "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N66E030_Map.tif",
          "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N63E021_Map.tif",
          "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N63E024_Map.tif",
          "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N63E027_Map.tif",
          "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N63E030_Map.tif",
          "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N60E021_Map.tif",
          "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N60E024_Map.tif",
          "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N60E027_Map.tif",
          "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N60E030_Map.tif",
          "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N57E024_Map.tif",
          "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N57E021_Map.tif",
          "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N60E018_Map.tif")

## Apply function
finland_ch_lst <- map(urls, get_canopy_height)

## Get Finland
finland_sf <- gisco_get_countries(country = "Finland")

# 3. Prepare data ---------------------------------------------------------

## 3.1. Get raster for Finland ------------------

## Crop to Finland
ch_crop <- map(finland_ch_lst,
               crop, y = finland_sf, snap = "in", mask = TRUE,
               .progress = TRUE)

## Create mosaic
finland_ch_sr <- reduce(ch_crop, mosaic)

## Aggregate values
finland_aggregated_sr <- finland_ch_sr %>% 
  aggregate(fact = 10)

## Snapshot
writeRaster(finland_aggregated_sr, "")

## 3.2. Reclassify ------------------------------

## Breaks
classIntervals(
  values(finland_ch_sr)
)

## Matrix
















