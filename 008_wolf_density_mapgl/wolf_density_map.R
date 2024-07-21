# ----------------------------------------------------------------------- #
#
# - Title: Interactive density map of wolf in the Iberian Peninsula - Part II
# - Author: Adrián Cidre
# - Website: https://adrian-cidre.com
#
# ----------------------------------------------------------------------- #

# 1. Load packages --------------------------------------------------------

## Load pacman
library(pacman)

## Install mapgl
# pak::pak("walkerke/mapgl")

## Load rest of the packages
p_load(geodata, giscoR, mapboxapi, mapgl, sf, terra, tidyverse)

# 2. Mapbox API set-up ----------------------------------------------------

## Mapbox API token (set only once)
# mb_access_token("your_token_goes_here", install = TRUE)

# 3. Load data ------------------------------------------------------------

## Source script
source("008_wolf_density_mapgl/prepare_wolf_data.R")

## Visualize loaded data
plot(st_geometry(iberia_sf))
plot(dem_sr, add = TRUE)
plot(st_geometry(wolf_sf), add = TRUE)

# 4. Visualization --------------------------------------------------------

## Basic mapboxgl
mapboxgl()

## Tweak parameters
mapboxgl(
  style  = mapbox_style("satellite"),
  center = c(-7, 42),
  zoom   = 5
) |> 
  fly_to(
    center = c(-7, 42),
    zoom   = 10
  )

## Create first heatmap
mapboxgl(
  style = mapbox_style("dark")
) |> 
  add_heatmap_layer(
    id     = "wolves-heatmap",
    source = wolf_sf
  )

## Create second heatmap
mapboxgl(
  style = mapbox_style("satellite")
) |> 
  fit_bounds(iberia_sf) |> 
  add_heatmap_layer(
    id     = "wolves-heatmap",
    source = wolf_sf,
    heatmap_intensity = interpolate(
      property = "zoom",
      values   = c(0, 10),
      stops    = c(0, 2),
    ),
    heatmap_opacity = .5
  )

## Add flying effect
mapboxgl(
  style = mapbox_style("satellite")
) |> 
  fly_to(
    center = c(-3.6, 40.5),
    zoom   = 5
  ) |> 
  add_heatmap_layer(
    id     = "wolves-heatmap",
    source = wolf_sf,
    heatmap_intensity = interpolate(
      property = "heatmap-density",
      values   = c(0, 1),
      stops    = c(1, 2),
    ),
    heatmap_opacity = .5
  )














