# ----------------------------------------------------------------------- #
#
# - Title: Density map of wolf in the Iberian Peninsula
# - Author: Adrián Cidre
# - Website: https://adrian-cidre.com
#
# ----------------------------------------------------------------------- #

# 1. Load packages --------------------------------------------------------

## Load pacman
library(pacman)

## Load rest of the packages
p_load(geodata, ggnewscale, giscoR, rayshader, sf, terra, tidyterra, tidyverse)

# 2. Load data ------------------------------------------------------------

## 2.1. Study area ----------------------

## Get Portugal
portugal_sf <- gisco_get_countries(
  resolution = "01",
  country    = "Portugal"
) |> 
  st_cast("POLYGON")

## Visualize
plot(st_geometry(portugal_sf))

## Get main land
portugal_sf <- portugal_sf |> 
  mutate(
    area = st_area(portugal_sf)
  ) |> 
  slice_max(area)

## Visualize
plot(st_geometry(portugal_sf))

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

## Visualize
plot(st_geometry(spain_sf))

## Make union
iberia_sf <- st_union(
  spain_sf,
  portugal_sf
) |> 
  st_transform(25830)

## Visualize
plot(st_geometry(iberia_sf))

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

## Visualize
plot(st_geometry(wolf_sf))
plot(st_geometry(iberia_sf), add = TRUE)

## Get data as tibble with coordinates
wolf_tbl <- wolf_sf |> 
  mutate(
    x = st_coordinates(wolf_sf)[, 1],
    y = st_coordinates(wolf_sf)[, 2]
  ) |> 
  as_tibble()

## 3.2. Prepare DEM ---------------

## Convert to tibble
dem_tbl <- dem_sr |> 
  as_tibble(xy = TRUE) |> 
  na.omit() |> 
  rename(elevation = wc2.1_30s_elev)

# 4. Visualize -----------------------------------------------------------

## 4.1. 2D map --------------------------

## Create ggplot2 map
map <- wolf_tbl |> 
  ggplot() +
  geom_raster(
    data = dem_tbl,
    aes(x, y, fill = elevation)
  ) +
  scale_fill_gradientn(
    colours = whitebox.colors(20),
    guide   = guide_colourbar(
      title          = "Elevation (m)",
      title.hjust    = .5,
      position       = "inside",
      barwidth       = unit(4, "mm"),
      barheight      = unit(4, "cm")
    )
  ) +
  new_scale_fill() +
  stat_density_2d(
    aes(x = x, y = y, fill = after_stat(level)), 
    geom = "polygon", 
    alpha = .1,
    bins = 25,
    show.legend = FALSE
  ) +
  geom_point(
    aes(x, y),
    alpha = .3
  ) +
  scale_fill_gradientn(
    colours = c("darkblue", "blue", "green", "yellow", "red"),
    # colours = pal
  ) +
  labs(
    title   = "Where are wolves seen the most in the Iberian Peninsula?",
    caption = "Author: Adrián Cidre | Data source: GBIF (2013-2024)"
  ) +
  theme_void(
    base_size = 10
  ) +
  theme(
    text = element_text(color = "snow"),
    plot.title = element_text(
      family = "Merriweather",
      face   = "bold",
      size   = 12,
      hjust  = .5
    ),
    plot.caption = element_text(
      hjust = .5
    ),
    legend.position.inside = c(.85, .3)
  )

## 4.2. 3D map --------------------------

## Dimensions of the DEM
h <- nrow(dem_sr) 
w <- ncol(dem_sr) 

## Render map in 3D
plot_gg(
  ggobj  = map,
  width  = w / 200,
  height = h / 200,
  scale  = 50,
  solid  = FALSE,
  zoom   = .8,
  phi    = 85,
  theta  = 0,
  shadow = FALSE
)

## Render 3D map in high quality and export
render_highquality(
  filename          = "007_wolf_density/wolf_density.png",
  width             = w,
  height            = h,
  preview           = TRUE,
  light             = TRUE,
  environment_light = "data/env_lights/misty_farm_road_4k.hdr",
  intensity_env     = 1,
  interactive       = FALSE
)



