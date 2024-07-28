# ----------------------------------------------------------------------- #
#
# - Title: Cycling routes and landmarks in Picos de Europa National Park
#   using MapLibre and MapTiles
# - Author: Adrián Cidre
# - Website: https://adrian-cidre.com
#
# You will learn:
# ️- How to unrar files in R
# - Download data from Open Street Maps directly into R
# - Use MapLibre and MapTiler in R
# - Add vectorial layers to MapLibre and MapTiler (points, lines, and polygons),
#   and customize them adding hover and tooltip effects
# - Create a legend
# ----------------------------------------------------------------------- #

# 1. Load packages --------------------------------------------------------

## Load pacman
library(pacman)

## Load rest of the packages
p_load(archive, fs, mapgl, mapview, osmdata, sf, tidyverse)

# 2. Load data ------------------------------------------------------------

## 2.1. National Park ---------------------

## Url to national parks file
url <- "https://www.miteco.gob.es/content/dam/miteco/es/parques-nacionales-oapn/red-parques-nacionales/sig/limites_red_tcm30-452281.rar"

## Define paths for download and unrar 
rar_file <- str_glue("{tempdir()}/{basename(url)}")
dest_dir <- str_remove(rar_file, ".rar")

## Download file
download.file(
  url      = url,
  destfile = rar_file,
  mode     = "wb"
)

# Construct the unrar command
archive_extract(rar_file, dest_dir)

## List files
fs::dir_tree(dest_dir)

## Unrar national parks
try(
  archive_extract(
    paste0(dest_dir, "/limites_red/desc_Red_PN_LIM_Enero2023.rar"), 
    dest_dir
  ), 
  silent = TRUE
)

## List files
fs::dir_tree(dest_dir)

## Read national parks
national_parks_sf <- 
  paste0(dest_dir, "/desc_Red_PN_LIM_Enero2023/Limite_PN_p_b.shp") |> 
  read_sf()

## Filter National Park of Picos de Europa
picos_europa_sf <- national_parks_sf |> 
  filter(
    str_detect(d_Nom_Parq, "Picos de Europa")
  ) |> 
  st_transform(4326)

## Visualize
mapview(picos_europa_sf)

## 2.2. Cycling routes ---------------------

## Check OSM features
available_features()
available_tags("route")

## Cycling routes in the bounding box of the National Park
cycling_routes_osm <- opq(
  bbox = st_bbox(picos_europa_sf)
) |> 
  add_osm_feature(
    key   = "route",
    value = "bicycle"
  ) |> 
  osmdata_sf()

## Extract routes
cycling_routes_sf <- cycling_routes_osm$osm_multilines

## Group by name
cycling_routes_united_sf <- cycling_routes_sf |> 
  group_by(name) |> 
  summarise(
    geometry = st_union(geometry)
  ) 

## Calculate path length, and create labels for the map
cycling_routes_united_sf <- cycling_routes_united_sf |> 
  mutate(
    length = st_length(cycling_routes_united_sf) |> units::set_units(km)
  ) |> 
  mutate(
    label = str_glue("{name} <br> <b>{round(length, 2)} km</b>")
  )

## 2.3. Landmarks --------------------------

## OSM tags for tourism
available_tags("tourism") |> pull(Value)

## Get the viewpoints and attractions
landmarks_osm <- opq(
  bbox = st_bbox(picos_europa_sf)
) |> 
  add_osm_feature(
    key   = "tourism",
    value = c("viewpoint", "attraction")
  ) |> 
  osmdata_sf()

## Extract points, and create a "type" column
landmarks_sf <- landmarks_osm$osm_points |> 
  mutate(
    name = ifelse(
      is.na(name), "Unidentified", name
    ),
    type = if_else(
      str_detect(name, "Mirador"), "Viewpoint", "Other"
    )
  ) |> 
  select(name, type)

# 3. Map ------------------------------------------------------------------

## Colours for points
point_col <- c("lightcoral", "lavender")

## Map using MapLibre and CARTO
maplibre(
  style  = carto_style("voyager"),
)  |> 
  fit_bounds(
    picos_europa_sf,
    animate = TRUE
  ) |>
  add_navigation_control() |> 
  add_fullscreen_control() |> 
  add_fill_layer(
    id                 = "picos_europa",
    source             = picos_europa_sf,
    fill_color         = "transparent",
    fill_opacity       = 1,
    fill_outline_color = "red"
  ) |> 
  add_line_layer(
    id            = "routes",
    source        = cycling_routes_united_sf,
    line_color    = "darkblue",
    line_width    = 2,
    tooltip       = "label",
    hover_options = list(
      line_color = "red",
      line_width = 5
    )
  ) |> 
  add_circle_layer(
    id                    = "landmarks",
    source                = landmarks_sf,
    popup                 = "name",
    circle_opacity        = 1,
    circle_radius         = 6,
    circle_stroke_color   = "black",
    circle_stroke_opacity = 1,
    circle_stroke_width   = 1,
    circle_color          = match_expr(
      column = "type",
      values = landmarks_sf$type |> unique(),
      stops  = point_col
    ),
  ) |> 
  add_categorical_legend(
    legend_title     = "Landmark",
    values           = landmarks_sf$type |> unique(),
    colors           = point_col,
    unique_id        = "landmark_legend",
    circular_patches = TRUE
  ) 

## Map using MapLibre and MapTiler
## ** Needs an API token
maplibre(
  style  = maptiler_style("satellite"),
)  |> 
  fit_bounds(
    picos_europa_sf,
    animate = TRUE
  ) |>
  add_navigation_control() |> 
  add_fullscreen_control() |> 
  add_fill_layer(
    id                 = "picos_europa",
    source             = picos_europa_sf,
    fill_color         = "transparent",
    fill_opacity       = 1,
    fill_outline_color = "red"
  ) |> 
  add_line_layer(
    id            = "routes",
    source        = cycling_routes_united_sf,
    line_color    = "wheat",
    line_width    = 2,
    tooltip       = "label",
    hover_options = list(
      line_color = "red",
      line_width = 5
    )
  ) |> 
    add_circle_layer(
      id                    = "landmarks",
      source                = landmarks_sf,
      popup                 = "name",
      circle_opacity        = 1,
      circle_radius         = 6,
      circle_stroke_color   = "black",
      circle_stroke_opacity = 1,
      circle_stroke_width   = 1,
      circle_color          = match_expr(
        column = "type",
        values = landmarks_sf$type |> unique(),
        stops  = point_col
      ),
    ) |> 
    add_categorical_legend(
      legend_title     = "Landmark",
      values           = landmarks_sf$type |> unique(),
      colors           = point_col,
      unique_id        = "landmark_legend",
      circular_patches = TRUE
    ) 


