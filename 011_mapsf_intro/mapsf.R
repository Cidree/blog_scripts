# ----------------------------------------------------------------------- #
#
# - Title: Mapping wildfires using R and mapsf
# - Author: Adrián Cidre
# - Website: https://adrian-cidre.com
#
# You will learn:
# - How to download data with R
# - Create maps with mapsf
# ----------------------------------------------------------------------- #


# 1. Import packages -----------------------------------------------------

library(pacman)

p_load(janitor, mapsf, mapSpain, readxl, sf, tidyverse)

sel_crs <- "EPSG:25829"

# 2. Load data -----------------------------------------------------------

## 2.1. Wildfires data ------------

## url to file
url <- "https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/frq_incendios2006_2015_dd-web_tcm30-525841.xlsx"
wildfires_file <- tempfile(fileext = ".xlsx")

## download file
download.file(
  url      = url,
  destfile = wildfires_file,
  mode     = "wb"
)

## read file into R
wildfires_tbl <- read_xlsx(wildfires_file, sheet = 3) |> 
  clean_names()

## 2.2. Study area -------------------

## Load Spanish municipalities
municipalities_es_sf <- esp_get_munic() |> 
  st_transform(sel_crs)

# 3. Prepare data --------------------------------------------------------

## 3.1. Prepare wildfires -----------

## Filter Ourense municipalities
wildfires_prep_tbl <- wildfires_tbl |> 
  filter(pro_n_ine == "Ourense") |> 
  mutate(
    code = as.character(codigoine)
  ) |> 
  select(
    name = nombre, code, n_fires = total_incendios, ha_burned = total
  )

## 3.2. Spatial -------------------

wildfires_prep_sf <- municipalities_es_sf |> 
  filter(ine.prov.name == "Ourense") |> 
  select(name, code = LAU_CODE) |> 
  left_join(wildfires_prep_tbl, by = join_by(code)) |> 
  mutate(
    n_fires   = if_else(is.na(n_fires), 0, n_fires),
    ha_burned = if_else(is.na(ha_burned), 0, ha_burned),
  )

# 4. Visualize -----------------------------------------------------------

## 4.1. Mapsf defaults -------------

# systemfonts::system_fonts() |> pull(family) |> sort()
par(family = "Roboto")
mf_theme("iceberg", fg = "black", bg = "gray60")


## 4.2. Mapping number of wildfires ---------

## Start template 
mf_shadow(wildfires_prep_sf, "grey80")

## Add basemap
mf_map(wildfires_prep_sf)

## Add map
mf_map(
  wildfires_prep_sf,
  var = "n_fires",
  type = "prop",
  col  = "wheat",
  leg_title = "Number of wildfires",
  leg_frame = TRUE,
  leg_val_cex = .9
)

## Add title
mf_title(
  "Number of wildfires",
  pos = "center",
  tab = FALSE
)




## 4.3. Mapping burned area -----------------

## Start template 
mf_shadow(wildfires_prep_sf, "grey80")

## Add basemap
mf_map(wildfires_prep_sf)

## Add map
mf_map(
  wildfires_prep_sf,
  var = "ha_burned",
  type = "choro",
  pal = "Reds",
  leg_title = "Burned area (ha)",
  leg_frame = TRUE,
  leg_val_cex = .9,
  # leg_horiz = TRUE
  leg_bg = "wheat",
  leg_pos = "bottomleft",
  leg_val_rnd = 0
)

## Add title
mf_title(
  "Hectares burned in Ourense",
  pos = "center",
  tab = FALSE
)

## Add inset map
mf_inset_on("worldmap", fig = c(.8, 1, .1, .3))
mf_worldmap(wildfires_prep_sf)
mf_inset_off()



## 4.4. Mapping both ------------------------

## Start template 
mf_shadow(wildfires_prep_sf, "grey80")


## Add basemap
mf_map(wildfires_prep_sf, col = "gray80")

## Add map
mf_map(
  x           = wildfires_prep_sf,
  var         = c("n_fires", "ha_burned"),
  type        = "prop_choro",
  pal         = "Reds",
  leg_val_rnd = c(0, 0),
  leg_val_cex = .7,
  leg_pos     = "left",
  leg_frame   = TRUE,
  leg_title   = c("Number of wildfires", "Burned area (ha)")   
)

## Add title
mf_title(
  "Total wildfires and burned area in Ourense municipalities, 2006-2015",
  pos  = "right",
  cex  = 1.3,
  font = 2
)

## Add credits
mf_credits(
  "@2024 Adrián Cidre\nSource: MITECO",
  pos = "rightbottom",
  cex = .8
)

## Add labels
# mf_label(
#   wildfires_prep_sf,
#   var     = "name.y",
#   overlap = FALSE,
#   lines   = TRUE
# )

## Add inset worldmap
mf_inset_on(
  "worldmap", 
  fig = c(.75, 1, .1, .35)
)

mf_worldmap(wildfires_prep_sf)

mf_inset_off()

## North arrow and scale
mf_arrow()
mf_scale(pos = "bottomleft", lwd = 2, cex = 1)


















