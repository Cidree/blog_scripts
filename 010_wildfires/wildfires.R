# ----------------------------------------------------------------------- #
#
# - Title: Mapping wildfires incidence over the last century
# - Author: Adrián Cidre
# - Website: https://adrian-cidre.com
#
# ----------------------------------------------------------------------- #

# 1. Load packages --------------------------------------------------------

## Load pacman
library(pacman)

## Load rest of the packages
p_load(
  fs, ggspatial, giscoR, mapview, patchwork, sf, terra, tidyterra, tidyverse
)

# 2. Load data ------------------------------------------------------------

## 2.1. Study area ---------------------

## Load Ourense province
ourense_sf <- gisco_get_nuts(
  resolution = "01",
  nuts_level = 3,
  country = "Spain"
) |> 
  filter(NAME_LATN == "Ourense") |> 
  st_transform(25829)

## Convert to SpatVect
ourense_vect <- vect(ourense_sf)

## 2.2. Wildfires data ----------------

## Url to data
url <- "https://maps.effis.emergency.copernicus.eu/effis?service=WFS&request=getfeature&typename=ms:modis.ba.poly&version=1.1.0&outputformat=SHAPEZIP"

## Folder to download and unzip
destfile_zip   <- str_glue("{tempdir()}/effis_data.zip")
destfile_unzip <- str_remove(destfile_zip, ".zip")

## Donwload data
download.file(
  url      = url,
  destfile = destfile_zip,
  mode     = "wb"
)

## Unzip data
unzip(
  zipfile = destfile_zip,
  exdir   = destfile_unzip
)

## Check directory tree
dir_tree(tempdir())

## Read data into R
wildfires_sf <- str_glue("{destfile_unzip}/modis.ba.poly.shp") |> 
  read_sf() |> 
  st_transform(25829)

## 4.3. World -----------------------

## Load world data
world_sf <- gisco_get_countries()

# 3. Prepare data ---------------------------------------------------------

## 3.1. Filter data ----------------------

## Filter Ourense wildfires
wildfires_ourense_sf <- wildfires_sf |> 
  filter(PROVINCE == "Ourense")

## Filter by Intersection
wildfires_ourense_sf <- st_intersection(
  ourense_sf,
  wildfires_sf |> st_make_valid()
)

## 3.2. Wildfires count per pixel -----------

## Create a column with value 1 and convert to SpatVector
wildfires_ourense_sf <- wildfires_ourense_sf |> 
  mutate(
    id   = row_number(),
    fire = 1
  ) |> 
  select(id, fire) 

## Split into a list (easier to map)
wildfires_sf_list <- split(wildfires_ourense_sf, wildfires_ourense_sf$id)

## Create a raster template
template_sr <- rast(
  extent = ext(ourense_vect),
  res    = 100,
  crs    = crs(ourense_vect)
)

## Rasterize each wildfire
## - The wildfire values will become 1
## - The other values will become 0
wildfires_raster_list <- map(
  wildfires_sf_list,
  \(x) rasterize(
    x          = x,
    y          = template_sr,
    field      = "fire",
    background = 0
  ),
  .progress = TRUE
)

## Sum up all the wildfires
wildfires_sr <- reduce(wildfires_raster_list, `+`)

## Convert 0 to NA
wildfires_sr <- ifel(
  wildfires_sr == 0, NA, wildfires_sr
) |> 
  as.factor()

## Plot
plot(wildfires_sr)

## 3.3. Calculate burned area ----------

## Calculate % burned at least once
burned_area <- wildfires_ourense_sf |> 
  st_union() |> 
  st_area()

## Ourense area
ourense_area <- expanse(ourense_vect)

## Percentage of burned area
perc_burned <- round(burned_area / ourense_area * 100, 2)

# 4. Map ------------------------------------------------------------------

## 4.1. Main map ----------------

main_gg <-
  ggplot() +
  geom_spatvector(
    data = ourense_vect,
    fill = "grey15"
  ) +
  geom_spatraster(
    data = wildfires_sr
  ) +
  scale_fill_manual(
    name         = "Number of wildfires",
    values       = hcl.colors(6, "Reds", rev = TRUE),
    na.translate = FALSE
  ) +
  guides(
    fill = guide_legend(
      position       = "inside",
      direction      = "horizontal",
      title.position = "top",
      title.hjust    = .5,
      label.position = "bottom",
      nrow           = 1,
      keywidth       = 2,
      keyheight      = .3,
      units          = "mm"
    )
  ) +
  theme_void() +
  theme(
    text            = element_text(color = "snow"),
    plot.background = element_rect(fill = "black"),
    legend.position.inside = c(.5, .92),
  ) +
  annotation_scale(
    text_col   = "snow",
    style      = "ticks",
    line_col   = "snow",
    line_width = 2,
    text_cex   = 1,
    width_hint = .25
  )

## 4.2. Inset --------------------

inset_gg <- ggplot() +
  geom_sf(
    data = world_sf
  ) +
  geom_sf(
    data = ourense_vect,
    fill = "red"
  ) +
  coord_sf(
    xlim = c(-10, 5),
    ylim = c(35, 45)
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(
      color = "#BCD0C7",
      fill = "black"
    )
  )

## 4.3. Final map ----------------

main_gg +
  inset_element(
    p      = inset_gg, 
    left   = .8,
    right  = 1,
    bottom = 0,
    top    = .2,
    align_to = "full",
  ) +
  plot_annotation(
    title   = str_glue("{perc_burned} % of Ourense province burned at least once in the last century"),
    caption = "Author: Adrián Cidre"
  ) &
  theme(
    text = element_text(color = "snow"),
    plot.title = element_text(
      family = "Merriweather",
      face   = "bold",
      size   = rel(1.7),
      hjust  = .5,
      margin = margin(t = 5, unit = "mm")
    ),
    plot.caption = element_text(
      hjust = .5,
      size   = rel(1)
    ),
    plot.background = element_rect(fill = "black", color = "black")
  ) 
