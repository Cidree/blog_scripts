# CidreRForest
# 01 - Choropleth Maps with ggplot2
#
# -> For weekly updates on my latest blog posts,
#    you can check out my blog: adrian-cidre.com

# 1. Packages -------------------------------------------------------------

# install.packages("pacman")
library(pacman)

p_load(
  ## Core
  tidyverse,
  
  ## Spatial data manipulation
  sf,
  
  ## Download data
  mapSpain, rnaturalearth,
  
  ## Visualization
  RColorBrewer, ggspatial
)

# High resolution world map
remotes::install_github("ropensci/rnaturalearthhires")

# 2. Load data ------------------------------------------------------------

## World countries ----
world_sf <- ne_countries(
  scale       = 10,
  returnclass = "sf"
)

plot(world_sf["region_un"], main = "World Map")

## Get Spanish population by municipality in 2019
spain_pop_tbl <- mapSpain::pobmun19

head(spain_pop_tbl)

## Get Spain boundaries by municipality ----
spain_sf <- esp_get_munic()

head(spain_sf)

## Join population to sf object
spain_pop_sf <- right_join(
  spain_sf,
  spain_pop_tbl,
  by = join_by(cpro, cmun)
)

# 3. Spanish Population Map -----------------------------------------------

## Define the breaks (bin edges) based on percentiles
breaks <- quantile(
  spain_pop_sf$pob19, 
  probs = seq(0, 1, by = 0.1)
)

## Round to hundred, and keep unique values
breaks <- round(breaks, -2) %>% unique()
breaks[length(breaks)] <- breaks[length(breaks)] + 100

## Create bins
spain_pop_ready_sf <- spain_pop_sf %>% 
  mutate(
    pop_bin = cut(pob19, breaks = breaks, dig.lab = 10)
  )

## Map the data
ggplot(spain_pop_ready_sf) + 
  ## Geometries
  geom_sf(data = world_sf, fill = "grey90", color = "black") +
  geom_sf(aes(fill = pop_bin), color = NA) +
  ## Scales
  scale_fill_brewer(palette = "RdBu", na.translate = FALSE, direction = -1) +
  ## Labels
  labs(
    title   = "Spanish Population by Municipality",
    fill    = "Population",
    caption = "Author: Adrián Cidre González"
  ) +
  ## Coordinates
  coord_sf(xlim = st_bbox(spain_pop_ready_sf)[c(1,3)],
           ylim = st_bbox(spain_pop_ready_sf)[c(2,4)]) +
  ## Theme
  theme_bw() +
  theme(
    plot.title        = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.background = element_rect(color = "black")
  ) +
  ## Ggspatial
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", which_north = "true")

# 4. Ratio men-women ------------------------------------------------------

## Calculate ratio
spain_pop_ready_sf <- spain_pop_sf %>% 
  mutate(ratio_mw = men/women)

## Check distribution
summary(spain_pop_ready_sf$ratio_mw)
boxplot(spain_pop_ready_sf$ratio_mw)

# Define the breaks (bin edges) based on percentiles
breaks <- quantile(
  spain_pop_ready_sf$ratio_mw, 
  probs = seq(0, 1, by = 0.1)
)

# Round to hundred, and keep unique values
breaks <- round(breaks, 2) %>% unique()

# Create bins
spain_pop_ready_sf <- spain_pop_ready_sf %>% 
  mutate(
    ratio_bin = cut(ratio_mw, breaks = breaks)
  )

## Plot the data
ggplot(spain_pop_ready_sf) + 
  ## Geometries
  geom_sf(data = world_sf, fill = "grey90", color = "black") +
  geom_sf(aes(fill = ratio_bin), color = NA) +
  ## Scales
  scale_fill_brewer(palette = "RdBu", na.translate = FALSE, direction = -1) +
  ## Labels
  labs(
    title   = "Men/Women ratio",
    fill    = "Ratio",
    caption = "Author: Adrián Cidre González"
  ) +
  ## Coordinates
  coord_sf(xlim = st_bbox(spain_pop_ready_sf)[c(1,3)],
           ylim = st_bbox(spain_pop_ready_sf)[c(2,4)]) +
  ## Theme
  theme_bw() +
  theme(
    plot.title        = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.background = element_rect(color = "black")
  ) +
  ## Ggspatial
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", which_north = "true")
