# -------------------------------------------------------------------- #
#
# - Title: Pinus sylvestris potential distribution under two climate change
# scenarios
# - Author: Adrián Cidre
# - Website: https://adrian-cidre.com
#
# -------------------------------------------------------------------- #

# 1. Load packages --------------------------------------------------------

## Install development version of forestdata
remotes::install_github("Cidree/forestdata")

## Load pacman
library(pacman)

## Load rest of the packages
p_load(
  forestdata, ggtext, patchwork, terra, tidyterra, tidyverse
)


# 2. Load data ------------------------------------------------------------

## Load Pinus sylvestris data for 2005
psylvestris_2005_sr <- fd_forest_eutrees4f(
  species  = "Pinus sylvestris",
  period   = 2005,
  type     = "bin",
  distrib  = "pot"
)

## Load Pinus sylvestris data for 2095 (rcp45)
psylvestris_2095_rcp45_sr <- fd_forest_eutrees4f(
  species  = "Pinus sylvestris",
  period   = 2095,
  scenario = "rcp45"
)

## Load Pinus sylvestris data for 2095 (rcp85)
psylvestris_2095_rcp85_sr <- fd_forest_eutrees4f(
  species  = "Pinus sylvestris",
  period   = 2095,
  scenario = "rcp85"
)


# 3. Prepare data ---------------------------------------------------------

## 3.1 Reclassify --------------------------------

## Create a function to reclassify each layer
reclassify_rcp <- function(raster, classes) {
  
  ## Reclassify 1 for 2
  raster_class <- ifel(
    raster == 1, 2, raster
  )
  ## Sum to current (2005) distribution
  raster_class <- as.factor(raster_class + psylvestris_2005_sr)
  ## Rename levels
  levels(raster_class)[[1]][, 2] <- classes
  raster_class
  
}

## Classes and colors for each class
ps_classes <- c("Absent", "Will disappear", "Will appear",  "Present and stable")
ps_colors  <- c("#BE92A2", "#96031A", "#6DA34D", "#CEEDDB")

## Apply function
psylvestris_rcp45_sr <- reclassify_rcp(psylvestris_2095_rcp45_sr, ps_classes)
psylvestris_rcp85_sr <- reclassify_rcp(psylvestris_2095_rcp85_sr, ps_classes)

## 3.2. Exploratory visualization -----------------

## Visualize
plot(psylvestris_rcp45_sr, col = ps_colors)
plot(psylvestris_rcp85_sr, col = ps_colors)


# 4. Visualization --------------------------------------------------------

## Create a helper function
map_rcp <- function(data, title = "(a) RCP 4.5") {
  
  ggplot() +
    geom_spatraster(
      data = data,
      show.legend = FALSE
    ) +
    scale_fill_manual(
      values   = ps_colors,
      na.value = NA
    ) +
    labs(
      title = title
    ) +
    theme_void(base_family = "Roboto") +
    theme(
      plot.title = element_text(
        face = "bold", hjust = .5, color = "snow"
      )
    )
  
}

## Create maps
rcp45_gg <- map_rcp(psylvestris_rcp45_sr)
rcp85_gg <- map_rcp(psylvestris_rcp85_sr, title = "(b) RCP 8.5")

## Wrappers for {ggtext}
absent_txt   <- str_glue("<b style = 'color: {ps_colors[1]};'>almost absent</b>")
decrease_txt <- str_glue("<b style = 'color: {ps_colors[2]};'>decrease</b>")
increase_txt <- str_glue("<b style = 'color: {ps_colors[3]};'>shift</b>")
present_txt  <- str_glue("<b style = 'color: {ps_colors[4]};'>present</b>")

## Plot title
title_txt <- str_glue(
  "*Pinus sylvestris*, {absent_txt} in southern Europe and {present_txt} in Northern and Central Europe, is<br>
  projected to {increase_txt} its potential distribution northward and {decrease_txt} in Central Europe under two<br>
  climatic scenarios by 2095"
)

## Final maps
ps_gg <- rcp45_gg + 
  rcp85_gg +
  plot_annotation(
    title   = title_txt,
    caption = "Author: Adrián Cidre | Data source: EU-Trees4F",
    theme   = theme(
      plot.title = element_markdown(
        family     = "Merriweather",
        face       = "bold",
        lineheight = 1.2,
        margin     = margin(t = 5, l = 5, b = 10)
      ),
      plot.caption = element_text(
        hjust  = .5,
        family = "Roboto"
      ),
      plot.background = element_rect(
        fill = "gray10",
        colour = "gray10"
      )
    )
  ) 

## Export
ggsave(
  filename = "05_pinus_sylvestris_rcp_map/psylvestris_rcp.png",
  plot     = ps_gg,
  width    = 25,
  height   = 20,
  units    = "cm"
)







