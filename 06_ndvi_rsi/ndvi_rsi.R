# ----------------------------------------------------------------------- #
#
# - Title: Creating easy NDVI and NDMI maps using Sentinel-2 through {rsi}
# - Author: Adrián Cidre
# - Website: https://adrian-cidre.com
#
# ----------------------------------------------------------------------- #

# 1. Load packages --------------------------------------------------------

## Load pacman
library(pacman)

## Load rest of the packages
p_load(
  giscoR, patchwork, sf, rsi, terra, tidyterra, tidyverse
)

# 2. Load data ------------------------------------------------------------

## 2.1. Study area ----------------------

## Get study area
tenerife_sf <- gisco_get_nuts(
  country    = "Spain",
  resolution = "01",
  nuts_level = 3
) |> 
  filter(
    NAME_LATN == "Tenerife"
  ) |> 
  st_transform(25828)

## 2.2. Satellite image ----------------

## Band names
sel_bands <- sentinel2_band_mapping$planetary_computer_v1[c("B02", "B03", "B04", "B08", "B11")]

## Download Sentinel-2 data
tic()
tenerife_s2 <- get_sentinel2_imagery(
  aoi             = tenerife_sf,
  start_date      = "2023-07-15",
  end_date        = "2023-08-15",
  asset_names     = sel_bands,
  output_filename = glue::glue("{tempdir()}/sentinel.tif")
)
toc()

## Read data
tenerife_sr <- rast(tenerife_s2) / 10000

## Visualize RGB
plotRGB(
  tenerife_sr,
  3, 2, 1,
  scale   = .3,
  stretch = "lin"
)

# 3. Prepare data ---------------------------------------------------------

## 3.1. Mask island --------------------------

tenerife_sr <- mask(tenerife_sr, tenerife_sf)

## 3.1. Select indices ------------------------

## Available indices?
spectral_indices() |> 
  glimpse()

## Select indices
indices_tbl <- spectral_indices() |> 
  filter(
    short_name %in% c("NDMI", "NDVI")
  )

## 3.2. Calculate indices ---------------------

## Calculate indices
indices_path <- calculate_indices(
  raster          = tenerife_sr,
  indices         = indices_tbl,
  output_filename = glue::glue("{tempdir()}/spectral_indices.tif")
)

## Read indices
indices_sr <- rast(indices_path)

## 3.3. Classify NDMI -----------------------

## Classification matrix for NDMI
ndmi_mat <- matrix(
  c(
    -Inf, -.2, 1,
    -.2, 0, 2,
    0, .2, 3,
    .2, .4, 4,
    .4, Inf, 5
  ),
  ncol  = 3,
  byrow = TRUE
)

## Classify NDMI
indices_sr$NDMI <- indices_sr$NDMI |> 
  classify(
    rcl = ndmi_mat
  ) |> 
  as.factor()

## Label values
levels(indices_sr$NDMI)[[1]][, 2] <- c(
  "Low or very low canopy cover, dry or very low canopy cover, wet",
  "Mid-low canopy cover, high water stress or low canopy cover, low water stress",
  "Average canopy cover, high water stress or mid-low canopy cover, low water stress",
  "Mid-high canopy cover, high water stress or average canopy cover, low water stress",
  "No water stress"
)

# 4. Visualize ------------------------------------------------------------

## Visualize NDMI
ndmi_gg <- ggplot() +
  geom_spatraster(
    data = indices_sr$NDMI
  ) +
  scale_fill_whitebox_d(
    palette   = "bl_yl_rd",
    direction = -1
  ) +
  guides(
    fill = guide_legend(
      position = "inside",
      title    = NULL
    )
  ) +
  theme_void(
    base_size   = 8,
    base_family = "Roboto"
  ) +
  theme(
    legend.position.inside = c(.3, .8),
    legend.key.spacing.y   = unit(2, "mm"),
    legend.key.width       = unit(5, "mm"),
    legend.key.height      = unit(1, "mm"),
    legend.key             = element_rect(colour = "black", linewidth = .2),
    legend.text            = element_text(size = 5),
  )

## Visualize NDVI
ndvi_gg <- ggplot() +
  geom_spatraster(
    data = indices_sr$NDVI
  ) +
  scale_fill_whitebox_c(
    palette   = "muted",
    direction = -1
  ) +
  guides(
    fill = guide_colorbar(
      position       = "inside",
      title          = "NDVI",
      title.position = "top",
      title.hjust    = .5,
      direction      = "horizontal"
    )
  ) +
  theme_void(
    base_size   = 8,
    base_family = "Roboto"
  ) +
  theme(
    legend.position.inside = c(.3, .8),
    legend.key.width       = unit(1, "cm"),
    legend.key.height      = unit(1.5, "mm"),
    legend.text            = element_text(size = 5),
    legend.title           = element_text(size = 8)
  )

## Put plots together
ndvi_gg + 
  ndmi_gg +
    plot_annotation(
      title    = "Pre-Wildfire Vegetation and Moisture Analysis of Tenerife",
      subtitle = "NDVI and NDMI averaged from 15-July 2023 to 15-August 2023",
      caption  = "Author: Adrián Cidre | https://adrian-cidre.com | Data source: Sentinel-2",
      theme    = theme(
        plot.title = element_text(
          family     = "Merriweather",
          face       = "bold",
          lineheight = 1.2,
          margin     = margin(t = 5, l = 5, b = 10),
          hjust = .5
        ),
        plot.subtitle = element_text(hjust = .5),
        plot.caption  = element_text(
          hjust  = .5,
          family = "Roboto"
        )
      )
    ) 

## Save the plot
ggsave(
  filename = "06_ndvi_rsi/tenerife_indexes.png",
  height   = 14,
  width    = 30,
  units    = "cm"
)
