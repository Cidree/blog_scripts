# CidreRForest
# 04 - Analysing Spanish Wildfires by Municipality
#
# -> For weekly updates on my latest blog posts,
#    you can check out my blog: adrian-cidre.com

# 1. Packages -------------------------------------------------------------

library(pacman)

p_load(
  ## Data manipulation
  tidyverse, readxl,
  
  ## Spatial data manipulation
  sf, 
  
  ## Download data
  mapSpain, rnaturalearth,
  
  ## Visualization
  rayshader
)

# 2. Load data ------------------------------------------------------------

## Load data
wildfires_tbl <- read_excel("002_wildfires_ccaa_2006_2015/wildfires_2006_2015.xlsx",
                            sheet = 3) 

## Set names
wildfires_tbl <- wildfires_tbl %>% 
  select(mun = NOMBRE, code = CODIGOINE, small_fires = conatos, wildfires = incendios, 
         total_wildires = TOTAL_INCENDIOS, woodland_area = ARBOLADO, 
         treeless_area = NOARBOLADO, total_area = TOTAL)

# 3. Prepare data ---------------------------------------------------------

## Group by Autonomous Community
wildfires_area_tbl <- wildfires_tbl %>% 
  group_by(code) %>% 
  summarise(total_wildfires = sum(total_wildires),
            total_area = sum(total_area)) %>% 
  ungroup() %>% 
  arrange(desc(total_area))

# 4. Get spatial data -----------------------------------------------------

## Get municipalities
esp_mun_sf <- esp_get_munic() %>% 
  filter(!(ine.ccaa.name %in% c("Melilla", "Ceuta"))) %>% 
  mutate(code = as.numeric(LAU_CODE))

## Join wildfires and municipalities
esp_mun_all <- esp_mun_sf %>% 
  left_join(
    wildfires_area_tbl,
    by = join_by(code == code)
  ) %>% 
  select(name, ine.ccaa.name, total_wildfires, total_area, geometry) %>%
  mutate(
    total_area = if_else(is.na(total_area), 0, total_area),
    total_wildfires = if_else(is.na(total_wildfires), 0, total_wildfires)
  )

# 5. Prepare ggplot2 ------------------------------------------------------

esp_mun_all %>% 
  # filter(ine.ccaa.name == "Galicia") %>% 
  ggplot() +
  # Geometries
  geom_sf(aes(fill = total_area), color = NA) +
  # Scales
  scale_fill_distiller(palette = "Reds", direction = 1) +
  # Labels
  labs(title    = "Burned Area in Spain during 2006-2015",
       subtitle = "Source of data: Spanish Ministry of Agriculture, Food and Environment",
       caption  = "Author: Adrián Cidre González",
       fill     = "Area (ha)") +
  # Theme
  theme_void(base_family = "Source Sans Pro") +
  theme(
    plot.title        = element_text(size = 14, face = "bold", hjust = .95, colour = "indianred4"),
    plot.subtitle     = element_text(size = 8, hjust = .95, colour = "indianred4",
                                   margin = margin(b = 23, t = 5)),
    plot.caption      = element_text(size = 8, colour = "indianred4"),
    legend.title      = element_text(size = 8, colour = "indianred4"),
    legend.text       = element_text(size = 8, colour = "indianred4"),
    legend.position   = "bottom",
    legend.key.width  = unit(1, "cm"),
    legend.key.height = unit(0.3, "cm"),
    plot.background   = element_rect(fill = "#B0C4DE", colour = NA)
    
  ) -> g


# 6. 3D visualization -----------------------------------------------------

plot_gg(g,
        multicore        = T,
        width            = 5,
        height           = 5,
        scale            = 150,
        shadow_intensity = .6,
        sunangle         = 315,
        offset_edges     = T,
        windowsize       = c(1400,866),
        zoom             = .5, 
        phi              = 35, 
        theta            = -30) 
render_snapshot("003_wildfires_by_municipality/wildfires_ha_muni.png", clear = T)


# 7. Number of fires ------------------------------------------------------

esp_mun_all %>% 
  # filter(ine.ccaa.name == "Galicia") %>% 
  ggplot() +
  # Geometries
  geom_sf(aes(fill = total_wildfires), color = NA) +
  # Scales
  scale_fill_distiller(palette = "Reds", direction = 1) +
  # Labels
  labs(title    = "Number of Wildfires in Spain during 2006-2015",
       subtitle = "Source of data: Spanish Ministry of Agriculture, Food and Environment",
       caption  = "Author: Adrián Cidre González",
       fill     = "") +
  # Theme
  theme_void() +
  theme(
    plot.title      = element_text(size = 14, face = "bold", hjust = .95, colour = "indianred4"),
    plot.subtitle   = element_text(size = 8, hjust = .95, colour = "indianred4",
                                   margin = margin(b = 20, t = 5)),
    plot.caption    = element_text(size = 8, colour = "indianred4"),
    legend.title    = element_text(size = 8, colour = "indianred4"),
    legend.text     = element_text(size = 8, colour = "indianred4"),
    legend.position = "bottom",
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.3, "cm"),
    plot.background = element_rect(fill = "#B0C4DE", colour = NA)
    
  ) -> g2


plot_gg(g2,
        multicore = T,
        width = 5,
        height = 5,
        scale = 150,
        shadow_intensity = .75,
        sunangle = 360,
        offset_edges = T,
        windowsize = c(1400,866),
        zoom = .5, 
        phi    = 35, 
        theta  = -30)
render_snapshot("003_wildfires_by_municipality/wildfires_number_muni.png", clear = T)


