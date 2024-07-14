# CidreRForest
# 02 - Analyzing Spanish Wildfires
#
# -> For weekly updates on my latest blog posts,
#    you can check out my blog: adrian-cidre.com

# 1. Packages -------------------------------------------------------------

# install.packages("pacman")
library(pacman)

p_load(
  ## Core
  tidyverse, readxl, stringi,
  
  ## Spatial data manipulation
  sf,
  
  ## Download data
  mapSpain, rnaturalearth,
  
  ## Visualization
  RColorBrewer, ggspatial
)

# 2. Load data ------------------------------------------------------------

## Load data
wildfires_tbl <- read_excel("002_wildfires_ccaa_2006_2015/wildfires_2006_2015.xlsx",
                 sheet = 3) 

## Set names
wildfires_tbl <- wildfires_tbl %>% 
  select(ccaa = COM_N_INE, small_fires = conatos, wildfires = incendios, 
         total_wildires = TOTAL_INCENDIOS, woodland_area = ARBOLADO, 
         treeless_area = NOARBOLADO, total_area = TOTAL)


# 3. Analyse number of wildfires by CA ------------------------------------

## Group by Autonomous Community
wildfires_area_tbl <- wildfires_tbl %>% 
  group_by(ccaa) %>% 
  summarise(total_wildfires = sum(total_wildires),
            total_area = sum(total_area)) %>% 
  ungroup() %>% 
  mutate(
    ccaa       = fct_reorder(ccaa, total_wildfires),
    label_perc = scales::percent(total_wildfires / sum(total_wildfires),
                                 accuracy = .01)
    ) %>%
  arrange(desc(total_area))

wildfires_area_tbl %>% 
  head(10) %>% 
  ggplot(aes(x = ccaa, y = total_wildfires)) +
  geom_col(fill = "steelblue") +
  geom_label(aes(label = label_perc)) +
  coord_flip() +
  expand_limits(y = 40000) +
  labs(x = NULL,
       y = "Total Wildfires",
       title = "Wildfires by Autonomous Community (2006-2015)",
       subtitle = "Top 10 Autonomous Communities in Spain",
       caption = "Author: Adrián Cidre González") +
  annotate(
    "rect", xmin = 7.5, xmax = 10.5, ymin = -500, ymax = 41000,
    fill = NA, color = "indianred4", lwd = 1
  ) +
  annotate(
    "text",
    x = 8.5, y = 30000, 
    label = "The northwest of Spain \nsuffers the 58% of \nthe wildfires",
    color = "indianred4"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# map ---------------------------------------------------------------------

##
esp_ccaa_sf <- esp_get_ccaa() %>% 
  filter(!(iso2.ccaa.name.es %in% c("Melilla", "Ceuta")))

##
rdy <- esp_ccaa_sf %>% 
  mutate(nuts2.name = nuts2.name %>% stri_trans_general("Latin-ASCII")) %>% 
  mutate(nuts2.name = case_when(
    nuts2.name == "Comunidad Valenciana" ~ "Comunitat Valenciana",
    nuts2.name == "Cataluna" ~ "Cataluña",
    .default = nuts2.name
  )) %>% 
  inner_join(
    wildfires_area_tbl,
    by = join_by(nuts2.name == ccaa)
  ) 

ggplot(rdy) +
  geom_sf(aes(fill = total_area)) +
  scale_fill_distiller(palette = "Reds", direction = 1)

# -------------------------------------------------------------------------

wildfires_tbl %>% 
  pivot_longer(
    cols     = woodland_area:treeless_area,
    names_to = "woodland",
    values_to = "burned_area"
  ) %>% 
  mutate(
    woodland = if_else(woodland == "woodland_area", "Woodland", "Treeless") %>% 
      as_factor()
  ) %>% 
  
  group_by(ccaa, woodland) %>% 
  summarise(burned_area = sum(burned_area)) %>% 
  ungroup() %>% 
  mutate(ccaa = fct_reorder(ccaa, burned_area)) %>% 
  
  
  # Plot
  ggplot(aes(x = burned_area, y = ccaa)) +
  geom_col(fill = "indianred4") +
  
  labs(
    x = "Burned area (ha)",
    y = NULL
  ) +
  
  facet_wrap(~ woodland, scales = "free_y") +
  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))






















