
# 1. Import packages ------------------------------------------------------

library(pacman)

p_load(elevatr, giscoR, keyring, rpostgis, RPostgres, sf, terra, tidyverse)

# 2. Load some data ------------------------------------------------------

## 2.1. Study area -------------------

## Slovakia country borders
slovakia_sf <- gisco_get_countries(country = "SK")

## 2.2. Terrain rasters --------------

## Get DEM as SpatRaster, and project to same CRS as study area
dem_sr <- get_elev_raster(
  locations = slovakia_sf,
  z = 8
) |> 
  rast() |> 
  project(crs(slovakia_sf))

## Terrain rasters
slope_sr  <- terrain(dem_sr, "slope", unit = "degrees")
aspect_sr <- terrain(dem_sr, "aspect", unit = "degrees")

## Stack bands
terrain_sr <- c(dem_sr, slope_sr, aspect_sr)
names(terrain_sr) <- c("dem", "slope", "aspect")

## 2.3. Create radom points ----------

## Get random points
set.seed(136)
points_sf <- st_sample(slovakia_sf, size = 5000) |> 
  st_as_sf()

## Extract values of DEM, slope and aspect
terrain_tbl <- terra::extract(terrain_sr, points_sf)

## Add terrain values to spatial object
terrain_sf <- bind_cols(points_sf, terrain_tbl) |> 
  rename(geometry = x)


## Up to here we have:
## - slovakia_sf: polygon with country boundaries
## - terrain_sr: raster with 3 bands (DEM, slope, aspect)
## - terrain_sf: 5000 random points with terrain values



# 3. rpostgis ------------------------------------------------------------

## STEP 1: connect to database
## ---------------------------

## using {keyring}
key_list()
key_set_with_value("my_service", "my_username", "mysecretpassword")
key_get("my_service", "my_username")
key_delete("my_service", "my_username")

## Connect to database
conn <- RPostgres::dbConnect(
  drv      = Postgres(), 
  host     = "localhost",
  dbname   = "test_rpostgis", 
  user     = "postgres",
  port     = 5433,
  password = key_get("rpostgis", "postgres")
)


## STEP 2: install PostGIS extension
## ---------------------------------

## install postgis and postgis_raster
pgPostGIS(conn, raster = TRUE)


## STEP 3: create schema
## --------------------------------

## create schema called "my_first_schema"
dbSchema(conn, "my_first_schema")

## detele schema
dbDrop(conn, "my_first_schema", type = "schema")

## create schema "slovakia"
dbSchema(conn, "slovakia")

## STEP 4: insert/read vectorial data
## ---------------------------------

## check geometry and raster tables in the database
pgListGeom(conn)
pgListRast(conn)

## insert vectorial data
pgWriteGeom(
  conn,
  name      = "slovakia_boundaries",
  data.obj  = slovakia_sf,
  overwrite = TRUE
)

## read it back into R
pgGetGeom(
  conn,
  name = "slovakia_boundaries"
)

## write in different schema
pgWriteGeom(
  conn,
  name      = c("slovakia", "slovakia_boundaries"),
  data.obj  = slovakia_sf
)

## read it back into R
pgGetGeom(
  conn,
  name = c("slovakia", "slovakia_boundaries"),
)


## STEP 5: using clauses
## ----------------------------

## insert points into the DB
pgWriteGeom(
  conn,
  name     = c("slovakia", "terrain_points"),
  data.obj = terrain_sf
)

## read it back into R
pgGetGeom(
  conn,
  name    = c("slovakia", "terrain_points"),
  clauses = glue::glue("
  WHERE slope > 10 AND (aspect > 337.5 OR aspect < 22.5)
  ORDER BY dem DESC
  ")
)


## STEP 6: insert/read raster
## -------------------------------

## insert 
pgWriteRast(
  conn,
  name   = c("slovakia", "terrain_raster"),
  raster = terrain_sr
)

## read table into R
pgGetRast(
  conn,
  name = c("slovakia", "terrain_raster")
)

## read all bands
pgGetRast(
  conn,
  name  = c("slovakia", "terrain_raster"),
  bands = TRUE
)

## read bands 1 and 3
pgGetRast(
  conn,
  name  = c("slovakia", "terrain_raster"),
  bands = c(1, 3)
)

## STEP 7: disconnect from DB
## -----------------------------

dbDisconnect(conn)
