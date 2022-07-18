
# Load libraries  ---------------------------------------------------------
require(pacman)
p_load(raster, terra, sf, geodata, RSAGA, tidyverse, gstat, glue, fs, readxl)

rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
tble <- read_excel('data/RESULTATS_2020.xls')
colnames(tble)  

cols <- c('PLUIE DECA', 'Kc=0,5')
tble <- tble[,c(1, 2, 3, grep(paste0(cols, collapse = '|'), colnames(tble)))]

# To download -------------------------------------------------------------
civs <- geodata::gadm(country = 'CIV', level = 1, path = 'tmpr')
plot(civs)
civs$NAME_1

plot(civs, border = 'blue')
points(tble$LONG, tble$LAT, pch = 16, col = 'red')

# To project the shapefile ------------------------------------------------
civs <- terra::project(civs, '+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs')

# Convert the table to shapefile -------------------------------------------
pnts <- tble
pnts <- drop_na(pnts)
pnts <- st_as_sf(pnts, coords = c('LONG', 'LAT'), crs = st_crs(4326))
pnts <- st_transform(pnts, st_crs(32630))

plot(st_geometry(pnts), add = T, col = 'red')
pnts <- terra::vect(pnts)

# Make IDW ----------------------------------------------------------------
x.range <- terra::ext(civs)[1:2]
y.range <- terra::ext(civs)[3:4]
grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 5000),
                   y = seq(from = y.range[1], to = y.range[2], by = 5000))
coordinates(grd) <- ~ x + y
gridded(grd) <- TRUE
raster::crs(grd) <- '+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs'

pnts <- as(pnts, 'Spatial')

colnames(pnts@data) <- c('stt', 'kc', 'pd')

idw.d <- gstat::idw(kc ~ 1, pnts, grd)
idw.d <- raster::raster(idw.d)
idw.d <- rast(idw.d)
idw.d <- terra::crop(idw.d, civs) %>% terra::mask(., civs)
idw.d <- terra::project(idw.d, '+proj=longlat +datum=WGS84 +no_defs')

terra::writeRaster(idw.d, 'data/idw_v2.tif')

# To download -------------------------------------------------------------
# SRTM --------------------------------------------------------------------
srtm <- geodata::elevation_30s(country = 'CIV', path = 'tmpr')
srtm <- terra::project(srtm, crs(idw.d))
terra::writeRaster(srtm, 'data/srtm.tif', overwrite = T)

# GWR ---------------------------------------------------------------------
env <- rsaga.env(path = 'C:/saga-8.0.0_x64')
fle.srt <- 'data/srtm.tif'
fle.inp <- 'data/idw_v2.tif'
fle.out <- 'data/gwr_v2.tif'

rsl <- rsaga.geoprocessor(
  lib = 'statistics_regression', 
  module = 'GWR for Grid Downscaling',
  param = list(PREDICTORS = fle.srt,
               REGRESSION = fle.out,
               DEPENDENT = fle.inp),
  env = env
)

rst <- terra::rast(fle.out)
plot(rst)

