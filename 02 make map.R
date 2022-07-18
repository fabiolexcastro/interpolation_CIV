

# Load libraries  ---------------------------------------------------------
require(pacman)
p_load(raster, terra, sf, geodata, RSAGA, RColorBrewer, tidyverse, gstat, glue, fs, readxl)

rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
fles <- dir_ls('data', regexp = '.tif$')
fles <- grep('idw', fles, value = T)
fles <- as.character(fles)
rstr <- terra::rast(fles)
tble <- terra::as.data.frame(rstr, xy = T)
colnames(tble) <- c('x', 'y', 'v1', 'v2')
tble <- as_tibble(tble)

civs <- gadm(country = 'CIV', level = 1, path = 'tmpr')

# To make the map ---------------------------------------------------------

# First one 
g1 <- ggplot() + 
  geom_tile(data = tble, aes(x = x, y = y, fill = v1)) +
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'Spectral')) + 
  geom_sf(data = st_as_sf(civs), lwd = 0.5, fill = NA, col = 'grey50') +
  coord_sf() + 
  theme_bw() + 
  labs(x = 'Lon', y = 'Lat', fill = 'Pluie deca') +
  theme(legend.position = 'bottom', 
        legend.key.width = unit(3, 'line'))
