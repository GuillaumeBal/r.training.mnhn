rm(list = ls())

require(magrittr)

# graphiques dont cartes
require(ggplot2)
require(viridis) # palette 
require(ggmap)

# to aggregate some spatial info
require(tidyverse)

#carto
require(sf)
require(sp)
require(raster)
# shapefiles online
require(eurostat)
require(rnaturalearth)

require(Cairo)
#require(ggedit)

wd <- "C:/Users/gbal/Desktop/r.training.mnhn/3.code" %T>%
  setwd()

dir()

# load some base map layers ============================

# get_eurostat_geospatial(resolution = "03") %>% st_transform(., 4326) %>%

# word map
word.map <-
  st_read('0.maps/CNTR_RG_03M_2024_4326.shp') %>% 
  st_make_valid() 

# plot 
ggplot() +
  geom_sf(data = word.map)

# deactivate some features for cropping, s2 can be fussy
sf_use_s2(use_s2 = FALSE) # because et_make_valid not enough

# crop to area europe of lynx metapop
plot(st_geometry(word.map))
locator() # resolution ecra n 100

# crop to europe
eu.map <- 
  word.map %>% #st_make_valid() %>%
  st_crop(.,
          st_bbox(c(xmin = -7, xmax = 11,
                    ymax = 51.5, ymin = 41.75),
                  crs = st_crs(4326)))

# plot eu map
ggplot() +
  geom_sf(data = eu.map)

# all eu sub countries levels
eu.divisions <- 
  st_read('0.maps/ENUTS2.shp') %>% 
  st_crop(.,
          st_bbox(c(xmin = -7, xmax = 11, ymax = 51.5, ymin = 41.75)))

# plot eu fiv data
ggplot() +
  geom_sf(data = eu.divisions)

# make countries from divisions
eu.countries <- 
  eu.divisions %>%
  group_by(.$CNTRY_NAME) %>% summarize()

ggplot() +
  geom_sf(data = eu.countries)

# make europe shores
eu.countries$europe.group <- 'europe' # add a variable to groun based on 
europe.coastline <-
  eu.countries %>% group_by(eu.countries$europe.group) %>% summarize()

# plot eu, some maps issues
ggplot() +
  geom_sf(data = europe.coastline)

# pop esco max range ==========================================================
# add some lynx data

# layer of populations
pop.map <-
  st_read('0.maps/cores.model.shp') %>% st_simplify() %>%  # 'cores.area.model.shp'
  st_transform(., 4326) %>% st_make_valid() 

# plot that with eu
ggplot() +
  geom_sf(data = eu.countries) +
  geom_sf(data = pop.map, aes(fill = code))

# cut t oget lynx in france only
pop.france <- 
  pop.map %>%
  st_intersection(., eu.countries[eu.countries$`.$CNTRY_NAME` == 'France' , ]) # because a part on the sea was not excluded

# quick check
ggplot() +
  geom_sf(data = europe.coastline) +
  geom_sf(data = pop.france, aes(fill = code))

# make maps with only reg with lynx ===========================================

#isolate french regions
fr.reg <- eu.divisions[eu.divisions$CNTRY_NAME == 'France', ]

# isolate reg dep with esco
fr.reg.lynx <- st_overlaps(pop.map %>% st_make_valid(), 
                           fr.reg %>% st_make_valid()) %>% unlist %>% unique %>% sort

# get entirely covered one too
fr.reg.lynx <-
  c(fr.reg.lynx,
    st_covered_by(fr.reg %>% st_make_valid(), pop.map %>% st_make_valid()) %>% c %>%  `!=`(0) %>% which
  ) %>% unique %>% sort

# base map
ggplot() +
  geom_sf(data = europe.coastline) +
  #geom_sf(data = eu.countries[eu.countries$`eu.map$CNTR_CODE` == "FR", ]) +
  geom_sf(data = eu.countries[eu.countries$`.$CNTRY_NAME` == "France", ]) +
  geom_sf(data = pop.map, fill = rainbow(n = dim(pop.map)[1])) +
  geom_sf(data = fr.reg[fr.reg.lynx, ], alpha = .6, fill = 'cornsilk') +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

#===============================================================================
# raster part

dir('0.maps')

# get my raster
hab.rast <- raster('0.maps/mapHabCatRas_EPSG3035.tif')
crs(hab.rast)
plot(hab.rast)

# mask a part base on polygons
pop.lynx.crs2 <- st_transform(x = pop.map, hab.rast %>% st_crs())
europe.coastline.crs2 <- st_transform(x = europe.coastline, hab.rast %>% st_crs())
hab.pop.lynx.rast <- mask(hab.rast, 
     pop.lynx.crs2 )

# convert raster for ggplot
hab.pop.lynx.spdf <- as(hab.pop.lynx.rast, "SpatialPixelsDataFrame")  # Convert to SpatialPixelsDataFrame
hab.pop.lynx.df <- as.data.frame(hab.pop.lynx.spdf)

# plot raster
ggplot() +
  geom_sf(data = europe.coastline.crs2) +
  geom_tile(data = hab.pop.lynx.df, 
            aes(x = x, y = y, fill = mapHabCatL))

# add st_area and st_polygones
# possiblly make polygone from locator


