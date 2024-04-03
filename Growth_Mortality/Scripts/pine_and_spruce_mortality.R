library(tidyverse)
library(here)
library(dplyr)
library(ggplot2)
library(sp)
library(rgdal)
library(sf)
library(terra)
library(raster)

# fps
hemlock <- readOGR('GitHub/FP-Mapping/data/polygons/', 'hemlock_fp_daytime')
ems <- readOGR('GitHub/FP-Mapping/data/polygons/', 'ems_fp_daytime')

# read in ndmi data from neon for ems and hem fps
ann_hem <- read_csv('~/GitHub/FP-mapping/neon_ndmi_hemlock_annual.csv')
raw_hem <- read_csv('~/GitHub/FP-mapping/neon_ndmi_hemlock_raw.csv')

# try to make a raster
raw_hem_ras <- subset(raw_hem, select = c('x','y','value', 'year'))
hem_ras <- rasterFromXYZ(raw_hem_ras)

ann_ems <- read_csv('~/GitHub/FP-mapping/neon_ndmi_ems_annual.csv')
raw_ems <- read_csv('~/GitHub/FP-mapping/neon_ndmi_ems_raw.csv')

# read in forestgeo shp file of growth
geogrowth_shp <- readOGR("GitHub/FP-Mapping/data/forestgeo_shp/", 'geo_growth')
geomort_shp <- readOGR("GitHub/FP-Mapping/data/forestgeo_shp/", 'geo_mort')

# crop 
hem_grow <- geogrowth_shp[hemlock[2,],]
hem_dead <- geomort_shp[hemlock[2,],]
ems_grow <- geogrowth_shp[ems[8,],]
ems_dead <- geomort_shp[ems[8,],]


# convert back to df
grow_ems_df <- as.data.frame(ems_grow)
grow_hem_df <- as.data.frame(hem_grow)
mort_ems_df <- as.data.frame(ems_dead)
mort_hem_df <- as.data.frame(hem_dead)

# grab ndmi for each tree location
grow_hem_df$ndmi <- extract(hem_ras, hem_grow)
mort_hem_df$ndmi <- extract(hem_ras, hem_dead)

# subset to red pine
mort_hem_redpin <- filter(mort_hem_df, spp_y == 'pinure')

plot(mort_hem_redpin$gx_tm_x, mort_hem_redpin$gy_tm_x)

ggplot()+
  geom_point()


# look at red pine and spruce distribution
# red pine = pinure
# red spruce = piceru
# unknown spruce = picexx
# norway spruce = piceab





