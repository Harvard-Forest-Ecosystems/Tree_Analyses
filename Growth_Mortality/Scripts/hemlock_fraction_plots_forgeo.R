library(tidyverse)
library(here)
library(dplyr)
library(ggplot2)
library(sp)
library(rgdal)
library(sf)
library(terra)
library(raster)

hemlock <- readOGR('GitHub/FP-Mapping/data/polygons/', 'hemlock_fp_daytime')

# read in 2019 forestgeo data
dat_2019 <- read_csv("GitHub/Dendrometer/data/forestgeo_data/hf253-05-stems-2019.csv")

dat_2019 <- dat_2019%>%
  filter(dbh < 100)

# Coordinates of the corner of the Megaplot for mapping
# Southwest corner
mgeast = 731592.9 # Coordinate easting
mgnorth = 4713221.9  # Coordinate northing

# Create quadrat and coordinate columns
dat_2019 <- dat_2019 %>%
  mutate(quadrat = stringr::str_pad(quadrat, width=4, pad="0", side="left"),
         coords = paste(gx,gy),
         gx_utm = mgeast+gx,
         gy_utm = mgnorth+gy) %>%
  filter(sp == 'tsugca') %>%
  mutate(basal_area = signif(pi*(dbh/2)^2, 4))

# plot with hemlock footprint
hem_sf <- st_as_sf(hemlock[2,])

ggplot() +
  geom_point(dat_2019, mapping = aes(x = gx_utm, y=gy_utm, col = basal_area), size = .2)+
  geom_sf(data=hem_sf, aes(alpha=0))+
  coord_sf(datum=sf::st_crs(32618))+
  theme_minimal()

# do it by quadrat
# Create quadrat coordinates
dat_quad <- dat_2019 %>%
  mutate(quad_row = as.numeric(substr(quadrat,1,2)),
         quad_col = as.numeric(substr(quadrat,3,4)),
          quad_row = quad_row * 20,
         quad_col = quad_col * 20,
         quad_x = mgeast+quad_row,
         quad_y = mgnorth+quad_col)

dat_quad2 <- dat_quad %>%
  group_by(quadrat)%>%
  summarize(quad_y = mean(quad_y, na.rm = TRUE),
            quad_x = mean(quad_x,na.rm = TRUE),
            basal = sum(basal_area, na.rm = TRUE))

# plot quadrats
ggplot() +
  geom_tile(dat_quad2, mapping = aes(x = quad_x, y=quad_y, fill = basal))+
  geom_sf(data=hem_sf, aes(alpha=0))+
  coord_sf(datum=sf::st_crs(32618))+
  theme_minimal()


# find total basal area per quadrat
dat_19 <- read_csv("GitHub/Dendrometer/data/forestgeo_data/hf253-05-stems-2019.csv")

dat_19 <- dat_19%>%
  filter(dbh < 100)

dat_quad_full <- dat_19 %>%
  mutate(quad_row = as.numeric(substr(quadrat,1,2)),
         quad_col = as.numeric(substr(quadrat,3,4)),
         quad_row = quad_row * 20,
         quad_col = quad_col * 20,
         quad_x = mgeast+quad_row,
         quad_y = mgnorth+quad_col,
         basal_area = signif(pi*(dbh/2)^2, 4))


dat_quad_full2 <- dat_quad_full %>%
  group_by(quadrat)%>%
  summarize(quad_y = mean(quad_y, na.rm = TRUE),
            quad_x = mean(quad_x,na.rm = TRUE),
            basal = sum(basal_area, na.rm = TRUE))

# subset columns and rename
dat_quad_f <- subset(dat_quad_full2, select = c('quadrat','basal'))

dat_quad_f <- dat_quad_f %>% rename('basal_total'='basal')
dat_quad2$quadrat <- as.numeric(dat_quad2$quadrat)

# combine
dat_all <- left_join(dat_quad2, dat_quad_f, by = 'quadrat')

# calculate ba fraction
dat_all <- dat_all %>%
  mutate(fract = basal/basal_total)

# plot
ggplot() +
  geom_tile(dat_all, mapping = aes(x = quad_x, y=quad_y, fill = fract))+
  geom_sf(data=hem_sf, aes(alpha=0))+
  coord_sf(datum=sf::st_crs(32618))+
  theme_minimal()
