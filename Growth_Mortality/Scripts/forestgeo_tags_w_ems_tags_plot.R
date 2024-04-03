library(tidyverse)
library(here)
library(dplyr)
library(ggplot2)
library(sp)
library(rgdal)
library(sf)
library(terra)
library(raster)

# read in data
# ems trees with forestgeo tags
tags <- read_csv('GitHub/Dendrometer/forestgeo_tags_dbhs.xlsx.csv')
tags <- tags[!duplicated(tags$Forestgeo_tag), ]
tags <- tags %>%
  rename('emstag' = 'tag',
         'tag' = 'Forestgeo_tag')

# forestgeo data
dat_2014 <- read_csv("GitHub/Dendrometer/data/forestgeo_data/hf253-04-stems-2014.csv")
dat_2019 <- read_csv("GitHub/Dendrometer/data/forestgeo_data/hf253-05-stems-2019.csv")

# subset to only ems trees
trees <- unique(tags$tag)


ems14 <- filter(dat_2014, tag %in% trees)
ems19 <- filter(dat_2019, tag %in% trees)


# add coords
# Coordinates of the corner of the Megaplot for mapping
# Southwest corner
mgeast = 731592.9 # Coordinate easting
mgnorth = 4713221.9  # Coordinate northing

# Create quadrat and coordinate columns
ems14 <- ems14 %>%
  mutate(quadrat = stringr::str_pad(quadrat, width=4, pad="0", side="left"),
         coords = paste(gx,gy),
         gx_utm = mgeast+gx,
         gy_utm = mgnorth+gy)

ems19 <- ems19 %>%
  mutate(quadrat = stringr::str_pad(quadrat, width=4, pad="0", side="left"),
         coords = paste(gx,gy),
         gx_utm = mgeast+gx,
         gy_utm = mgnorth+gy)

# merge with ems data to get plot
ems14_plot <- left_join(ems14, tags, by = 'tag')
ems19_plot <- left_join(ems19, tags, by = 'tag')


# plot
ggplot()+
  geom_point(ems14_plot, mapping = aes(x=gx_utm, y = gy_utm, col = plot))+
  theme_minimal()
