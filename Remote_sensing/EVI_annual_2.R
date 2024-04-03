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

# read in landsat 7 processed evi
landsat7_evi <- read_csv("GitHub/Dendrometer/data/evi_summer.csv")

# process landsat 5 evi
#first import all files in a single folder as a list 
rastlist <- list.files(path = "EVI_summer_84_12", pattern='.tif$', all.files=TRUE, full.names=TRUE)

allrasters <- stack(rastlist)

plot(allrasters, zlim = c(0,1))

# crop to average footprint
r2 <- crop(allrasters, extent(ems[8,]))
r3 <- mask(r2, ems[8,])

plot(r3, zlim = c(0,1))
plot(r3)

# convert to dataframe
evi_trimmed <-
  as.data.frame(r3, xy = TRUE)%>%
  #--- remove cells with NA for any of the layers ---#
  na.omit()

evi_trim <- pivot_longer(evi_trimmed, cols = starts_with("EVI"), names_to = "EVI")

evi_trim <- evi_trim %>%
  mutate(year = as.numeric(substr(EVI, 5, nchar(EVI))),
         year = year+1983)


# try plotting evi_trim by year
ggplot()+
  geom_tile(evi_trim, mapping = aes(x=x, y=y, fill = value))+
  facet_wrap(~year)+
  theme_minimal()

# plot mean of each year
evi_year <- evi_trim %>%
  group_by(year)%>%
  summarize(evi = mean(value, na.rm = TRUE))#,
            #sem = sd(value, na.rm = TRUE)/sqrt(n()),
            #upper = evi+sem,
            #lower = evi-sem)

# export
write_csv(evi_year, "GitHub/Dendrometer/data/evi_summer_1984_2002.csv")

# combine with newer data
evi_all <- rbind(evi_year,landsat7_evi)

# plot
ggplot()+
  geom_point(data = evi_year, mapping = aes(x=year, y=evi))+
  geom_line(data =evi_year, mapping = aes(x=year, y=evi))+
  #geom_errorbar(data = evi_year, aes(x = year, y = evi, ymin = lower, ymax = upper), width = 0.2)+
  theme_minimal()


# calculate mean of each year (to take care of overlapping years)
evi_year <- evi_all %>%
  group_by(year)%>%
  summarize(evi = mean(evi, na.rm = TRUE))#,
#sem = sd(value, na.rm = TRUE)/sqrt(n()),
#upper = evi+sem,
#lower = evi-sem)

# plot
ggplot()+
  geom_point(data = evi_all, mapping = aes(x=year, y=evi))+
  geom_line(data =evi_all, mapping = aes(x=year, y=evi))+
  #geom_errorbar(data = evi_year, aes(x = year, y = evi, ymin = lower, ymax = upper), width = 0.2)+
  theme_minimal()

