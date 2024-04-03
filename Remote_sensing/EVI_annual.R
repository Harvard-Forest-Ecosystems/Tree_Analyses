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

# import all files in a single folder as a list 
rastlist <- list.files(path = "EVI_84_12_all", pattern='.tif$', all.files=TRUE, full.names=TRUE)

allrasters <- stack(rastlist)

plot(allrasters, zlim = c(0,1))

# crop to average footprint
r2 <- crop(allrasters, extent(ems[8,]))
r3 <- mask(r2, ems[8,])

plot(r3, zlim = c(0,1))
plot(r3)

# convert to dataframe
evi_trimmedold <-
  as.data.frame(r3, xy = TRUE)

evi_trimold <- pivot_longer(evi_trimmedold, cols = starts_with("evi"), names_to = "evi")

evi_trimold <- evi_trimold %>%
  mutate(year = as.numeric(substr(evi, 5, nchar(evi))),
         year = year+1983)

# try plotting evi_trim by year
ggplot()+
  geom_tile(evi_trimold, mapping = aes(x=x, y=y, fill = value))+
  facet_wrap(~year)+
  theme_minimal()

# group for each year
evi_yearold <- evi_trimold %>%
  group_by(year)%>%
  summarize(evi = mean(value, na.rm = TRUE))

# plot means for good years
ggplot()+
  geom_point(data = evi_yearold, mapping = aes(x=year, y=evi))+
  geom_line(data =evi_yearold, mapping = aes(x=year, y=evi))+
  #geom_errorbar(data = evi_year, aes(x = year, y = evi, ymin = lower, ymax = upper), width = 0.2)+
  theme_minimal()



# import all files in a single folder as a list 
rastlist <- list.files(path = "EVI_99_21_cloudfilt", pattern='.tif$', all.files=TRUE, full.names=TRUE)

allrasters <- stack(rastlist)


#names(allrasters) = c("1999", "2000", "2001", "2002", 
#                     "2003", "2004", "2005", "2006",
#                    "2007", "2008", "2009", "2010", 
#                   "2011", "2012", "2013", "2014", 
#                  "2015", "2016", "2017", "2018", 
#                 "2019", "2020", "2021")

plot(allrasters, zlim = c(0,1))

# crop to average footprint
r2 <- crop(allrasters, extent(ems[8,]))
r3 <- mask(r2, ems[8,])

plot(r3, zlim = c(0,1))
plot(r3)

# convert to dataframe
evi_trimmed <-
  as.data.frame(r3, xy = TRUE)

evi_trim <- pivot_longer(evi_trimmed, cols = starts_with("evi"), names_to = "evi")

evi_trim <- evi_trim %>%
  mutate(year = as.numeric(substr(evi, 5, nchar(evi))),
         year = year+1998)

# try plotting evi_trim by year
ggplot()+
  geom_tile(evi_trim, mapping = aes(x=x, y=y, fill = value))+
  facet_wrap(~year)+
  theme_minimal()

# group for each year
evi_year <- evi_trim %>%
  group_by(year)%>%
  summarize(evi = mean(value, na.rm = TRUE))

# plot means for good years
ggplot()+
  geom_point(data = evi_year, mapping = aes(x=year, y=evi))+
  geom_line(data =evi_year, mapping = aes(x=year, y=evi))+
  #geom_errorbar(data = evi_year, aes(x = year, y = evi, ymin = lower, ymax = upper), width = 0.2)+
  theme_minimal()

ggplot()+
  geom_point(data = evi_year, mapping = aes(x=year, y=evi), color = 'chartreuse4')+
  geom_line(data =evi_year, mapping = aes(x=year, y=evi), color = 'chartreuse4')+
  geom_point(data = evi_yearold, mapping = aes(x=year, y=evi), color = 'purple')+
  geom_line(data =evi_yearold, mapping = aes(x=year, y=evi), color = 'purple')+
  #geom_errorbar(data = evi_year, aes(x = year, y = evi, ymin = lower, ymax = upper), width = 0.2)+
  theme_minimal()


# export for Jackie
write_csv(evi_year, 'evi_annual_landsat.csv')


