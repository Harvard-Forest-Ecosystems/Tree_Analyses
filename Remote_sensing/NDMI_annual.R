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
rastlistold <- list.files(path = "NDMI_84_12_filt", pattern='.tif$', all.files=TRUE, full.names=TRUE)


allrasters <- stack(rastlistold)

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

evi_trim <- pivot_longer(evi_trimmed, cols = starts_with("NDMI"), names_to = "NDMI")

evi_trimold <- evi_trim %>%
  mutate(year = as.numeric(substr(NDMI, 6, nchar(NDMI))),
         year = year+1983)

ggplot()+
  geom_tile(evi_trimold, mapping = aes(x=x, y=y, fill = value))+
  facet_wrap(~year)+
  theme_minimal()

# remove 1984 cause its sus
evi_trimold <- filter(evi_trimold, year != '1984')

# group for each year
ndmi_yearold <- evi_trimold %>%
  group_by(year)%>%
  summarize(ndmi = mean(value, na.rm = TRUE))

# plot means for good years
ggplot()+
  geom_point(data = ndmi_yearold, mapping = aes(x=year, y=ndmi))+
  geom_line(data =ndmi_yearold, mapping = aes(x=year, y=ndmi))+
  #geom_errorbar(data = ndmi_year, aes(x = year, y = ndmi, ymin = lower, ymax = upper), width = 0.2)+
  theme_minimal()



# import all files in a single folder as a list 
rastlist <- list.files(path = "NDMI_99_21_radfilt", pattern='.tif$', all.files=TRUE, full.names=TRUE)


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

evi_trim <- pivot_longer(evi_trimmed, cols = starts_with("NDMI"), names_to = "NDMI")

evi_trim <- evi_trim %>%
  mutate(year = as.numeric(substr(NDMI, 6, nchar(NDMI))),
         year = year+1998)

# try plotting evi_trim by year
ggplot()+
  geom_tile(evi_trim, mapping = aes(x=x, y=y, fill = value))+
  facet_wrap(~year)+
  theme_minimal()

# group for each year
ndmi_year <- evi_trim %>%
  group_by(year)%>%
  summarize(ndmi = mean(value, na.rm = TRUE))

# plot means for good years
ggplot()+
  geom_point(data = ndmi_year, mapping = aes(x=year, y=ndmi))+
  geom_line(data =ndmi_year, mapping = aes(x=year, y=ndmi))+
  #geom_errorbar(data = ndmi_year, aes(x = year, y = ndmi, ymin = lower, ymax = upper), width = 0.2)+
  theme_minimal()



# plot means for 1984-2011 and 1999-2021
ggplot()+
  geom_point(data = ndmi_year, mapping = aes(x=year, y=ndmi), color = 'chartreuse4')+
  geom_line(data =ndmi_year, mapping = aes(x=year, y=ndmi), color = 'chartreuse4')+
  geom_point(data = ndmi_yearold, mapping = aes(x=year, y=ndmi), color = 'purple')+
  geom_line(data =ndmi_yearold, mapping = aes(x=year, y=ndmi), color = 'purple')+
  #geom_errorbar(data = ndmi_year, aes(x = year, y = ndmi, ymin = lower, ymax = upper), width = 0.2)+
  theme_minimal()


# export for Jackie
write_csv(ndmi_year, 'NDMI_annual_landsat2.csv')


# add on neon NDMI
ndmi_neon <- read_csv('~/GitHub/FP-mapping/neon_ndmi_ems_annual.csv')
ndmi_neon$ndmi <- ndmi_neon$nmdi
ggplot()+
  geom_point(data = ndmi_year, mapping = aes(x=year, y=ndmi))+
  geom_line(data =ndmi_year, mapping = aes(x=year, y=ndmi))+
  geom_point(data = ndmi_neon, mapping = aes(x=year, y=ndmi))+
  geom_line(data =ndmi_neon, mapping = aes(x=year, y=ndmi))+
  #geom_errorbar(data = ndmi_year, aes(x = year, y = ndmi, ymin = lower, ymax = upper), width = 0.2)+
  theme_minimal()


