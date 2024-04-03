library(tidyverse)
library(here)
library(dplyr)

#------------data-----------------
growth <- read_csv('GitHub/Dendrometer/Data/ems_growth_data.csv')

# subset to only include first 3 plots and red oaks
grow_ro <- growth %>%
  #filter(species == 'ro')%>%
  filter(pl_col <= 3)

# Calculate 2010 and before average growth average
grow_ro_pre2010 <- grow_ro %>%
  filter(year.x < 2011)

grow_ro_pre_ann <- grow_ro_pre2010 %>% 
  group_by(year.x) %>%
  summarise(growth = sum(kgc_gr, na.rm = TRUE)/(10^2 * pi * 21))


before_2011_average_growth <- mean(grow_ro_pre_ann$growth, na.rm = TRUE)

# Now look at after 2010
grow_ro_post_2010 <- grow_ro %>%
  filter(year.x >= 2011)

# calculate annual anomaly
grow_ro_post_ann <- grow_ro_post_2010 %>% 
  group_by(year.x) %>%
  summarise(growth = sum(kgc_gr, na.rm = TRUE)/(10^2 * pi * 21),
            growth_anomaly = growth -before_2011_average_growth)

ggplot()+
  geom_point(grow_ro_post_ann, mapping = aes(y = growth_anomaly, x = year.x), col = 'darkred')+
  geom_line(grow_ro_post_ann, mapping = aes(y = growth_anomaly, x = year.x), col = 'darkred')+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('KgC/ha')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


sum(grow_ro_post_ann$growth_anomaly)


grow_ro <- grow_ro %>% 
  group_by(year.x) %>%
  summarise(growth = sum(kgc_gr, na.rm = TRUE)/(10^2 * pi * 21))

ggplot()+
  geom_point(grow_ro, mapping = aes(y = growth, x = year.x), col = 'darkred')+
  geom_line(grow_ro, mapping = aes(y = growth, x = year.x), col = 'darkred')+
  geom_hline(yintercept = 0)+
  theme_minimal()+
  ylab('KgC/ha')+
  xlab('Year')+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))






#---------------------------------
#-----Try Using ForestGeo---------
#------------data-----------------
#---------------------------------
dat_14 <- read_csv("GitHub/Dendrometer/data/forestgeo_data/hf253-04-stems-2014.csv")

dat_19 <- read_csv("GitHub/Dendrometer/data/forestgeo_data/hf253-05-stems-2019.csv")


# Coordinates of the corner of the Megaplot for mapping
mgeast = 731592.9 # Coordinate easting
mgnorth = 4713221.9  # Coordinate northing


# Create quadrat and coordinate columns
dat_14 <- dat_14 %>%
  mutate(quadrat = stringr::str_pad(quadrat, width=4, pad="0", side="left"),
         coords = paste(gx,gy),
         gx_utm = mgeast+gx,
         gy_utm = mgnorth+gy) %>%
  filter(df.status == 'alive', sp != "deadhw", sp != "deadsw", sp != "betuxx", sp != "uniden") 

dat_19 <- dat_19 %>%
  mutate(quadrat = stringr::str_pad(quadrat, width=4, pad="0", side="left"),
         coords = paste(gx,gy),
         gx_utm = mgeast+gx,
         gy_utm = mgnorth+gy) %>%
  filter(df.status == 'alive', sp != "deadhw", sp != "deadsw", sp != "betuxx", sp != "uniden") 

# calculate kgc
# rename species column to spp
dat_14 <- dat_14 %>%
  rename('spp'='sp')
dat_19 <- dat_19 %>%
  rename('spp'='sp')

# calculate kgc
dat_14$kgc <- calc_kgc(dat_14)

dat_19$kgc <- calc_kgc(dat_19)

# merge datasets based on tree and stem id
dat <- inner_join(dat_14, dat_19, by = c('tree.id','stem.id'))

# calculate growth rate 
redoak <- filter(dat, spp.x == 'querru' & dbh.y < 100)
redoak <- redoak %>%
  mutate(growth_rate = (kgc.y-kgc.x)/5)

total_annual_growth = sum(redoak$growth_rate, na.rm = TRUE)

# forestgeo plot is 3500m2 ha
total_annual_growth_ha = total_annual_growth/3500



# let's average ems growth over 2014-2018
ems_compare <- grow_ro%>%
  filter(year.x >= 2014 & year.x <2019)

total_ems_growth <- sum(ems_compare$kgc_gr, na.rm = TRUE)
ems_meters_grow <- total_ems_growth/(10^2 * pi * 21)
ems_ann <- ems_meters_grow/5
