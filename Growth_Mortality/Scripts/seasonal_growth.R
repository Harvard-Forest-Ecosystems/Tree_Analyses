library(tidyverse)
library(here)
library(dplyr)
library(sf)
library(raster)
library(terra)
library(rgdal)


# try to calculate growth rates by season
# read in data 
dat <- read_csv("~/GitHub/Dendrometer/2022_archive/full-tree-summ.csv")

# look at what days of the year censuses normally take place
table(unique(dat$date))

#-----------------------------------------------#
#-----------------------------------------------#
# define cutoffs based on date of measurements----
#-----------------------------------------------#
#-----------------------------------------------#
# early spring= before may 30th
# early summer = before july 15
# summer = before sept 1
# fall = before end of year
early_spring <- 147
early_summer <- 196
summer <- 253
fall <- 365

dat07 <-filter(dat, year =='2005')
table(dat07$doy)
# filter out dead ones
dat <-filter(dat, tree_type != 'dead')

# label each tree by its season
dat <- dat %>% 
  mutate(season = ifelse(doy <= early_spring, 'ear_spr', 'NA'),
         season = ifelse(doy > early_spring & doy <= early_summer, 'ear_sum', season),
         season = ifelse(doy > early_summer & doy <= summer, 'summer', season),
         season = ifelse(doy > summer, 'fall',season))

# remove unnecessary columns
dat <- subset(dat, select = -c(date, tree_type,plot, nindivs))

# Use only the min dbh from each season
dat_try <- dat %>% 
  group_by(year, plottag, season, species) %>%
  summarize(dbh = min(dbh, na.rm = TRUE),
            kgc = min(kgc, na.rm = TRUE),
            basal_area = min(basal_area, na.rm = TRUE))

# remove Inf (they are equivalent to NA in this case so we dont need them)
dat_try <- filter(dat_try, dbh <1000)

# reformat dataframe
dat_wide <- dat_try %>% pivot_wider(names_from = season, values_from = dbh)

# remove 1993
dat_wide <- filter(dat_wide, year!='1993')

# try to group
dat_wide2 <- dat_wide %>%
  group_by(plottag, year,species) %>%
  summarize(ear_spr = mean(ear_spr,na.rm = TRUE),
            ear_sum = mean(ear_sum,na.rm = TRUE),
            summer = mean(summer,na.rm = TRUE),
            fall = mean(fall,na.rm = TRUE))

# remove 1993

dat_wide2 <- dat_wide %>%
  group_by(plottag, year,species) %>%
  summarize(#ear_spr_gr = mean(ear_spr,na.rm = TRUE),
            ear_sum_gr = mean(ear_sum,na.rm = TRUE)-mean(ear_spr,na.rm = TRUE),
            summer_gr = mean(summer,na.rm = TRUE)- mean(ear_sum,na.rm = TRUE),
            fall_gr = mean(fall,na.rm = TRUE)-mean(summer,na.rm = TRUE))

# reformat again
dat_fin <- dat_wide2%>%
  pivot_longer(cols = ends_with('gr'), names_to = "season", values_to = "growth")

# make plots with all seasonal growth
gr_sum <- dat_fin %>%
  group_by(year, season) %>%
  summarize(grow = mean(growth, na.rm = TRUE))

ggplot()+
  geom_line(data=gr_sum, aes(x = year, y = grow,col = season))

# Make plots for red oak
redoak <- filter(dat_fin, species == 'ro')

red_sum <- redoak %>%
  group_by(year, season) %>%
  summarize(grow = mean(growth, na.rm = TRUE))

ggplot()+
  geom_line(data=red_sum, aes(x = year, y = grow,col = season))+
  geom_smooth()

# Make plots for hemlock
hem <- filter(dat_fin, species == 'hem')

hem_sum <- hem %>%
  group_by(year, season) %>%
  summarize(grow = mean(growth, na.rm = TRUE))

ggplot()+
  geom_line(data=hem_sum, aes(x = year, y = grow,col = season))+
  geom_smooth()

