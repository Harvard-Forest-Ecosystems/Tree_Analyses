library(gridExtra)
library(tidyverse)
library(here)
library(dplyr)
library(sf)
library(raster)
library(terra)
library(rgdal)

# load in EMS plot data
fulldat <- read_csv("~/Documents/GitHub/Dendrometer/2022_archive/full-tree-summ.csv")

#--------------------------------#
#--------------------------------#
#-----heatmap of all species---
#--------------------------------#
#--------------------------------#
fulldat <- filter(fulldat, year != '1993')
years <- unique(fulldat$year)

growth <- data.frame()

for(year in 1:(length(years)-1)){
  current <- years[year]
  future <- years[year+1]
  old <- filter(fulldat, year ==  current & tree_type != "dead")
  new <- filter(fulldat, year == future & tree_type != "dead")
  old_date <- min(old$doy, na.rm = TRUE)
  new_date <- min(new$doy, na.rm = TRUE)
  old <- filter(old, doy <= old_date+20)
  new <- filter(new, doy <= new_date+20)
  old <- subset(old, select = c('plottag', 'dbh','year','species','plot','kgc','basal_area'))
  new <- subset(new, select = c('plottag', 'dbh','year','plot','kgc','basal_area'))
  full <- left_join(old, new, by = "plottag")
  # calculate 
  temp <- full %>%
    group_by(year.x, plot.x) %>%
    summarize(dbh_gr = sum(dbh.y-dbh.x, na.rm = TRUE)/n(),
              basal_gr = sum(basal_area.y-basal_area.x, na.rm=TRUE)/n(),
              kgc_gr = sum(kgc.y-kgc.x, na.rm=TRUE)/n()) %>%
    mutate(pl_row = str_sub(plot.x,1,1),
           pl_col = as.numeric(str_sub(plot.x,2,2)))
  growth <- rbind(growth,temp)
}


# Create heatmaps of growth
ggplot() + 
  geom_tile(growth, mapping = aes(x=pl_row, y=pl_col, fill= dbh_gr)) +
  facet_wrap(~year.x)+
  theme_minimal()+
  ylab('Plot Number')+
  xlab('Plot Letter')+
  labs(fill = "DBH Growth")

ggplot() + 
  geom_tile(growth, mapping = aes(x=pl_row, y=pl_col, fill= basal_gr)) +
  facet_wrap(~year.x)+
  theme_minimal()+
  ylab('Plot Number')+
  xlab('Plot Letter')+
  labs(fill = "Basal Area Growth")

ggplot() + 
  geom_tile(growth, mapping = aes(x=pl_row, y=pl_col, fill= kgc_gr)) +
  facet_wrap(~year.x)+
  theme_minimal()+
  ylab('Plot Number')+
  xlab('Plot Letter')+
  labs(fill = "KgC Growth")

# make boxplots for each year to show range of growth rates
ggplot()+
  geom_boxplot(growth, mapping = aes(x=year.x, y=dbh_gr, group = year.x))+
  ylab('DBH Growth')+
  xlab('Year')+
  theme_minimal()
ggplot()+
  geom_boxplot(growth, mapping = aes(x=year.x, y=basal_gr, group = year.x))+
  ylab('Basal Growth')+
  xlab('Year')+
  theme_minimal()
ggplot()+
  geom_boxplot(growth, mapping = aes(x=year.x, y=kgc_gr, group = year.x))+
  ylab('KgC Growth')+
  xlab('Year')+
  theme_minimal()

range(growth$dbh_gr)

#--------------------------------#
#--------2021-2022---------
growth21 <- filter(fulldat, year == '2021' & doy == '132' & tree_type == 'live')
growth22 <- filter(fulldat, year == '2022' & doy == '117' & tree_type == 'live')

# merge
growth2122 <- inner_join(growth21, growth22, by = 'plottag')

# calculate growth
growth2122 <- growth2122 %>%
  mutate(dbh_gr = dbh.y-dbh.x,
         kgc_gr = kgc.y-kgc.x,
         basal_gr = basal_area.y-basal_area.x)

# make a boxplot showing growth rates by species
ggplot()+
  geom_boxplot(growth2122, mapping = aes(x=species.x, y = dbh_gr))

growth2122 <- filter(growth2122, dbh_gr > -.5)

ggplot()+
  geom_boxplot(growth2122, mapping = aes(x=species.x, y = dbh_gr))

ggplot()+
  geom_boxplot(growth2122, mapping = aes(x=species.x, y = kgc_gr))

ggplot()+
  geom_boxplot(growth2122, mapping = aes(x=species.x, y = basal_gr))


ggplot(growth21, mapping = aes(x=sp, y=dbh_gr, fill = sp))+
  geom_boxplot()+
  ylab('DBH Growth')+
  xlab('Year')+
  theme_minimal()
#--------------------------------#
#-Look at specific species----------
#--------heatmaps---------
#--------------------------------#
#--------------------------------#
# make list of years (remove 1993)
fulldat <- filter(fulldat, year != '1993')
years <- unique(fulldat$year)

hem <- filter(fulldat, species == 'hem')

growth_hem <- data.frame()

for(year in 1:(length(years)-1)){
  current <- years[year]
  future <- years[year+1]
  old <- filter(hem, year ==  current & tree_type != "dead")
  new <- filter(hem, year == future & tree_type != "dead")
  old_date <- min(old$doy, na.rm = TRUE)
  new_date <- min(new$doy, na.rm = TRUE)
  old <- filter(old, doy <= old_date+20)
  new <- filter(new, doy <= new_date+20)
  old <- subset(old, select = c('plottag', 'dbh','year','plot','kgc','basal_area'))
  new <- subset(new, select = c('plottag', 'dbh','year','plot','kgc','basal_area'))
  full <- left_join(old, new, by = "plottag")
  # calculate 
  temp <- full %>%
    group_by(year.x, plot.x) %>%
    summarize(dbh_gr = sum(dbh.y-dbh.x, na.rm = TRUE)/n(),
              basal_gr = sum(basal_area.y-basal_area.x, na.rm=TRUE)/n(),
              kgc_gr = sum(kgc.y-kgc.x, na.rm=TRUE)/n()) %>%
    mutate(pl_row = str_sub(plot.x,1,1),
           pl_col = as.numeric(str_sub(plot.x,2,2)))
  growth_hem <- rbind(growth_hem,temp)
}

# Remove the very high negative growth rate for now
growth_hem <- filter(growth_hem, dbh_gr > -2)

# Create heatmaps of growth
ggplot() + 
  geom_tile(growth_hem, mapping = aes(x=pl_row, y=pl_col, fill= dbh_gr)) +
  facet_wrap(~year.x)+
  scale_fill_gradient(low="chartreuse", high = 'black')+
  theme_minimal()+
  ylab('Plot Number')+
  xlab('Plot Letter')+
  labs(fill = "DBH Growth")

ggplot() + 
  geom_tile(growth_hem, mapping = aes(x=pl_row, y=pl_col, fill= basal_gr)) +
  facet_wrap(~year.x)+
  scale_fill_gradient(low="chartreuse", high = 'black')+
  theme_minimal()+
  ylab('Plot Number')+
  xlab('Plot Letter')+
  labs(fill = "Basal Area Growth")

ggplot() + 
  geom_tile(growth_hem, mapping = aes(x=pl_row, y=pl_col, fill= kgc_gr)) +
  facet_wrap(~year.x)+
  scale_fill_gradient(low="chartreuse", high = 'black')+
  theme_minimal()+
  ylab('Plot Number')+
  xlab('Plot Letter')+
  labs(fill = "KgC Growth")

# make boxplots for each year to show range of growth rates
ggplot()+
  geom_boxplot(growth_hem, mapping = aes(x=year.x, y=dbh_gr, group = year.x),fill = 'chartreuse3')+
  ylab('DBH Growth')+
  xlab('Year')+
  theme_minimal()
ggplot()+
  geom_boxplot(growth_hem, mapping = aes(x=year.x, y=basal_gr, group = year.x),fill = 'chartreuse3')+
  ylab('Basal Growth')+
  xlab('Year')+
  theme_minimal()
ggplot()+
  geom_boxplot(growth_hem, mapping = aes(x=year.x, y=kgc_gr, group = year.x),fill = 'chartreuse3')+
  ylab('KgC Growth')+
  xlab('Year')+
  theme_minimal()


#-------------------------------------#
#-------------------------------------#
#---------------Red Oaks-------------
#-------------------------------------#
#-------------------------------------#
# make list of years (remove 1993)
fulldat <- filter(fulldat, year != '1993')
years <- unique(fulldat$year)

ro <- filter(fulldat, species == 'ro')

growth_ro <- data.frame()

for(year in 1:(length(years)-1)){
  current <- years[year]
  future <- years[year+1]
  old <- filter(ro, year ==  current & tree_type != "dead")
  new <- filter(ro, year == future & tree_type != "dead")
  old_date <- min(old$doy, na.rm = TRUE)
  new_date <- min(new$doy, na.rm = TRUE)
  old <- filter(old, doy <= old_date+20)
  new <- filter(new, doy <= new_date+20)
  old <- subset(old, select = c('plottag', 'dbh','year','plot','kgc','basal_area'))
  new <- subset(new, select = c('plottag', 'dbh','year','plot','kgc','basal_area'))
  full <- left_join(old, new, by = "plottag")
  # calculate 
  temp <- full %>%
    group_by(year.x, plot.x) %>%
    summarize(dbh_gr = sum(dbh.y-dbh.x, na.rm = TRUE)/n(),
              basal_gr = sum(basal_area.y-basal_area.x, na.rm=TRUE)/n(),
              kgc_gr = sum(kgc.y-kgc.x, na.rm=TRUE)/n()) %>%
    mutate(pl_row = str_sub(plot.x,1,1),
           pl_col = as.numeric(str_sub(plot.x,2,2)))
  growth_ro <- rbind(growth_ro,temp)
}


# Create heatmaps of growth
ggplot() + 
  geom_tile(growth_ro, mapping = aes(x=pl_row, y=pl_col, fill= dbh_gr)) +
  facet_wrap(~year.x)+
  scale_fill_gradient(low="red", high = 'black')+
  theme_minimal()+
  ylab('Plot Number')+
  xlab('Plot Letter')+
  labs(fill = "DBH Growth")

ggplot() + 
  geom_tile(growth_ro, mapping = aes(x=pl_row, y=pl_col, fill= basal_gr)) +
  facet_wrap(~year.x)+
  scale_fill_gradient(low="red", high = 'black')+
  theme_minimal()+
  ylab('Plot Number')+
  xlab('Plot Letter')+
  labs(fill = "Basal Area Growth")

ggplot() + 
  geom_tile(growth_ro, mapping = aes(x=pl_row, y=pl_col, fill= kgc_gr)) +
  facet_wrap(~year.x)+
  scale_fill_gradient(low="red", high = 'black')+
  theme_minimal()+
  ylab('Plot Number')+
  xlab('Plot Letter')+
  labs(fill = "KgC Growth")

# make boxplots for each year to show range of growth rates
ggplot()+
  geom_boxplot(growth_ro, mapping = aes(x=year.x, y=dbh_gr, group = year.x),fill = 'red3')+
  ylab('DBH Growth')+
  xlab('Year')+
  theme_minimal()
ggplot()+
  geom_boxplot(growth_ro, mapping = aes(x=year.x, y=basal_gr, group = year.x),fill = 'red3')+
  ylab('Basal Growth')+
  xlab('Year')+
  theme_minimal()
ggplot()+
  geom_boxplot(growth_ro, mapping = aes(x=year.x, y=kgc_gr, group = year.x),fill = 'red3')+
  ylab('KgC Growth')+
  xlab('Year')+
  theme_minimal()

range(growth_ro$dbh_gr)
