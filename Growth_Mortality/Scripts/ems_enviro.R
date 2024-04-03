library(tidyverse)
library(here)
library(dplyr)
library(sf)
library(raster)
library(terra)
library(rgdal)

# load in environmental data-------------
soils <- readOGR('GitHub/Dendrometer/enviro_data', layer = 'soils')
elev <- raster('GitHub/Dendrometer/enviro_data/dblbnd.adf')

# calculate slope and aspect from elevation data
slopeaspect <- terrain(elev, opt = c('slope', 'aspect'), unit='degrees')

# load in tree data and coordinates (UPDATED TO INCLUDE 2022 DATA)
treedat <- read_csv("~/Documents/GitHub/Dendrometer/2022_archive/ems-tree-summ.csv")
coords2 <- read_csv("~/Documents/GitHub/Dendrometer/EMS_plots_utm.csv")

treedat <- read_csv("~/GitHub/Dendrometer/2022_archive/ems-tree-summ.csv")
coords2 <- read_csv("~/GitHub/Dendrometer/growth_mort_analyses/EMS_plots_utm.csv")

treedat$spp <- treedat$species

# calculate kgc and basal area for growth
treedat <- treedat %>% 
  rename(basal_area = basal_area_cm2,
         kgc = biomass_kgc,
         dbh = dbh_cm)


# make a shape file from coords
coord <- coords2[,c('E.1',"N.1")]
crs1 <- CRS("+proj=utm +zone=18 +datum=WGS84")
dat <- SpatialPointsDataFrame(coords = coord,
                              data = coords2,
                              proj4string = crs1)
plot(dat)

# reproject to match elevation CRS
newcrs <- crs(elev)
dat2 <- spTransform(dat,newcrs)

# Plot to check overlap
plot(elev)
plot(dat2, add = TRUE)
plot(soils, add = TRUE)

# Extract topographic data for every EMS plot
coords2 <- coords2 %>% mutate(elevation = raster::extract(elev, dat2),
                              slope = raster::extract(slopeaspect$slope, dat2),
                              aspect = raster::extract(slopeaspect$aspect, dat2))


# Add environmental data to full dataframe
coords2$plot <- coords2$Plot
fulldat <- left_join(treedat, coords2, by = c("plot"))
fulldat <- fulldat %>%
  mutate(northness = cos(aspect))
# filter out understory
fulldat <- filter(fulldat, tree_type != "und")

# extract soils data 
dat3 = st_as_sf(dat2)
soils2 = st_as_sf(soils)

## intersect polygons with points, keeping the information from both
soil_df = st_intersection(dat3, soils2)

# remove unnecessary cols
soil_df <- subset(soil_df, select = c('Plot','SIMMONS_SO','MESIC_SOIL','DRAINAGE_C',
                                      'TYPE_'))
# merge with full dataset
coords3 <- inner_join(coords2,soil_df,by = 'Plot')

# Add environmental data to full dataframe
coords3$plot <- coords3$Plot
fulldat <- left_join(treedat, coords3, by = c("plot"))

# calculate northness
fulldat <- fulldat %>% 
  mutate(northness = cos(aspect))


soils <- fulldat[!duplicated(fulldat[,c('TYPE_')]),]
soils <- subset(soils, select = c('SIMMONS_SO','MESIC_SOIL',
                                  'DRAINAGE_C',"TYPE_"))
write_csv(soils, "GitHub/Dendrometer/soils_description.csv")
#----------------------------------------------------#
#----------------------------------------------------#
# Calculate growth rates----
#----------------------------------------------------#
#----------------------------------------------------#
# idea: loop through every year and find the growth rate
temp <- filter(fulldat,year != '1993')
years <- unique(temp$year)

growth <- data.frame()

for(year in 1:(length(years)-1)){
  current <- years[year]
  future <- years[year+1]
  print(current)
  print(future)
  old <- filter(fulldat, year ==  current & tree_type != "dead")
  new <- filter(fulldat, year == future & tree_type != "dead")
  old_date <- min(old$doy, na.rm = TRUE)
  new_date <- min(new$doy, na.rm = TRUE)
  old <- filter(old, doy <= old_date+14)
  new <- filter(new, doy <= new_date+14)
  old <- subset(old, select = c('plottag', 'dbh','year','plot','elevation','species','slope','northness','kgc','basal_area','TYPE_'))
  new <- subset(new, select = c('plottag', 'dbh','year','plot','kgc','basal_area'))
  full <- left_join(old, new, by = "plottag")
  # calculate 
  temp <- full %>%
    mutate(dbh_gr = dbh.y-dbh.x,
              basal_gr = basal_area.y-basal_area.x,
              kgc_gr = kgc.y-kgc.x,pl_row = str_sub(plot.x,1,1),
           pl_col = as.numeric(str_sub(plot.x,2,2)))
  growth <- rbind(growth,temp)
} 

# subset red oaks
grow_ro <- subset(growth, species == 'ro')

# remove understory trees and x plots
cutplots <- c('X1','X2','X3','X4','X5', 'X6')
grow_ro <- subset(grow_ro, !plot.x %in% cutplots)
grow_ro$year <- grow_ro$year.x
grow_ro$plot <- grow_ro$plot.x

# run linear regressions
summary(lm(dbh_gr~elevation, data = grow_ro))
summary(lm(dbh_gr~slope, data = grow_ro))
summary(lm(dbh_gr~northness, data = grow_ro))


summary(lm(basal_gr~elevation, data = grow_ro))
summary(lm(basal_gr~slope, data = grow_ro))
summary(lm(basal_gr~northness, data = grow_ro))


summary(lm(kgc_gr~elevation, data = grow_ro))
summary(lm(kgc_gr~slope, data = grow_ro))
summary(lm(kgc_gr~northness, data = grow_ro))

#------------------------------------------------#
#------------------------------------------------#
#--------Random Effect Models----------
#------------------------------------------------#
#------------------------------------------------#
library(lme4)
dbhlm <- lm(dbh_gr~ year, data = grow_ro)
summary(dbhlm)
dbhlm1 <- lmer(dbh_gr~ plot + (1|year), data = grow_ro)
ranef(dbhlm1)
summary(dbhlm1)

dbhlm2 <- lmer(dbh_gr~ plot + slope + northness+ elevation+ (1|year), data = grow_ro)
summary(dbhlm2)

AIC(dbhlm1)
AIC(dbhlm2)

basallm <- lmer(basal_gr~ plot + (1|year), data = grow_ro)
summary(basallm)

kgclm <- lmer(kgc_gr~ plot + (1|year), data = grow_ro)
summary(kgclm)


# second model
dbhlm <- lmer(dbh_gr~ year+(1 | plot/plottag), data = grow_ro)
summary(dbhlm)

dbhlm <- lmer(dbh_gr~ year+(1 | plot), data = grow_ro)
summary(dbhlm)

























# heatmaps, only work if you summarize by plot
ggplot() + 
  geom_tile(growth, mapping = aes(x=pl_row, y=pl_col, fill= dbh_gr)) +
  facet_wrap(~year.x)

ggplot() + 
  geom_tile(growth, mapping = aes(x=pl_row, y=pl_col, fill= basal_gr)) +
  facet_wrap(~year.x)

ggplot() + 
  geom_tile(growth, mapping = aes(x=pl_row, y=pl_col, fill= kgc_gr)) +
  facet_wrap(~year.x)

#--------------------------------#
#--------------------------------#
#-Look for relationships between growth and enviro----------
#--------------------------------#
#--------------------------------#
summary(lm(dbh_gr~elevation, data = growth))
summary(lm(dbh_gr~slope, data = growth))
summary(lm(dbh_gr~aspect, data = growth))
summary(lm(dbh_gr~TYPE_, data = growth))

summary(lm(basal_gr~elevation, data = growth))
summary(lm(basal_gr~slope, data = growth))
summary(lm(basal_gr~aspect, data = growth))
summary(lm(basal_gr~TYPE_, data = growth))

summary(lm(kgc_gr~elevation, data = growth))
summary(lm(kgc_gr~slope, data = growth))
summary(lm(kgc_gr~aspect, data = growth))
summary(lm(kgc_gr~TYPE_, data = growth))

anov <- aov(kgc_gr~TYPE_, growth)
summary(anov)

#--------------------------------#
#--------------------------------#
#-Look at red oak----------
#--------------------------------#
#--------------------------------#
grow_ro <- filter(growth, species == 'ro')

summary(lm(dbh_gr~elevation, data = grow_ro))
summary(lm(dbh_gr~slope, data = grow_ro))
summary(lm(dbh_gr~aspect, data = grow_ro))
summary(lm(dbh_gr~TYPE_, data = grow_ro))

summary(lm(basal_gr~elevation, data = grow_ro))
summary(lm(basal_gr~slope, data = grow_ro))
summary(lm(basal_gr~aspect, data = grow_ro))
summary(lm(basal_gr~TYPE_, data = grow_ro))

summary(lm(kgc_gr~elevation, data = grow_ro))
summary(lm(kgc_gr~slope, data = grow_ro))
summary(lm(kgc_gr~aspect, data = grow_ro))
summary(lm(kgc_gr~TYPE_, data = grow_ro))


TukeyHSD(aov(log(kgc_gr+0.0001)~TYPE_, data = grow_ro))

anov <- aov(kgc_gr~TYPE_, grow_ro)
summary(anov)


# make some maps-------

# soils
# average by soil type
soil_sum <- grow_ro %>% 
  group_by(TYPE_) %>%
  summarize(dbh_m = mean(dbh_gr, na.rm = TRUE),
            bas_m = mean(basal_gr, na.rm = TRUE),
            kgc_m = mean(kgc_gr, na.rm = TRUE))

soil_sum <- na.omit(soil_sum)

grow_ro <- na.omit(grow_ro)
ggplot()+
  geom_boxplot(data = grow_ro, aes(x = TYPE_, y = dbh_gr, fill = TYPE_))

ggplot()+
  geom_boxplot(data = grow_ro, aes(x = TYPE_, y = basal_gr, fill = TYPE_))+
  theme_minimal()+
  ylab(expression("Basal Area Growth (cm" ^ 2*")"))+
  xlab("Soil Type")+
  labs(fill = "Soil Type")

ggplot()+
  geom_boxplot(data = grow_ro, aes(x = TYPE_, y = log(basal_gr), fill = TYPE_))+
  theme_minimal()+
  ylab(expression("Logged Basal Area Growth (cm" ^ 2*")"))+
  xlab("Soil Type")+
  labs(fill = "Soil Type")

ggplot()+
  geom_boxplot(data = grow_ro, aes(x = TYPE_, y = kgc_gr, fill = TYPE_))+
  theme_minimal()+
  ylab("Biomass Growth (KgC)")+
  xlab("Soil Type")+
  labs(fill = "Soil Type")

ggplot()+
  geom_boxplot(data = grow_ro, aes(x = TYPE_, y = log(kgc_gr), fill = TYPE_))+
  theme_minimal()+
  ylab("Logged Biomass Growth (KgC)")+
  xlab("Soil Type")+
  labs(fill = "Soil Type")


ggplot()+
  geom_boxplot(data = grow_ro, aes(x = TYPE_, y = kgc_gr, col = TYPE_))

ggplot()+
  geom_point(data = grow_ro, aes(x = year.x, y = kgc_gr, col = TYPE_))+
  facet_wrap(~TYPE_)

grow_ro$year.x <- as.character(grow_ro$year.x)

grow_ro$year.x <- as.numeric(grow_ro$year.x)
ggplot()+
  geom_boxplot(data = grow_ro, aes(x = year.x, y = kgc_gr, group = year.x, col = TYPE_))+
  facet_wrap(~TYPE_)+
  theme_minimal()



ggplot()+
  geom_boxplot(data = grow_ro, aes(x = year.x, y = kgc_gr, col = TYPE_))

grow_ro_summ <- 
  grow_ro %>%
  group_by(year.x) %>%
  summarize(kgc = mean(kgc_gr, na.rm = TRUE),
            basal = mean(basal_gr, na.rm =TRUE),
            dbh = mean(dbh_gr, na.rm = TRUE))
ggplot()+
  geom_point(data = grow_ro_summ, aes(x = year.x, y = kgc))+
  geom_line()

ggplot()+
  geom_point(data = grow_ro_summ, aes(x = year.x, y = basal))+
  geom_line()

ggplot()+
  geom_point(data = grow_ro_summ, aes(x = year.x, y = dbh))+
  geom_line(data = grow_ro_summ, aes(x = year.x, y = dbh))

#--------------------------------#
#--------------------------------#
#-Super plots or super trees?---------
#--------------------------------#
#--------------------------------#
# make a tag column
grow_ro <- grow_ro %>% 
  mutate(tag = str_sub(plottag, 4,5))

grow_ro_20 <- filter(grow_ro, year.x == 2020)
ggplot()+
  geom_point(data = grow_ro_20, aes(x = plot.x, y = tag, size = dbh_gr, col = dbh_gr))+
  facet_wrap(~year.x)

ggplot()+
  geom_point(data = grow_ro_20, aes(x = tag, y = dbh_gr, size = dbh_gr, col = dbh_gr))+
  facet_wrap(~plot.x)

# check to see if number of trees in a plot affects growth rate
# summarize
grow_ro_plot <- grow_ro %>%
  group_by(plot.x, year.x, pl_row,pl_col) %>%
  summarize(dbh = mean(dbh_gr, na.rm = TRUE),
            kgc = mean(kgc_gr, na.rm = TRUE),
            basal = mean(basal_gr, na.rm = TRUE),
            n_trees = n(),
            rang = range(basal_gr, na.rm = TRUE))

# filter out 1998 bc it seems weird?
grow_ro_plot <- filter(grow_ro_plot, year.x != 1998)

ggplot()+
  geom_point(data = grow_ro_plot, aes(x = pl_row, y = pl_col, size = dbh, col = dbh))+
  facet_wrap(~year.x)

ggplot()+
  geom_point(data = grow_ro_plot, aes(x = pl_row, y = pl_col, size = rang, col = rang))+
  facet_wrap(~year.x)

ggplot()+
  geom_point(data = grow_ro_plot, aes(x = pl_row, y = pl_col, size = n_trees, col = n_trees))+
  facet_wrap(~year.x)

ggplot()+
  geom_point(data = grow_ro_plot, aes(x = pl_row, y = pl_col, size = basal, col = basal))+
  facet_wrap(~year.x)

# run a linear regression to see if number of trees affects growth
dbh_lm <- lm(dbh~n_trees, grow_ro_plot)
summary(dbh_lm)

basal_lm <- lm(basal~n_trees, grow_ro_plot)
summary(basal_lm)

kgc_lm <- lm(kgc~n_trees, grow_ro_plot)
summary(kgc_lm)





#--------------------------------#
#--------------------------------#
#--------Climate and soil---------
#--------------------------------#
#--------------------------------#

# annual and monthly climate
clim <- read_csv("~/Documents/GitHub/Dendrometer/archive_data/hf300-01-annual-m.csv")
mon_clim <- read_csv("~/Documents/GitHub/Dendrometer/archive_data/hf300-03-monthly-m.csv")

clim <- read_csv("~/GitHub/Dendrometer/archive_data/hf300-01-annual-m.csv")
mon_clim <- read_csv("~/GitHub/Dendrometer/archive_data/hf300-03-monthly-m.csv")

# create month and year column
mon_clim <- mon_clim %>%
  mutate(month = str_sub(date, 6,7),
         year = as.numeric(str_sub(date, 1,4)),
         prev_year = as.numeric(str_sub(date, 1,4)))

clim <- clim %>%
  mutate(prev_year = year)

# make different climate metrics
# summer (may-sept) (5-9)
summer = c('05','06','07','08','09')
summr <- subset(mon_clim, month %in% summer)

sum_sum <- summr %>%
  group_by(year)%>%
  summarize(sumair_t = mean(airt, na.rm = TRUE),
            sumair_tmin = mean(airtmin, na.rm = TRUE),
            sumair_tmax = mean(airtmax, na.rm = TRUE),
            sumprecip = mean(prec, na.rm = TRUE))


# early summer (may/june)
early_sum = c('05','06')
early_sm <- subset(mon_clim, month %in% early_sum)

early_sum_sum <- early_sm %>%
  group_by(year)%>%
  summarize(sprair_t = mean(airt, na.rm = TRUE),
            sprair_tmin = mean(airtmin, na.rm = TRUE),
            sprair_tmax = mean(airtmax, na.rm = TRUE),
            sprprecip = mean(prec, na.rm = TRUE))

# add spring and summer climate variables to dataframe
#red_sum$year <- red_sum$year.x
# make a clean grow ro
grow_ro <- filter(growth, species == 'ro')
cutplots = c('X1','X2','X3','X4','X5','X6')
grow_ro <- filter(grow_ro, !plot.x %in% cutplots)

# remove unnecessary columns from grow ro
grow_ro <- subset(grow_ro, select = -c(dbh.x,year.x,kgc.x,plot.x,basal_area.x))
# rename .y columns
grow_ro <- grow_ro %>% 
  rename("dbh" = "dbh.y",
           'kgc'='kgc.y',
           'year' = 'year.y',
           'plot' = 'plot.y',
           'basal_area' = 'basal_area.y')

grow_ro$prev_year <- grow_ro$year-1

# merge with red oak data based on year
red_clim1 <- left_join(grow_ro,clim, by = 'prev_year')
# rename climate columns
red_clim1 <- red_clim1 %>% 
  rename('prec_prev' = 'prec',
         'year' = 'year.x')


# drop other clim columns for now
red_clim1 <- red_clim1 %>%
  subset(select = -c(year.y,airt, airtmax,airtmin,airtmmx,airtmmn))

# add other climate variables 
red_clim2 <- left_join(red_clim1,clim, by = 'year')
red_clim3 <- left_join(red_clim2,sum_sum, by = 'year')
red_clim <- left_join(red_clim3,early_sum_sum, by = 'year')


# summarize by year and soil type
#red_sum <- grow_ro %>%
#  group_by(year.x, TYPE_) %>%
#  summarize(dbh = mean(dbh_gr, na.rm = TRUE),
#            kgc = mean(kgc_gr, na.rm = TRUE),
#            basal = mean(basal_gr, na.rm = TRUE),
#            n_trees = n())

# pearsons correlation
# make a df by year
red_clim_an <- red_clim %>%
  group_by(year, prec) %>%
  summarize(basal = sum(basal_gr, na.rm = TRUE))
red_clim_an <- red_clim_an %>% 
  drop_na(year)
correlation <- cor(red_clim_an$prec, red_clim_an$basal, method = 'pearson')

# make a df by year and soil
red_clim_soil <- red_clim %>%
  group_by(year, prec, TYPE_) %>%
  summarize(basal = sum(basal_gr, na.rm = TRUE))
red_clim_soil <- red_clim_soil %>% 
  drop_na(year)
correlation <- cor(red_clim_soil$prec, red_clim_soil$basal, method = 'pearson')


# plot
ggplot()+
  geom_boxplot(data = red_clim, aes(group = TYPE_, y = basal_gr))+
  facet_wrap(~year)

# soils
ggplot()+
  geom_boxplot(data = red_clim, aes(x = year, y = basal_gr, group = year, fill = TYPE_))+
  facet_wrap(~TYPE_)

# combined
coeff <- 10000
ggplot() +
  geom_boxplot(data = red_clim, aes(x = year, y = basal_gr, group = year, fill = TYPE_)) + 
  geom_line(data = red_clim, aes(x = year, y = prec/coeff), size = 2) +
  geom_point(data = red_clim, aes(x = year, y = prec/coeff))+
  scale_y_continuous(
    limits = c(0,0.4),
    # Features of the first axis
    name = "Basal Growth",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Precipitation")
  )+ 
  facet_wrap(~TYPE_)



ggplot() +
  geom_boxplot(data = red_clim, aes(x = year.x, y = basal_gr, group = year.x)) + 
  geom_line(data = red_clim, aes(x = year.x, y = prec/coeff), size = 2) +
  geom_point(data = red_clim, aes(x = year.x, y = prec/coeff))+

coeff <- 10000
ggplot() +
  geom_boxplot(data = red_clim, aes(x = year, y = basal_gr, group = year)) + 
  geom_line(data = red_clim, aes(x = year, y = prec/coeff), size = 2) +
  geom_point(data = red_clim, aes(x = year, y = prec/coeff))+
  scale_y_continuous(
    limits = c(0,0.4),
    # Features of the first axis
    name = "Basal Growth",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Precipitation")
  )


ggplot() +
  geom_boxplot(data = red_clim, aes(x = year, y = basal_gr, group = year)) + 
  geom_line(data = red_clim, aes(x = year, y = prec_prev/coeff), size = 2) +
  geom_point(data = red_clim, aes(x = year, y = prec_prev/coeff))+
  scale_y_continuous(
    limits = c(0,0.4),
    # Features of the first axis
    name = "Basal Growth",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Prev Yr Precipitation")
  )

ggplot() +
  geom_boxplot(data = red_clim, aes(x = year, y = basal_gr, group = year, fill = TYPE_)) + 
  geom_line(data = red_clim, aes(x = year, y = prec_prev/coeff), size = 2) +
  geom_point(data = red_clim, aes(x = year, y = prec_prev/coeff))+
  scale_y_continuous(
    limits = c(0,0.4),
    # Features of the first axis
    name = "Basal Growth",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Previous Yr Precipitation")
  )+ 
  facet_wrap(~TYPE_)

coeff <- 1000
ggplot() +
  geom_boxplot(data = red_clim, aes(x = year, y = basal_gr, group = year)) + 
  geom_line(data = red_clim, aes(x = year, y = sumprecip/coeff), size = 2) +
  geom_point(data = red_clim, aes(x = year, y = sumprecip/coeff))+
  scale_y_continuous(
    limits = c(0,0.4),
    # Features of the first axis
    name = "Basal Growth",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Summer Precipitation")
  )

coeff <- 1000
ggplot() +
  geom_boxplot(data = red_clim, aes(x = year, y = basal_gr, group = year)) + 
  geom_line(data = red_clim, aes(x = year, y = sprprecip/coeff), size = 2) +
  geom_point(data = red_clim, aes(x = year, y = sprprecip/coeff))+
  scale_y_continuous(
    limits = c(0,0.4),
    # Features of the first axis
    name = "Basal Growth",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Spring Precipitation")
  )


coeff=20
ggplot()+
  geom_point(data = red_clim, aes(x = year, y = airt, group = year))
            
ggplot() +
  geom_boxplot(data = red_clim, aes(x = year, y = basal_gr, group = year)) + 
  geom_line(data = red_clim, aes(x = year, y = airt/coeff), size = 2) +
  geom_point(data = red_clim, aes(x = year, y = airt/coeff))+
  scale_y_continuous(
    limits = c(0,0.4),
    # Features of the first axis
    name = "Basal Growth",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="")
  )


# linear regressions with climate and soils----
prec_lm <- lm(basal_gr~prec, red_clim)
summary(prec_lm)

prec_lm <- lm(basal_gr~prec+TYPE_, red_clim)
summary(prec_lm)

prev_lm <- lm(basal_gr~prec_prev, red_clim)
summary(prev_lm)

prev_lm <- lm(basal_gr~prec_prev+TYPE_, red_clim)
summary(prev_lm)

sum_lm <- lm(basal_gr~sumprecip, red_clim)
summary(sum_lm)

sum_lm <- lm(basal_gr~sumprecip+TYPE_, red_clim)
summary(sum_lm)

spr_lm <- lm(basal_gr~sprprecip, red_clim)
summary(spr_lm)

spr_lm <- lm(basal_gr~sprprecip+TYPE_, red_clim)
summary(spr_lm)

prec_lm <- lm(basal_gr~prec+TYPE_, red_clim)
summary(prec_lm)

prev_lm <- lm(basal_gr~prec_prev+TYPE_, red_clim)
summary(prev_lm)

sum_lm <- lm(basal_gr~sumprecip+TYPE_, red_clim)
summary(sum_lm)

spr_lm <- lm(basal_gr~sprprecip+TYPE_, red_clim)
summary(spr_lm)



>>>>>>> Stashed changes
#--------------------------------#
#--------------------------------#
#-Look at specific species----------
#--------heatmaps---------
#--------------------------------#
#--------------------------------#
# now do same for hemlock and red oak
# get rid of X plots
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
  old <- subset(old, select = c('plottag', 'dbh','year','plot','elevation','slope','aspect','kgc','basal_area'))
  new <- subset(new, select = c('plottag', 'dbh','year','plot','kgc','basal_area'))
  full <- left_join(old, new, by = "plottag")
  # calculate 
  temp <- full %>%
    group_by(year.x, plot.x, elevation, slope, aspect) %>%
    summarize(dbh_gr = sum(dbh.y-dbh.x, na.rm = TRUE)/n(),
              basal_gr = sum(basal_area.y-basal_area.x, na.rm=TRUE)/n(),
              kgc_gr = sum(kgc.y-kgc.x, na.rm=TRUE)/n()) %>%
    mutate(pl_row = str_sub(plot.x,1,1),
           pl_col = as.numeric(str_sub(plot.x,2,2)))
  growth_hem <- rbind(growth_hem,temp)
}

# remove cut plots

ggplot() + 
  geom_tile(growth_hem, mapping = aes(x=pl_row, y=pl_col, fill= dbh_gr)) +
  facet_wrap(~year.x)+
  scale_fill_gradient(low="chartreuse", high = 'black')+
  theme_minimal()

summary(lm(growth_rate~elevation, data = growth_hem))
summary(lm(growth_rate~slope, data = growth_hem))
summary(lm(growth_rate~aspect, data = growth_hem))

# there does seem to be a relationship between topography and 
# hemlock growth rates
# oddly higher slope and elevation and lower aspect is better

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
  old <- subset(old, select = c('plottag', 'dbh','year','plot','elevation','slope','aspect','kgc','basal_area'))
  new <- subset(new, select = c('plottag', 'dbh','year','plot','kgc','basal_area'))
  full <- left_join(old, new, by = "plottag")
  # calculate 
  temp <- full %>%
    group_by(year.x, plot.x, elevation, slope, aspect) %>%
    summarize(dbh_gr = sum(dbh.y-dbh.x, na.rm = TRUE)/n(),
              basal_gr = sum(basal_area.y-basal_area.x, na.rm=TRUE)/n(),
              kgc_gr = sum(kgc.y-kgc.x, na.rm=TRUE)/n()) %>%
    mutate(pl_row = str_sub(plot.x,1,1),
           pl_col = as.numeric(str_sub(plot.x,2,2)))
  growth_ro <- rbind(growth_ro,temp)
}

ggplot() + 
  geom_tile(growth_ro, mapping = aes(x=pl_row, y=pl_col, fill= growth_rate)) +
  facet_wrap(~year.x)+
  scale_fill_gradient(low="firebrick1", high = 'black')


summary(lm(growth_rate~elevation, data = growth_ro))
summary(lm(growth_rate~slope, data = growth_ro))
summary(lm(growth_rate~aspect, data = growth_ro))


summary(lm(dbh_gr~elevation, data = growth_ro))
summary(lm(dbh_gr~slope, data = growth_ro))
summary(lm(dbh_gr~aspect, data = growth_ro))


summary(lm(basal_gr~elevation, data = growth_ro))
summary(lm(basal_gr~slope, data = growth_ro))
summary(lm(basal_gr~aspect, data = growth_ro))


summary(lm(kgc_gr~elevation, data = growth_ro))
summary(lm(kgc_gr~slope, data = growth_ro))
summary(lm(kgc_gr~aspect, data = growth_ro))

# Lower elevations and aspects are better,

