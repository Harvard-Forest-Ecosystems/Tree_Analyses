# Quick HF megaplot code for mapping tree species 
# Map is kind of hard to interpret now, will work on adding
# some landmark features to understand where things are.

# Load the tidyverse library
# the first time you do this you'll need to run: 
# install.packages("tidyverse") 
library(tidyverse)
library(here)
library(ggplot2)

#KgC
"calc_kgc" = function(df)
{
  # 1/06 kmk
  # revised 5/07
  # a subroutine for calculating kgC of individual trees
  # using their dbhs & spp specific allometries
  # x.name is the name of the object the fnctn is being applied to in ""
  # spp is the column number that contains the spp names
  # dbh is the column number(s) that contain the dbhs in cm
  # allometries calculate biomass (kg) from diameters (cm)
  # many of the equations deal with units other than kg & cm, so conversions are done within the equations
  # "if" statements allow there to be no members of a given spp
  kgc <- df$dbh
  
  #Fraxinus americana (White ash) 	
  ash <- which(df$spp == "ash")
  if(sum(ash) > 0)
    kgc[ash] <- 3.2031 - 0.2337 * (df$dbh[ash]*10) + 0.006061 * (df$dbh[ash]*10)^2
  
  #Betula lenta (Black birch) 
  bb <- which(df$spp == "bb")
  if(sum(bb) > 0)
    kgc[bb] <- (1.6542 * (df$dbh[bb]/2.54)^2.6606)/2.2	
  
  #Fagus grandifolia (American beech)     
  beech <- which(df$spp == "beech")
  if(sum(beech) > 0)
    kgc[beech] <- (10^(2.028 + 2.3981 * log10(df$dbh[beech])) + 10^(1.4182 + 2.5509 * log10(df$dbh[beech])))/1000
  
  #Quercus velutina (Black oak) 
  bo <- which(df$spp == "bo")
  if(sum(bo) > 0)
    kgc[bo] <- (2.14567 * (df$dbh[bo]/2.54)^2.50304)/2.2 
  
  #Prunus serotina  (Black cherry)                
  cherry <- which(df$spp == "cherry")
  if(sum(cherry) > 0)
    kgc[cherry] <- (1.8082 * (df$dbh[cherry]/2.54)^2.6174)/2.2 
  
  #Betula populifolia (Gray birch)                   
  gb <- which(df$spp == "gb")
  if(sum(gb) > 0)
    kgc[gb] <- (exp(0.6541776 + 2.529903 * log(df$dbh[gb]/2.54)) + exp(-0.3969096 + 1.628732 * log(df$dbh[gb]/2.54)))/2.2
  
  #Tsuga canadensis (Eastern hemlock) 
  hem <- which(df$spp == "hem")
  if(sum(hem) > 0)
    kgc[hem] <- (exp(0.2607714 + 2.366161 * log(df$dbh[hem]/2.54)) + exp(-1.775095 + 2.703338 * log(df$dbh[hem]/2.54)))/2.2
  
  #Acer rubrum (Red maple) 
  rm <- which(df$spp == "rm")
  if(sum(rm) > 0)
    kgc[rm] <- (exp(5.02 + 2.33 * log(df$dbh[rm])))/1000
  
  #Quercus rubra (Northern red oak)  & Castenea dentate (American chestnut)
  
  ro <- which(df$spp == "ro" | df$spp == "chestnut")
  if(sum(ro) > 0)
    kgc[ro] <- (exp(5.03 + 2.39 * log(df$dbh[ro])))/1000
  
  #Pinus resinosa (Red pine) 
  rp <- which(df$spp == "rp")
  if(sum(rp) > 0)
    kgc[rp] <- (exp(0.3078881 + 2.448125 * log(df$dbh[rp]/2.54)) + exp(-1.508616 + 2.501085 * log(df$dbh[rp]/2.54)))/2.2
  
  #Acer pennsylvanicum (Striped maple) 
  sm <- which(df$spp == "sm")
  if(sum(sm) > 0)
    kgc[sm] <- (exp(4.68 + 2.38 * log(df$dbh[sm])))/1000
  
  #Betula papyrifera (Paper birch) 
  wb <- which(df$spp == "wb")
  if(sum(wb) > 0)
    kgc[wb] <- (exp(-0.1463825 + 2.896804 * log(df$dbh[wb]/2.54)) + exp(-1.053626 + 2.249402 * log(df$dbh[wb]/2.54)))/2.2
  
  #Hamamelis virginiania (witch hazel)
  wh <- which(df$spp == "wh")
  if(sum(wh) > 0)
    kgc[wh] <- (38.111 * (df$dbh[wh]^2.9))/1000
  
  #Quercus alba (White oak)
  wo <- which(df$spp == "wo")
  if(sum(wo) > 0)
    kgc[wo] <- exp(-0.82061 + 2.84694 * log(df$dbh[wo]/2.54))
  
  #Pinus strobus (white pine) 
  wp <- which(df$spp == "wp")
  if(sum(wp) > 0)
    kgc[wp] <- (exp(-0.04659059 + 2.545864 * log(df$dbh[wp]/2.54)) + exp(-1.701115 + 2.485762 * log(df$dbh[wp]/2.54)))/2.2
  
  #Picea glauca (white spruce) 
  #general spruce equation for all df$spp.
  ws <- which(df$spp == "ws")
  if(sum(ws) > 0)
    kgc[ws] <- (exp(0.3960433 + 2.374645 * log(df$dbh[ws]/2.54)) + exp(-1.351318 + 2.338385 * log(df$dbh[ws]/2.54)))/2.2
  
  #Betula alleghaniensis (Yellow birch) 
  yb <- which(df$spp == "yb")
  if(sum(yb) > 0)
    kgc[yb] <- (10^(2.1413 + 2.2683 * log10(df$dbh[yb])) + 10^(1.0535 + 2.7995 * log10(df$dbh[yb])))/1000
  #----------------------------------------------
  # these next equations are for the sapling data
  # they have units cm & g, so we do the conversion to in and lbs 
  # within the equations to be consistent will all the others
  
  #Vaccinium df$spp. (high bush blueberry) & Rhus typhina (Staghorn sumac)
  hbb <- which(df$spp == "hbb" | df$spp == "SS")
  if (sum(hbb) > 0)
    kgc[hbb] <- (95.143 * (df$dbh[hbb])^3.706)/1000
  
  #Viburnum df$spp. (Northern wild raisin)
  nwr <- which(df$spp == "nwr")
  if (sum(nwr) > 0)
    kgc[nwr] <- (29.615 * (df$dbh[nwr])^3.243)/1000
  
  #Spiraea df$spp.
  SPI <- which(df$spp == "SPI")
  if (sum(SPI) > 0)
    kgc[SPI] <- (36.648 * (df$dbh[SPI])^2.579)/1000
  
  #Prunus pennsylvanica (pin cherry)
  FPC <- which(df$spp == "FPC")
  if (sum(FPC) > 0)
    kgc[FPC] <- (49.916 * (df$dbh[FPC])^2.547)/1000
  
  #Crataegus df$spp. (Hawthorn)
  # for hawthorn df$spp use the general equation for mixed hardwood from Jenkins et al.
  # biomass is converted from kg to lbs in the equation
  haw <- which(df$spp == "haw")
  if (sum(haw) > 0)
    kgc[haw] <- exp(-2.48 + 2.4835*logb(df$dbh[haw]))
  #------------------------------------------------
  # convert from biomass to C
  kgc <- kgc/2
  return(kgc)
}

# Mortality Rates----------
# read in 2014 and 2019 censuses
dat_14 <- read_csv("GitHub/Dendrometer/data/forestgeo_data/hf253-04-stems-2014.csv")

dat_19 <- read_csv("GitHub/Dendrometer/Data/forestgeo_data/hf253-05-stems-2019.csv")


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
  filter(sp != "deadhw", sp != "deadsw", sp != "betuxx", sp != "uniden") 

# rename species column to spp
dat_14 <- dat_14 %>%
  rename('spp'='sp')
dat_19 <- dat_19 %>%
  rename('spp'='sp')

# calculate kgc
dat_14$kgc <- calc_kgc(dat_14)

dat_19$kgc <- calc_kgc(dat_19)

# merge datasets based on tree and stem id
dat <- left_join(dat_14, dat_19, by = c('tree.id','stem.id'))

# calculate mortality rate by quadrat
# question: should I include missing and broken trees as dead?
mortality2 <- dat %>%
  group_by(quadrat.x) %>%
  summarize(mort_rate = sum(df.status.y == 'stem dead')/n(),
            quad_x = round(mean(gx_utm.x, na.rm = TRUE), 0),
            quad_y = round(mean(gy_utm.x, na.rm = TRUE), 0)) %>%
  mutate(quad_row = as.numeric(str_sub(quadrat.x,1,2)),
         quad_col = as.numeric(str_sub(quadrat.x,3,4)))

# calculate kgc loss (mort) by quadrat
dead_trees <- filter(dat, df.status.y == 'stem dead')

dead_mort <- dead_trees %>%
  group_by(quadrat.x) %>%
  summarize(mort = sum(kgc.x, na.rm = TRUE),
            n_dead = n())

# combine with full mortality dataset
mortal <- left_join(mortality, dead_mort, by = 'quadrat.x')

# fill in kgc nas with 0
mortal[is.na(mortal)] <- 0

# create a heat map of kgc loss
mortal %>% ggplot() + 
  geom_tile(aes(x=quad_row, y=quad_col, fill= mort))+
  theme_minimal()+
  ylab('Quadrat Column')+
  xlab('Quadrat Row')+
  labs(fill = "Mortality Rate (KgC Loss)")


# create a heat map
mortality %>% ggplot() + 
  geom_tile(aes(x=quad_row, y=quad_col, fill= mort_rate))+
  theme_minimal()+
  ylab('Quadrat Column')+
  xlab('Quadrat Row')+
  labs(fill = "Mortality Rate")

#---------------------------------------------#
#---------------------------------------------#
# look at hemlock and red oak mortality separately
#---------------------------------------------#
#---------------------------------------------#
hem <- filter(dat, spp.x == 'tsugca')
hem_mort <- hem %>%
  group_by(quadrat.x) %>%
  summarize(mort_rate = sum(df.status.y == 'stem dead')/n(),
            quad_x = round(mean(gx_utm.x, na.rm = TRUE), 0),
            quad_y = round(mean(gy_utm.x, na.rm = TRUE), 0)) %>%
  mutate(quad_row = as.numeric(str_sub(quadrat.x,1,2)),
         quad_col = as.numeric(str_sub(quadrat.x,3,4)))

# calculate kgc loss (mort) for hemlock
dead_hems <- filter(hem, df.status.y == 'stem dead')

dead_hem <- dead_hems %>%
  group_by(quadrat.x) %>%
  summarize(mort = sum(kgc.x, na.rm = TRUE),
            n_dead = n())

# combine with full mortality dataset
hem_mortal <- left_join(hem_mort, dead_hem, by = 'quadrat.x')

# fill in kgc nas with 0
hem_mortal[is.na(hem_mortal)] <- 0




# red oaks
ro <- filter(dat, spp.x == 'querru')
ro_mort <- ro %>%
  group_by(quadrat.x) %>%
  summarize(mort_rate = sum(df.status.y == 'stem dead')/n(),
            quad_x = round(mean(gx_utm.x, na.rm = TRUE), 0),
            quad_y = round(mean(gy_utm.x, na.rm = TRUE), 0)) %>%
  mutate(quad_row = as.numeric(str_sub(quadrat.x,1,2)),
         quad_col = as.numeric(str_sub(quadrat.x,3,4)))

# calculate kgc loss (mort) for red oaks
dead_ros <- filter(ro, df.status.y == 'stem dead')

dead_ro <- dead_ros %>%
  group_by(quadrat.x) %>%
  summarize(mort = sum(kgc.x, na.rm = TRUE),
            n_dead = n())

# combine with full mortality dataset
ro_mortal <- left_join(ro_mort, dead_ro, by = 'quadrat.x')

# fill in kgc nas with 0
ro_mortal[is.na(ro_mortal)] <- 0



# make heat map of kgc loss in hemlock
hem_mortal %>% ggplot() + 
  geom_tile(aes(x=quad_row, y=quad_col, fill= mort))+
  scale_fill_gradient(high="chartreuse", low = 'black')+
  theme_minimal()+
  ylab('Quadrat Column')+
  xlab('Quadrat Row')+
  labs(fill = "Mortality Rate (KgC Loss)")

# make heat maps
par(mfcol = c(3,1))
mortality %>% ggplot() + 
  geom_tile(aes(x=quad_row, y=quad_col, fill= mort_rate))
hem_mort %>% ggplot() + 
  geom_tile(aes(x=quad_row, y=quad_col, fill= mort_rate))+
  scale_fill_gradient(low="chartreuse", high = 'black')+
  theme_minimal()+
  ylab('Quadrat Column')+
  xlab('Quadrat Row')+
  labs(fill = "Mortality Rate")


ro_mortal %>% ggplot() + 
  geom_tile(aes(x=quad_row, y=quad_col, fill= mort))+
  scale_fill_gradient(low="firebrick1", high = 'black')+
  theme_minimal()+
  ylab('Quadrat Column')+
  xlab('Quadrat Row')+
  labs(fill = "Mortality Rate (KgC Loss)")

ro_mortal %>% ggplot() + 
  geom_tile(aes(x=quad_row, y=quad_col, fill= mort))+
  scale_fill_gradient(low="black", high = 'firebrick1')+
  theme_minimal()+
  ylab('Quadrat Column')+
  xlab('Quadrat Row')+
  labs(fill = "Mortality Rate (KgC Loss)")


ro_mortal %>% ggplot() + 
  geom_tile(aes(x=quad_row, y=quad_col, fill= mort))+
  scale_fill_gradient(low="firebrick1", high = 'black')+
  theme_minimal()+
  ylab('Quadrat Column')+
  xlab('Quadrat Row')+
  labs(fill = "Mortality Rate")

ro_mort %>% ggplot() + 
  geom_tile(aes(x=quad_row, y=quad_col, fill= mort_rate))+
  scale_fill_gradient(low="firebrick1", high = 'black')+
  theme_minimal()+
  ylab('Quadrat Column')+
  xlab('Quadrat Row')+
  labs(fill = "Mortality Rate")



# Look at EMS plot mortality over from 2014 to 2019
# Load in data
treedat <- read_csv("~/Documents/Harvard_Forest/Dendrometer/fulldata_trees.csv")
coords <- read_csv("~/Documents/GitHub/Dendrometer/EMS_plots_utm.csv")

coords <- coords %>% mutate(plot = Plot)
coords <- subset(coords, select = c('E.1',"N.1", 'plot'))

# Assign quadrat to each EMS plot
# Calculate distance to each center quadrat point and then 
# assign quadrat based on shortest distance
coords$quadrat <- NA
for (row in 1:nrow(coords)){
  min_dis = 9999999
  for (y in 1:nrow(mortality)){
    x_c <- coords$E.1[row]-mortality$quad_x[y]
    y_c <- coords$N.1[row]-mortality$quad_y[y]
    distance = sqrt((x_c)^2 + (y_c)^2)
    if (distance < min_dis){
      quad <- mortality$quadrat.x[y]
      min_dis <- distance
    }
  }
  if(min_dis <=10){
    coords$quadrat[row] <- quad 
  }
}

coords <- na.omit(coords)

# merge coords with tree data
fulldat <- inner_join(treedat, coords, by = c("plot"))

# subset to only include data from 2014 & 2019
dat14 <- filter(fulldat, year == '2014' & tree_type !="dead" & 
                  doy == "145")
dat19 <- filter(fulldat, year == '2019' & doy == '296')

# join dataframes based on plottag
dat1419 <- left_join(dat14, dat19, by = "plottag")

# calculate 
mort_ems <- dat1419 %>%
  group_by(quadrat.x) %>%
  summarize(mort_rate = sum(tree_type.y == 'dead')/n(),
            n = n(),
            dead = sum(tree_type.y == 'dead', na.rm = TRUE)) %>%
  mutate(quad_row = as.numeric(str_sub(quadrat.x,1,2)),
         quad_col = as.numeric(str_sub(quadrat.x,3,4)))

# idea: loop through every year from 2014 to 2019 and find the mortality rate
# for the quadrat and then add all together
years = c('2014','2015','2016','2017','2018','2019')


mort <- data.frame()

for(year in 1:(length(years)-1)){
  current <- years[year]
  future <- years[year+1]
  old <- filter(fulldat, year ==  current & tree_type != "dead")
  new <- filter(fulldat, year == future)
  old_date <- min(old$doy)
  new_date <- min(new$doy)
  old <- filter(old, doy == old_date)
  new <- filter(new, doy == new_date)
  full <- left_join(old, new, by = "plottag")
  # calculate 
  temp <- full %>%
    group_by(quadrat.x, year.x) %>%
    summarize(mort_rate = sum(tree_type.y == 'dead')/n()) %>%
    mutate(quad_row = as.numeric(str_sub(quadrat.x,1,2)),
           quad_col = as.numeric(str_sub(quadrat.x,3,4)))
  mort <- rbind(mort,temp)
}

# sum for each quadrat
mort_quad <- mort %>% 
  group_by(quad_row,quad_col) %>%
  summarize(total_mort = sum(mort_rate, na.rm = TRUE))


ggplot() + 
  geom_tile(mortality, mapping = aes(x=quad_row, y=quad_col, fill= mort_rate), alpha = .9)+
  geom_point(mort_quad, mapping = aes(x=quad_row, y=quad_col, color= total_mort))

ggplot() + 
  geom_tile(mort_quad, mapping = aes(x=quad_row, y=quad_col, fill= total_mort))


# Plot heatmap of difference between EMS plots and ForestGEO mortality rates
diff <- inner_join(mortality, mort_quad, by = c('quad_row','quad_col'))

diff <- diff %>% mutate(mort_diff = mort_rate - total_mort)

ggplot() + 
  geom_tile(diff, mapping = aes(x=quad_row, y=quad_col, fill= mort_diff))

# Growth rates-----------
dat_14 <- read_csv("Documents/GitHub/Dendrometer/forestgeo_data/hf253-04-stems-2014.csv")

dat_19 <- read_csv("Documents/GitHub/Dendrometer/forestgeo_data/hf253-05-stems-2019.csv")


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

# merge datasets based on tree and stem id
dat <- inner_join(dat_14, dat_19, by = c('tree.id','stem.id'))

# calculate mortality rate by quadrat
# question: should I include missing and broken trees as dead?
redoak <- filter(dat, sp.x == 'querru' & dbh.y < 100)
growth_geo <- redoak %>%
  group_by(quadrat.x) %>%
  # calculate average annual growth rate for each quadrat
  # sum the difference between the second and first census dbh
  # divide by 5 (number of years between 2014-2019)
  # divide by number of individuals in the quadrat
  summarize(growth_rate = sum((dbh.y-dbh.x))/5/n(),
            pop = n(),
            quad_x = round(mean(gx_utm.x, na.rm = TRUE), 0),
            quad_y = round(mean(gy_utm.x, na.rm = TRUE), 0)) %>%
  mutate(quad_row = as.numeric(str_sub(quadrat.x,1,2)),
         quad_col = as.numeric(str_sub(quadrat.x,3,4)))

# create a heat map
growth_geo %>% ggplot() + 
  geom_tile(aes(x=quad_row, y=quad_col, fill= growth_rate))+
  scale_fill_gradient(low="firebrick1", high = 'black')

growth_geo %>% ggplot() + 
  geom_tile(aes(x=quad_row, y=quad_col, fill= pop))+
  scale_fill_gradient(low="firebrick1", high = 'black')


# Growth rates using EMS plot data------
# Load in data
treedat <- read_csv("~/Documents/Harvard_Forest/Dendrometer/fulldata_trees.csv")
coords <- read_csv("~/Documents/GitHub/Dendrometer/EMS_plots_utm.csv")

tree2004 <- filter(treedat, year == '2004')

coords <- coords %>% mutate(plot = Plot)
coords <- subset(coords, select = c('E.1',"N.1", 'plot'))

# Assign quadrat to each EMS plot
# Calculate distance to each center quadrat point and then 
# assign quadrat based on shortest distance
coords$quadrat <- NA
for (row in 1:nrow(coords)){
  min_dis = 9999999
  for (y in 1:nrow(mortality)){
    x_c <- coords$E.1[row]-mortality$quad_x[y]
    y_c <- coords$N.1[row]-mortality$quad_y[y]
    distance = sqrt((x_c)^2 + (y_c)^2)
    if (distance < min_dis){
      quad <- mortality$quadrat.x[y]
      min_dis <- distance
    }
  }
  if(min_dis <=10){
    coords$quadrat[row] <- quad 
  }
}

coords <- na.omit(coords)

# merge coords with tree data
fulldat <- left_join(treedat, coords, by = c("plot"))
# filter to only include redoaks
fulldat <- filter(fulldat, species == 'ro')
# idea: loop through every year from 2014 to 2019 and find the growth rate
# for each quadrat
years = c('2014','2015','2016','2017','2018','2019')

growth <- data.frame()

for(year in 1:(length(years)-1)){
  current <- years[year]
  future <- years[year+1]
  old <- filter(fulldat, year ==  current & tree_type != "dead")
  new <- filter(fulldat, year == future)
  old_date <- min(old$doy)
  new_date <- min(new$doy)
  old <- filter(old, doy <= old_date+15)
  new <- filter(new, doy <= new_date+15)
  full <- left_join(old, new, by = "plottag")
  # calculate 
  temp <- full %>%
    group_by(quadrat.x, year.x) %>%
    summarize(growth_rate = sum(dbh.y-dbh.x)/n()) %>%
    mutate(quad_row = as.numeric(str_sub(quadrat.x,1,2)),
           quad_col = as.numeric(str_sub(quadrat.x,3,4)))
  growth <- rbind(growth,temp)
}

# sum for each quadrat
grow_quad <- growth %>% 
  group_by(quad_row,quad_col) %>%
  summarize(total_growth = sum(growth_rate, na.rm = TRUE)/5)

ggplot() + 
  geom_tile(grow_quad, mapping = aes(x=quad_row, y=quad_col, fill= total_growth))


#----------------------------------------------------#
#----------------------------------------------------#
# try creating a heat map using EMS plot numbers so everything is included----
#----------------------------------------------------#
#----------------------------------------------------#

# merge coords with tree data
fulldat <- left_join(treedat, coords, by = c("plot"))

# filter to only include redoaks
fulldat <- filter(fulldat, species == 'ro')
# idea: loop through every year from 2014 to 2019 and find the growth rate
# for each quadrat
temp <- filter(fulldat,year != '1993')
years <- unique(temp$year)

growth <- data.frame()

for(year in 1:(length(years)-1)){
  current <- years[year]
  future <- years[year+1]
  old <- filter(fulldat, year ==  current & tree_type != "dead")
  new <- filter(fulldat, year == future & tree_type != "dead")
  old_date <- min(old$doy, na.rm = TRUE)
  new_date <- min(new$doy, na.rm = TRUE)
  old <- filter(old, doy <= old_date+15)
  new <- filter(new, doy <= new_date+15)
  old <- subset(old, select = c('plottag', 'dbh','year','plot'))
  new <- subset(new, select = c('plottag', 'dbh','year','plot'))
  full <- left_join(old, new, by = "plottag")
  # calculate 
  temp <- full %>%
    group_by(year.x, plot.x) %>%
    summarize(growth_rate = sum(dbh.y-dbh.x)/n()) %>%
    mutate(pl_row = str_sub(plot.x,1,1),
            pl_col = as.numeric(str_sub(plot.x,2,2)))
  growth <- rbind(growth,temp)
}

ggplot() + 
  geom_tile(growth, mapping = aes(x=pl_row, y=pl_col, fill= growth_rate)) +
  facet_wrap(~year.x)

                       


#----------------------------------------------------------#
#----------------------------------------------------------#
#----Accurately grab trees according to EMS plot location-------
#----------------------------------------------------------#
#----------------------------------------------------------#
# load in archive data and coordinate data for the EMS plots
treedat <- read_csv("~/Documents/Harvard_Forest/Dendrometer/fulldata_trees.csv")
coords <- read_csv("~/Documents/GitHub/Dendrometer/EMS_plots_utm.csv")

# combine coordinate data with the EMS plot data
coords <- coords %>% mutate(plot = Plot)
coords <- subset(coords, select = c('E.1',"N.1", 'plot'))
fulldat <- left_join(treedat, coords, by = c("plot"))

# load in the ForestGEO data
dat_14 <- read_csv("Documents/GitHub/Dendrometer/forestgeo_data/hf253-04-stems-2014.csv")
dat_19 <- read_csv("Documents/GitHub/Dendrometer/forestgeo_data/hf253-05-stems-2019.csv")

# Add coordinates to ForestGEO trees
mgeast = 731592.9 # Coordinate easting
mgnorth = 4713221.9  # Coordinate northing

# Create quadrat and coordinate columns
dat_14 <- dat_14 %>%
  mutate(quadrat = stringr::str_pad(quadrat, width=4, pad="0", side="left"),
         coords = paste(gx,gy),
         gx_utm = mgeast+gx,
         gy_utm = mgnorth+gy) %>%
  filter(df.status == 'alive', sp != "deadhw", sp != "deadsw", sp != "betuxx", sp != "uniden") 

dat_14 <- filter(dat_14, dbh >=10)

dat_19 <- dat_19 %>%
  mutate(quadrat = stringr::str_pad(quadrat, width=4, pad="0", side="left"),
         coords = paste(gx,gy),
         gx_utm = mgeast+gx,
         gy_utm = mgnorth+gy) %>%
  filter(df.status == 'alive',sp != "deadhw", sp != "deadsw", sp != "betuxx", sp != "uniden") 

dat_19 <- filter(dat_19, dbh >=10)

#------------------------------------#
#---------Do same for 2019!-----------
#------------------------------------#
forest_ems19 <- data.frame()
# Loop through every plot coordinate
for (i in 1:nrow(coords)){ 
  temp <- NA
  # grab coordinates of interest
  midx <- coords$E.1[i]
  midy <- coords$N.1[i]
  # define radius
  radius = 10
  # subset to only trees that are relatively close to the coordinate
  # makes loop run faster
  temp<-dat_19[dat_19$gx_utm>=(midx-radius) & dat_19$gx_utm<=(midx+radius) & dat_19$gy_utm>=(midy-radius) & dat_19$gy_utm<=(midy+radius), ]
  temp$dis<-rep(NA, nrow(temp))
  # loop through every tree and calculate its actual distance from the center point
  for(j in 1:nrow(temp))
  { 
    temp$dis[j]<-sqrt((midx-temp$gx_utm[j])^2+(midy-temp$gy_utm[j])^2)
  }
  # remove trees outside of the radius
  temp <- subset(temp, dis <= radius)
  temp$plot <- coords$plot[i]
  forest_ems19 <- rbind(forest_ems19, temp)
}

# check to see how many trees are in each plot
ems_geo <- forest_ems19 %>%
  group_by(plot) %>%
  summarize(num = unique(n()))

# check to see how many EMS plots say there were in 2019
treedat19 <- filter(treedat, year == '2019' & doy == '296')
ems_sum <- treedat19 %>%
  group_by(plot) %>%
  summarize(num = unique(n()))

# merge and calc difference
# num.x = what we actually measured
# num.y = forestgeo number
checks <- inner_join(ems_sum, ems_geo, by = 'plot')




forest_ems14 <- data.frame()

# Loop through every plot coordinate
for (i in 1:nrow(coords)){ 
  temp <- NA
  # grab coordinates of interest
  midx <- coords$E.1[i]
  midy <- coords$N.1[i]
  # define radius
  radius = 10
  # subset to only trees that are relatively close to the coordinate
  # makes loop run faster
  temp<-dat_14[dat_14$gx_utm>=(midx-radius) & dat_14$gx_utm<=(midx+radius) & dat_14$gy_utm>=(midy-radius) & dat_14$gy_utm<=(midy+radius), ]
  temp$dis<-rep(NA, nrow(temp))
  # loop through every tree and calculate its actual distance from the center point
  for(j in 1:nrow(temp))
  { 
    temp$dis[j]<-sqrt((midx-temp$gx_utm[j])^2+(midy-temp$gy_utm[j])^2)
  }
  # remove trees outside of the radius
  temp <- subset(temp, dis <= radius)
  temp$plot <- coords$plot[i]
  forest_ems14 <- rbind(forest_ems14, temp)
}

# check to see how many trees are in each plot
ems_geo <- forest_ems14 %>%
  group_by(plot) %>%
  summarize(num = unique(n()))

# check to see how many EMS plots say there were in 2014
treedat14 <- filter(treedat, year == '2014' & doy == '337')
ems_sum <- treedat14 %>%
  group_by(plot) %>%
  summarize(num = unique(n()))





# Now merge with 2019 data
dat <- left_join(forest_ems, dat_19, by = c('tree.id','stem.id'))

# Calculate growth rates for EMS plots using ForestGEO data
growth_geo <- dat %>%
  group_by(plot) %>%
  # calculate average annual growth rate for each quadrat
  # sum the difference between the second and first census dbh
  # divide by 5 (number of years between 2014-2019)
  # divide by number of individuals in the quadrat
  summarize(growth_rate = sum((dbh.y-dbh.x), na.rm = TRUE)/5/n(),
            pop = n()) %>%
  mutate(pl_row = str_sub(plot,1,1),
         pl_col = as.numeric(str_sub(plot,2,2)))

# Calculate growth rates for EMS plots from 2014-2019
years = c('2014','2015','2016','2017','2018','2019')

growth <- data.frame()

for(year in 1:(length(years)-1)){
  current <- years[year]
  future <- years[year+1]
  old <- filter(fulldat, year ==  current & tree_type != "dead")
  new <- filter(fulldat, year == future & tree_type != "dead")
  old_date <- min(old$doy)
  new_date <- min(new$doy)
  old <- filter(old, doy <= old_date+15)
  new <- filter(new, doy <= new_date+15)
  full <- left_join(old, new, by = "plottag")
  # calculate 
  temp <- full %>%
    group_by(plot.x, year.x) %>%
    summarize(growth_rate_ems = sum(dbh.y-dbh.x, na.rm = TRUE)/n()) %>%
    mutate(pl_row = str_sub(plot.x,1,1),
           pl_col = as.numeric(str_sub(plot.x,2,2)))
  growth <- rbind(growth,temp)
}

# merge with ForestGEO growth rate dataframe
growth$plot <- growth$plot.x
all_growth <- inner_join(growth, growth_geo, by='plot')

# plot side by side
ggplot() + 
  geom_tile(growth, mapping = aes(x=pl_row, y=pl_col, fill= growth_rate)) +
  facet_wrap(~year.x)


