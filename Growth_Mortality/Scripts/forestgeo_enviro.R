library(tidyverse)
library(here)
library(dplyr)
library(sf)
library(raster)
library(terra)
library(rgdal)

# functions----------
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


# load in environmental data------
soils <- readOGR('GitHub/Dendrometer/enviro_data', layer = 'soils')
elev <- raster('GitHub/Dendrometer/enviro_data/dblbnd.adf')
# calculate slope and aspect from elevation data
slopeaspect <- terrain(elev, opt = c('slope', 'aspect'), unit='degrees')

# load in tree data and coordinates
treedat14 <- read_csv("GitHub/Dendrometer/forestgeo_data/hf253-04-stems-2014.csv")
treedat19 <- read_csv("GitHub/Dendrometer/forestgeo_data/hf253-05-stems-2019.csv")

# Coordinates of the corner of the Megaplot for mapping
mgeast = 731592.9 # Coordinate easting
mgnorth = 4713221.9  # Coordinate northing

treedat14 <- treedat14 %>%
  mutate(quadrat = stringr::str_pad(quadrat, width=4, pad="0", side="left"),
         coords = paste(gx,gy),
         gx_utm = mgeast+gx,
         gy_utm = mgnorth+gy,
         basal_area = dbh^2 * 0.005454) %>%
  filter(df.status == 'alive', sp != "deadhw", sp != "deadsw", sp != "betuxx", sp != "uniden") 

treedat19 <- treedat19 %>%
  mutate(quadrat = stringr::str_pad(quadrat, width=4, pad="0", side="left"),
         coords = paste(gx,gy),
         gx_utm = mgeast+gx,
         gy_utm = mgnorth+gy,
         basal_area = dbh^2 * 0.005454) %>%
  filter(df.status == 'alive',sp != "deadhw", sp != "deadsw", sp != "betuxx", sp != "uniden") 

treedat19$spp <- treedat19$sp
treedat14$spp <- treedat14$sp
treedat19$kgc = calc_kgc(treedat19)
treedat14$kgc = calc_kgc(treedat14)

# narrow down 2019 census columns
treedat192 <- subset(treedat19, select = c('tree.id','stem.id','dbh','basal_area','kgc'))

# merge datasets based on tree and stem id
treedat <- inner_join(treedat14, treedat192, by = c('tree.id','stem.id'))

# make a shape file from coords
coord <- treedat[,c('gx_utm',"gy_utm")]
crs1 <- CRS("+proj=utm +zone=18 +datum=WGS84")
dat <- SpatialPointsDataFrame(coords = coord,
                              data = treedat,
                              proj4string = crs1)
plot(dat)

# reproject to match elevation CRS
newcrs <- crs(elev)
dat2 <- spTransform(dat,newcrs)

# Plot to check overlap
plot(elev)
plot(dat2, add = TRUE)

# Extract topographic data for every EMS plot
dat23 <- treedat %>% mutate(elevation = raster::extract(elev, dat2),
                              slope = raster::extract(slopeaspect$slope, dat2),
                              aspect = raster::extract(slopeaspect$aspect, dat2))

#----------------------------------------------------#
#----------------------------------------------------#
# Calculate growth rates----
#----------------------------------------------------#
#----------------------------------------------------#
dat23 <- dat23 %>% mutate(dbh_gr = dbh.y-dbh.x,
                          basal_gr = basal_area.y-basal_area.x,
                          kgc_gr = kgc.y-kgc.x)

#--------------------------------#
#--------------------------------#
#-Look for relationships between growth and enviro----------
#--------------------------------#
#--------------------------------#
summary(lm(dbh_gr~elevation, data = dat23))
summary(lm(dbh_gr~slope, data = dat23))
summary(lm(dbh_gr~aspect, data = dat23))


summary(lm(basal_gr~elevation, data = dat23))
summary(lm(basal_gr~slope, data = dat23))
summary(lm(basal_gr~aspect, data = dat23))


summary(lm(kgc_gr~elevation, data = dat23))
summary(lm(kgc_gr~slope, data = dat23))
summary(lm(kgc_gr~aspect, data = dat23))


oaks <- filter(dat23, sp == 'querru')
summary(lm(dbh_gr~elevation, data = oaks))
summary(lm(dbh_gr~slope, data = oaks))
summary(lm(dbh_gr~aspect, data = oaks))


summary(lm(basal_gr~elevation, data = oaks))
summary(lm(basal_gr~slope, data = oaks))
summary(lm(basal_gr~aspect, data = oaks))


summary(lm(kgc_gr~elevation, data = oaks))
summary(lm(kgc_gr~slope, data = oaks))
summary(lm(kgc_gr~aspect, data = oaks))

# there seems to be no relationship looking at
# all trees


#--------------------------------#
#--------------------------------#
#-Look at specific species----------
#--------------------------------#
#--------------------------------#
# now do same for hemlock and red oak
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
              basal_gr = sum(basal_area.x-basal_area.y, na.rm=TRUE)/n(),
              kgc_gr = sum(kgc.x-kgc.y, na.rm=TRUE)/n()) %>%
    mutate(pl_row = str_sub(plot.x,1,1),
           pl_col = as.numeric(str_sub(plot.x,2,2)))
  growth_hem <- rbind(growth_hem,temp)
}

ggplot() + 
  geom_tile(growth_hem, mapping = aes(x=pl_row, y=pl_col, fill= growth_rate)) +
  facet_wrap(~year.x)+
  scale_fill_gradient(low="chartreuse", high = 'black')

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
  old <- subset(old, select = c('plottag', 'dbh','year','plot','elevation','slope','aspect'))
  new <- subset(new, select = c('plottag', 'dbh','year','plot'))
  full <- left_join(old, new, by = "plottag")
  # calculate 
  temp <- full %>%
    group_by(year.x, plot.x, elevation, slope, aspect) %>%
    summarize(growth_rate = sum(dbh.y-dbh.x, na.rm = TRUE)/n()) %>%
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

# Lower elevations and aspects are better,

